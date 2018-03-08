package scadla.utils.oce

import scadla._
import squants.space.{Length, Angle, Area, Volume}
import squants.space.Millimeters
import org.jcae.opencascade.jni._

object ExtendedOps {

  implicit class VertexOps(lhs: TopoDS_Vertex) {

    def asPoint: Point = {
      val p = BRep_Tool.pnt(lhs)
      Point(Millimeters(p(0)), Millimeters(p(1)), Millimeters(p(2)))
    }

    def x: Length = {
      val p = BRep_Tool.pnt(lhs)
      Millimeters(p(0))
    }

    def y: Length = {
      val p = BRep_Tool.pnt(lhs)
      Millimeters(p(1))
    }
    
    def z: Length = {
      val p = BRep_Tool.pnt(lhs)
      Millimeters(p(2))
    }

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_Edge] = {
      TopoExplorerUnique.edges(shape).filter(edge => {
        TopoExplorerUnique.vertices(edge).contains(lhs)
      })
    }

  }

  implicit class EdgeOps(lhs: TopoDS_Edge) {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_Wire] = {
      TopoExplorerUnique.wires(shape).filter(wire => {
        TopoExplorerUnique.edges(wire).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Vertex] = {
      TopoExplorerUnique.vertices(lhs)
    }

    def length: Length = {
      val prop = new GProp_GProps()
      BRepGProp.linearProperties(lhs, prop)
      Millimeters(prop.mass)
    }

    def isDegenerate: Boolean = {
      BRep_Tool.degenerated(lhs)
    }

    def isClosed: Boolean = {
      children.length == 1
    }

    def curve: (Geom_Curve, Double, Double) = {
      var range = Array.ofDim[Double](2)
      val c = BRep_Tool.curve(lhs, range) //this leaks a bit of memory according to the file BRep.i in the jCAE repo
      (c, range(0), range(1))
    }

    def kind: GeomAbs_CurveType = {
      new BRepAdaptor_Curve(lhs).getType
    }

    def description = {
      val adaptor = new BRepAdaptor_Curve(lhs)
      val kind = kindToString(adaptor.getType)
      val start = adaptor.firstParameter
      val end = adaptor.lastParameter
      val startP = adaptor.value(start).mkString("(",",",")")
      val endP = adaptor.value(end).mkString("(",",",")")
      "curve: " + kind + "[" + start + "," + end + "], from " + startP + " to " + endP
    }

  }

  implicit class WireOps(lhs: TopoDS_Wire) {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_Face] = {
      TopoExplorerUnique.faces(shape).filter(face => {
        TopoExplorerUnique.wires(face).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Edge] = {
      TopoExplorerUnique.edges(lhs)
    }

    def length: Length = {
      val prop = new GProp_GProps()
      BRepGProp.linearProperties(lhs, prop)
      Millimeters(prop.mass)
    }

    def c1Continuous: Boolean = {
      //Console.println("\n\n==============\n" + lhs.description)

      def checkC1(g: GeomAbs_Shape) = g match {
        case GeomAbs_Shape.C0 => false
        case _ => true
      }

      def joined(c1: BRepAdaptor_Curve, c2: BRepAdaptor_Curve, toleranceP: Double = 1e-7, toleranceD: Double = 1e-1) = {
        val p1 = Array.ofDim[Double](3)
        val d1 = Array.ofDim[Double](3)
        c1.d1(c1.lastParameter, p1, d1)
        val p2 = Array.ofDim[Double](3)
        val d2 = Array.ofDim[Double](3)
        c2.d1(c2.firstParameter, p2, d2)
        //Console.println("p1 " + p1.mkString(", "))
        //Console.println("d1 " + d1.mkString(", "))
        //Console.println("p2 " + p2.mkString(", "))
        //Console.println("d2 " + d2.mkString(", "))
        (toPoint(p1) to toPoint(p2)).norm <= Millimeters(toleranceP) &&
        (toVector(d1).toUnitVector - toVector(d2).toUnitVector).norm <= Millimeters(toleranceD)
      }

      val it = children
      val first = it.next
      //Console.println("first: " + first.description)
      val firstAdaptor = new BRepAdaptor_Curve(first)
      if (first.isDegenerate || !checkC1(firstAdaptor.continuity)) {
        return false
      }

      var current = first
      var currentAdaptor = firstAdaptor
      while (it.hasNext) {
        val previous = current
        val previousAdaptor = currentAdaptor
        current = it.next
        //Console.println("current: " + current.description)
        currentAdaptor = new BRepAdaptor_Curve(current)
        if (current.isDegenerate || !checkC1(currentAdaptor.continuity)) {
          return false
        }
        if (!joined(previousAdaptor, currentAdaptor)) {
          return false
        }
      }
      if (!joined(currentAdaptor, firstAdaptor)) {
        return false
      } else {
        return true
      }
    }

    def description = {
      children.map(_.description).mkString("wire\n  ", "\n  ", "\n")
    }

  }

  implicit class FaceOps(lhs: TopoDS_Face) {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_Shell] = {
      TopoExplorerUnique.shells(shape).filter(s => {
        TopoExplorerUnique.faces(s).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Wire] = {
      TopoExplorerUnique.wires(lhs)
    }

    def area: Area = {
      val prop = new GProp_GProps()
      BRepGProp.surfaceProperties(lhs, prop)
      Millimeters(1) * Millimeters(prop.mass)
    }

    def isPlane: Boolean = {
      val s = BRep_Tool.surface(lhs)
      s.isInstanceOf[Geom_Plane]
    }

    protected def getPropsAt(u: Double, v: Double, degree: Int, tolerance: Length) = {
      val s = BRep_Tool.surface(lhs)
      val props = new GeomLProp_SLProps(degree, tolerance.toMillimeters)
      props.setSurface(s)
      //scale UV
      val bounds = Array.ofDim[Double](4)
      s.bounds(bounds)
      val uMin = bounds(0)
      val uMax = bounds(1)
      val vMin = bounds(2)
      val vMax = bounds(3)
      props.setParameters(uMin + (uMax - uMin) * u, vMin + (vMax - vMin) * v)
      props
    }

    def isUmbilic(implicit tolerance: Length): Boolean = {
      val props = getPropsAt(0.5, 0.5, 2, tolerance)
      props.isUmbilic
    }

    def isCurvatureDefined(u: Double = 0.5, v: Double = 0.5)(implicit tolerance: Length): Boolean = {
      val props = getPropsAt(u, v, 2, tolerance)
      props.isCurvatureDefined
    }

    def meanCurvature(u: Double = 0.5, v: Double = 0.5)(implicit tolerance: Length): Length = {
      val props = getPropsAt(u, v, 2, tolerance)
      Millimeters(props.meanCurvature)
    }

    def gaussianCurvature(u: Double = 0.5, v: Double = 0.5)(implicit tolerance: Length): Length = {
      val props = getPropsAt(u, v, 2, tolerance)
      Millimeters(props.gaussianCurvature)
    }

    def normal(u: Double = 0.5, v: Double = 0.5)(implicit tolerance: Length): Vector = {
      val props = getPropsAt(u, v, 2, tolerance)
      val n = props.normal()
      Vector(n(0), n(1), n(2), Millimeters)
    }

    def normal(pnt: Point)(implicit tolerance: Length): Option[Vector] = {
      val s = BRep_Tool.surface(lhs)
      val p = Array[Double](pnt.x.toMillimeters, pnt.y.toMillimeters, pnt.z.toMillimeters)
      val proj = new GeomAPI_ProjectPointOnSurf(p, s)
      if (proj.lowerDistance < tolerance.toMillimeters) {
        val u = Array[Double](0.0)
        val v = Array[Double](0.0)
        proj.lowerDistanceParameters(u, v);
        Some(normal(u(0), v(0))(tolerance))
      } else {
        None
      }
    }

    def nearestPoint(pnt: Point): Point = {
      val s = BRep_Tool.surface(lhs)
      val p = Array[Double](pnt.x.toMillimeters, pnt.y.toMillimeters, pnt.z.toMillimeters)
      val proj = new GeomAPI_ProjectPointOnSurf(p, s)
      val p2 = proj.nearestPoint
      Point(Millimeters(p2(0)), Millimeters(p2(1)), Millimeters(p2(2)))
    }

  }

  implicit class ShellOps(lhs: TopoDS_Shell) {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_Solid] = {
      TopoExplorerUnique.solids(shape).filter(s => {
        TopoExplorerUnique.shells(s).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Face] = {
      TopoExplorerUnique.faces(lhs)
    }

    def area: Area = {
      val prop = new GProp_GProps()
      BRepGProp.surfaceProperties(lhs, prop)
      Millimeters(1) * Millimeters(prop.mass)
    }

  }

  implicit class SolidOps(lhs: TopoDS_Solid) {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_CompSolid] = {
      TopoExplorerUnique.compSolids(shape).filter(comp => {
        TopoExplorerUnique.solids(comp).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Wire] = {
      TopoExplorerUnique.wires(lhs)
    }

    def volume: Volume = {
      val prop = new GProp_GProps()
      BRepGProp.volumeProperties(lhs, prop)
      Millimeters(1) * Millimeters(1) * Millimeters(prop.mass)
    }

  }

}
