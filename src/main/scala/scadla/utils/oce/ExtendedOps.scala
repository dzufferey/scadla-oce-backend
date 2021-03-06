package scadla.utils.oce

import scadla._
import squants.space.{Length, Angle, Area, Volume, VolumeUnit}
import squants.space.{Millimeters, Microlitres}
import org.jcae.opencascade.jni._

object ExtendedOps {

  implicit final class ShapeOps(private val lhs: TopoDS_Shape) extends AnyVal {
    def vertices = TopoExplorerUnique.vertices(lhs)
    def allVertices = TopoExplorer.vertices(lhs)
    def edges = TopoExplorerUnique.edges(lhs)
    def allEdges = TopoExplorer.edges(lhs)
    def wires = TopoExplorerUnique.wires(lhs)
    def allWires = TopoExplorer.wires(lhs)
    def faces = TopoExplorerUnique.faces(lhs)
    def allFaces = TopoExplorer.faces(lhs)
    def shells = TopoExplorerUnique.shells(lhs)
    def allShells = TopoExplorer.shells(lhs)
    def solids = TopoExplorerUnique.solids(lhs)
    def allSolids = TopoExplorer.solids(lhs)

    def isValid = lhs == null || new BRepCheck_Analyzer(lhs).isValid()
  }

  implicit final class VertexOps(private val lhs: TopoDS_Vertex) extends AnyVal {

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

  implicit final class EdgeOps(private val lhs: TopoDS_Edge) extends AnyVal {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_Wire] = {
      TopoExplorerUnique.wires(shape).filter(wire => {
        TopoExplorerUnique.edges(wire).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Vertex] = {
      TopoExplorerUnique.vertices(lhs)
    }

    def allChildren: Iterator[TopoDS_Vertex] = {
      TopoExplorer.vertices(lhs)
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

    def start = allChildren.next()

    def end = {
      val it = allChildren
      it.next()
      it.next()
    }

    def extremities = {
      val it = allChildren
      val s = it.next()
      val e = it.next()
      (s,e)
    }

    def point(u: Double) = {
      val adaptor = new BRepAdaptor_Curve(lhs)
      val start = adaptor.firstParameter
      val end = adaptor.lastParameter
      val pnt = adaptor.value(start + (end - start) * u)
      Point(Millimeters(pnt(0)), Millimeters(pnt(1)), Millimeters(pnt(2)))
    }

    def tangent(u: Double = 0.5) = {
      var vec = Array.ofDim[Double](3)
      var pnt = Array.ofDim[Double](3)
      val adaptor = new BRepAdaptor_Curve(lhs)
      val start = adaptor.firstParameter
      val end = adaptor.lastParameter
      adaptor.d1(start + (end - start) * u, pnt, vec)
      Vector(vec(0), vec(1), vec(2), Millimeters)
    }

    def adjacentFacesIn(shape: TopoDS_Shape): Iterator[TopoDS_Face] = {
      TopoExplorerUnique.faces(shape).filter(f => {
        TopoExplorerUnique.edges(f).contains(lhs)
      })
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

  implicit final class WireOps(private val lhs: TopoDS_Wire) extends AnyVal {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_Face] = {
      TopoExplorerUnique.faces(shape).filter(face => {
        TopoExplorerUnique.wires(face).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Edge] = {
      TopoExplorerUnique.edges(lhs)
    }

    def allChildren: Iterator[TopoDS_Edge] = {
      TopoExplorer.edges(lhs)
    }

    def length: Length = {
      val prop = new GProp_GProps()
      BRepGProp.linearProperties(lhs, prop)
      Millimeters(prop.mass)
    }

    // find the loops in the wire
    def subLoops: Iterable[TopoDS_Wire] = {
      //keep the edges and their starting point
      var prefixes: List[(TopoDS_Edge,TopoDS_Vertex)] = Nil
      var loops: List[TopoDS_Wire] = Nil
      for (e <- allChildren) {
        val (start,end) = e.extremities
        prefixes = (e -> start) :: prefixes
        val i = prefixes.indexWhere{ case (_, pts) => end == pts }
        if (i >= 0) {
          val (l, rest) = prefixes.splitAt(i+1)
          val builder = new BRepBuilderAPI_MakeWire()
          for ( (e,_) <- l.reverse ) builder.add(e)
          assert(builder.isDone)
          val w = builder.shape.asInstanceOf[TopoDS_Wire]
          loops = w :: loops
          prefixes = rest
        }
      }
      loops
    }

    def c1Continuous: Boolean = {
      scadla.utils.oce.c1Continuous(allChildren)
    }

    def description = {
      allChildren.map(_.description).mkString("wire\n  ", "\n  ", "\n")
    }

  }

  implicit final class FaceOps(private val lhs: TopoDS_Face) extends AnyVal {

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

    def kind: GeomAbs_SurfaceType = {
      new BRepAdaptor_Surface(lhs).getType
    }

    def isPlane: Boolean = {
      kind == GeomAbs_SurfaceType.GeomAbs_Plane
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
      val vec = Vector(n(0), n(1), n(2), Millimeters)
      if (lhs.orientation == TopAbs_Orientation.FORWARD) {
        vec
      } else {
        assert(lhs.orientation == TopAbs_Orientation.REVERSED)
        vec * -1
      }
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

    def pointOnFace(pnt: Point)(implicit tolerance: Length): Boolean = {
      val p = Array[Double](pnt.x.toMillimeters, pnt.y.toMillimeters, pnt.z.toMillimeters)
      val classifier = new BRepClass_FaceClassifier()
      classifier.perform(lhs, p, tolerance.toMillimeters)
      classifier.state == TopAbs_State.ON
    }

  }

  implicit final class ShellOps(private val lhs: TopoDS_Shell) extends AnyVal {

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

  implicit final class SolidOps(private val lhs: TopoDS_Solid) extends AnyVal {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_CompSolid] = {
      TopoExplorerUnique.compSolids(shape).filter(comp => {
        TopoExplorerUnique.solids(comp).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Wire] = {
      TopoExplorerUnique.wires(lhs)
    }

    def volume(unit: VolumeUnit = Microlitres): Volume = {
      val prop = new GProp_GProps()
      BRepGProp.volumeProperties(lhs, prop)
      unit(prop.mass)
    }

  }

  import scala.language.implicitConversions

  implicit def shapeIterator2Iterable[A <: TopoDS_Shape](it: Iterator[A]): Iterable[A] = it.to(Iterable)

}
