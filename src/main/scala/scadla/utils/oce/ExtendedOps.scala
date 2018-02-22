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
      /*
      val bounds = Array[Double](0, 0)
      val curve = BRep_Tool.curve(lhs, bounds)
      if (curve != null) {
        curve.isClosed()
      } else {
        sys.error("edge is degenerate or we need curve on surface ...")
      }
      */
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
