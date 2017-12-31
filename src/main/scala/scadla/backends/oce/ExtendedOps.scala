package scadla.backends.oce

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
      TopoExplorer.edges(shape).filter(edge => {
        TopoExplorer.vertices(edge).contains(lhs)
      })
    }

  }

  implicit class EdgeOps(lhs: TopoDS_Edge) {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_Wire] = {
      TopoExplorer.wires(shape).filter(wire => {
        TopoExplorer.edges(wire).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Vertex] = {
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
      val bounds = Array[Double](0, 0)
      val curve = BRep_Tool.curve(lhs, bounds)
      if (curve != null) {
        curve.isClosed()
      } else {
        sys.error("edge is degenerate of curve on surface ...")
      }
    }

  }

  implicit class WireOps(lhs: TopoDS_Wire) {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_Face] = {
      TopoExplorer.faces(shape).filter(face => {
        TopoExplorer.wires(face).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Edge] = {
      TopoExplorer.edges(lhs)
    }

    def length: Length = {
      val prop = new GProp_GProps()
      BRepGProp.linearProperties(lhs, prop)
      Millimeters(prop.mass)
    }

  }

  implicit class FaceOps(lhs: TopoDS_Face) {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_Shell] = {
      TopoExplorer.shells(shape).filter(s => {
        TopoExplorer.faces(s).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Wire] = {
      TopoExplorer.wires(lhs)
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

  }

  implicit class ShellOps(lhs: TopoDS_Shell) {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_Solid] = {
      TopoExplorer.solids(shape).filter(s => {
        TopoExplorer.shells(s).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Face] = {
      TopoExplorer.faces(lhs)
    }

    def area: Area = {
      val prop = new GProp_GProps()
      BRepGProp.surfaceProperties(lhs, prop)
      Millimeters(1) * Millimeters(prop.mass)
    }

  }

  implicit class SolidOps(lhs: TopoDS_Solid) {

    def parentsIn(shape: TopoDS_Shape): Iterator[TopoDS_CompSolid] = {
      TopoExplorer.compSolids(shape).filter(comp => {
        TopoExplorer.solids(comp).contains(lhs)
      })
    }

    def children: Iterator[TopoDS_Wire] = {
      TopoExplorer.wires(lhs)
    }

    def volume: Volume = {
      val prop = new GProp_GProps()
      BRepGProp.volumeProperties(lhs, prop)
      Millimeters(1) * Millimeters(1) * Millimeters(prop.mass)
    }

  }

}
