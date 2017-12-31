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
