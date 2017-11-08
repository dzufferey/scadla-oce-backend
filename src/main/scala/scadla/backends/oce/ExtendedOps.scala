package scadla.backends.oce

import scadla._
import squants.space.Length
import squants.space.Angle
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

  }

}
