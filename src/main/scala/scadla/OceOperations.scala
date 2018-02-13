package scadla

import squants.space.Length
import org.jcae.opencascade.jni._

case class OceShape(shape: TopoDS_Shape) extends Shape {
  assert(shape.shapeType == TopAbs_ShapeEnum.SOLID ||
         shape.shapeType == TopAbs_ShapeEnum.COMPSOLID ||
         shape.shapeType == TopAbs_ShapeEnum.COMPOUND)
}

case class Fillet(s: Solid, select: TopoDS_Edge => Option[Length]) extends Operation(Seq(s)) {
  def setChildren(c: Seq[Solid]) = {
    assert(c.length == 1)
    Fillet(c.head, select)
  }
}

//TODO chamfer, offset

object Fillet {
  def apply(s: Solid, radius: Length, select: TopoDS_Edge => Boolean): Fillet = Fillet(s, (e: TopoDS_Edge) => if (select(e)) Some(radius) else None)
}
