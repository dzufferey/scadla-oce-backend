package scadla

import squants.space.Length
import org.jcae.opencascade.jni._

case class OceShape(shape: TopoDS_Shape) extends Shape {
  assert(shape.shapeType == TopAbs_ShapeEnum.SOLID ||
         shape.shapeType == TopAbs_ShapeEnum.COMPSOLID ||
         shape.shapeType == TopAbs_ShapeEnum.COMPOUND)
}

case class Fillet(s: Solid, select: (TopoDS_Shape, TopoDS_Edge) => Option[Length]) extends Operation(Seq(s)) {
  def setChildren(c: Seq[Solid]) = {
    assert(c.length == 1)
    Fillet(c.head, select)
  }
}

case class Chamfer(s: Solid, select: (TopoDS_Shape, TopoDS_Face, TopoDS_Edge) => Option[Length]) extends Operation(Seq(s)) {
  def setChildren(c: Seq[Solid]) = {
    assert(c.length == 1)
    Chamfer(c.head, select)
  }
}

//TODO offset

object Fillet {
  def apply(s: Solid, select: TopoDS_Edge => Option[Length]): Fillet =
    Fillet(s, (s: TopoDS_Shape, e: TopoDS_Edge) => select(e))
  def apply(s: Solid, radius: Length, select: (TopoDS_Shape, TopoDS_Edge) => Boolean): Fillet =
    Fillet(s, (s: TopoDS_Shape, e: TopoDS_Edge) => if (select(s,e)) Some(radius) else None)
  def apply(s: Solid, radius: Length, select: TopoDS_Edge => Boolean): Fillet =
    Fillet(s, (s: TopoDS_Shape, e: TopoDS_Edge) => if (select(e)) Some(radius) else None)
  //TODO from Wire/Face in a more efficient way
}

object Chamfer {
  def apply(s: Solid, select: (TopoDS_Face, TopoDS_Edge) => Option[Length]): Chamfer =
    Chamfer(s, (s: TopoDS_Shape, f: TopoDS_Face, e: TopoDS_Edge) => select(f,e))
  def apply(s: Solid, distance: Length, select: (TopoDS_Shape, TopoDS_Face, TopoDS_Edge) => Boolean): Chamfer =
    Chamfer(s, (s: TopoDS_Shape, f: TopoDS_Face, e: TopoDS_Edge) => if (select(s,f,e)) Some(distance) else None)
  def apply(s: Solid, distance: Length, select: (TopoDS_Face, TopoDS_Edge) => Boolean): Chamfer =
    Chamfer(s, (s: TopoDS_Shape, f: TopoDS_Face, e: TopoDS_Edge) => if (select(f,e)) Some(distance) else None)
  //TODO from Wire/Face in a more efficient way
}
