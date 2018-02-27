package scadla

import squants.space.Length
import org.jcae.opencascade.jni._

case class OceShape(shape: TopoDS_Shape) extends Shape {
  assert(shape.shapeType == TopAbs_ShapeEnum.SOLID ||
         shape.shapeType == TopAbs_ShapeEnum.COMPSOLID ||
         shape.shapeType == TopAbs_ShapeEnum.COMPOUND)
}

case class OceOperation(s: Solid, op: TopoDS_Shape => TopoDS_Shape) extends Operation(Seq(s)) {
  def setChildren(c: Seq[Solid]) = {
    assert(c.length == 1)
    OceOperation(c.head, op)
  }
}

//TODO offset

object Fillet {

  def apply(s: Solid, select: TopoDS_Edge => Option[Length]): OceOperation = {
    OceOperation(s, shape => utils.oce.Fillet(shape, select))
  }
  def apply(s: Solid, radius: Length, select: TopoDS_Edge => Boolean): OceOperation = {
    Fillet(s, (e: TopoDS_Edge) => if (select(e)) Some(radius) else None)
  }

  def wire(s: Solid, select: TopoDS_Wire => Option[Length]): OceOperation = {
    OceOperation(s, shape => utils.oce.Fillet.wire(shape, select))
  }
  def wire(s: Solid, radius: Length, select: TopoDS_Wire => Boolean): OceOperation = {
    Fillet.wire(s, (e: TopoDS_Wire) => if (select(e)) Some(radius) else None)
  }

  def face(s: Solid, select: TopoDS_Face => Option[Length]): OceOperation = {
    OceOperation(s, shape => utils.oce.Fillet.face(shape, select))
  }
  def face(s: Solid, radius: Length, select: TopoDS_Face => Boolean): OceOperation = {
    Fillet.face(s, (e: TopoDS_Face) => if (select(e)) Some(radius) else None)
  }

}

object Chamfer {

  def apply(s: Solid, select: (TopoDS_Face, TopoDS_Edge) => Option[Length]): OceOperation = {
    OceOperation(s, shape => utils.oce.Chamfer(shape, select))
  }
  def apply(s: Solid, l: Length, select: (TopoDS_Face, TopoDS_Edge) => Boolean): OceOperation = {
    Chamfer(s, (f: TopoDS_Face, e: TopoDS_Edge) => if (select(f,e)) Some(l) else None)
  }

  def wire(s: Solid, select: (TopoDS_Face, TopoDS_Wire) => Option[Length]): OceOperation = {
    OceOperation(s, shape => utils.oce.Chamfer.wire(shape, select))
  }
  def wire(s: Solid, l: Length, select: (TopoDS_Face, TopoDS_Wire) => Boolean): OceOperation = {
    Chamfer.wire(s, (f: TopoDS_Face, e: TopoDS_Wire) => if (select(f,e)) Some(l) else None)
  }

  def face(s: Solid, select: TopoDS_Face => Option[Length]): OceOperation = {
    OceOperation(s, shape => utils.oce.Chamfer.face(shape, select))
  }
  def face(s: Solid, l: Length, select: TopoDS_Face => Boolean): OceOperation = {
    Chamfer.face(s, (f: TopoDS_Face) => if (select(f)) Some(l) else None)
  }

}
