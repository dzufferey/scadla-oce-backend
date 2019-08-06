package scadla

import squants.space.{Length, LengthUnit}
import org.jcae.opencascade.jni._
import scadla.backends.oce.ErrorPolicy._

case class OceShape(shape: TopoDS_Shape) extends Shape {
  assert(shape.shapeType == TopAbs_ShapeEnum.SOLID ||
         shape.shapeType == TopAbs_ShapeEnum.COMPSOLID ||
         shape.shapeType == TopAbs_ShapeEnum.COMPOUND)
}

case class OceOperation(s: Solid, op: (TopoDS_Shape, LengthUnit) => TopoDS_Shape,
                        onError: Option[ErrorPolicy] = None
                       ) extends Operation(Seq(s)) {
  def setChildren(c: Seq[Solid]) = {
    assert(c.length == 1)
    OceOperation(c.head, op)
  }

  def setErrorPolicy(errorPolicy: ErrorPolicy) = OceOperation(s, op, Some(errorPolicy))
}

object Fillet {

  def apply(s: Solid, select: TopoDS_Edge => Option[Length]): OceOperation = {
    OceOperation(s, (shape, unit) => utils.oce.Fillet(shape, select, unit))
  }
  def apply(s: Solid, radius: Length, select: TopoDS_Edge => Boolean): OceOperation = {
    Fillet(s, (e: TopoDS_Edge) => if (select(e)) Some(radius) else None)
  }

  def wire(s: Solid, select: TopoDS_Wire => Option[Length]): OceOperation = {
    OceOperation(s, (shape, unit) => utils.oce.Fillet.wire(shape, select, unit))
  }
  def wire(s: Solid, radius: Length, select: TopoDS_Wire => Boolean): OceOperation = {
    Fillet.wire(s, (e: TopoDS_Wire) => if (select(e)) Some(radius) else None)
  }

  def face(s: Solid, select: TopoDS_Face => Option[Length]): OceOperation = {
    OceOperation(s, (shape, unit) => utils.oce.Fillet.face(shape, select, unit))
  }
  def face(s: Solid, radius: Length, select: TopoDS_Face => Boolean): OceOperation = {
    Fillet.face(s, (e: TopoDS_Face) => if (select(e)) Some(radius) else None)
  }

  def shape(s: Solid, select: TopoDS_Shape => Iterable[(TopoDS_Edge, Length)]): OceOperation = {
    OceOperation(s, (shape, unit) => {
      val mf = new utils.oce.Fillet(shape, unit)
      for ((e,r) <- select(shape)) {
        mf.add(r, e)
      }
      mf.result
    })
  }
  def shape(s: Solid, radius: Length, select: TopoDS_Shape => Iterable[TopoDS_Edge]): OceOperation = {
    Fillet.shape(s, (e: TopoDS_Shape) => select(e).map( _ -> radius ) )
  }

}

object Chamfer {

  def apply(s: Solid, select: (TopoDS_Face, TopoDS_Edge) => Option[Length]): OceOperation = {
    OceOperation(s, (shape, unit) => utils.oce.Chamfer(shape, select, unit))
  }
  def apply(s: Solid, l: Length, select: (TopoDS_Face, TopoDS_Edge) => Boolean): OceOperation = {
    Chamfer(s, (f: TopoDS_Face, e: TopoDS_Edge) => if (select(f,e)) Some(l) else None)
  }

  def wire(s: Solid, select: (TopoDS_Face, TopoDS_Wire) => Option[Length]): OceOperation = {
    OceOperation(s, (shape, unit) => utils.oce.Chamfer.wire(shape, select, unit))
  }
  def wire(s: Solid, l: Length, select: (TopoDS_Face, TopoDS_Wire) => Boolean): OceOperation = {
    Chamfer.wire(s, (f: TopoDS_Face, e: TopoDS_Wire) => if (select(f,e)) Some(l) else None)
  }

  def face(s: Solid, select: TopoDS_Face => Option[Length]): OceOperation = {
    OceOperation(s, (shape, unit) => utils.oce.Chamfer.face(shape, select, unit))
  }
  def face(s: Solid, l: Length, select: TopoDS_Face => Boolean): OceOperation = {
    Chamfer.face(s, (f: TopoDS_Face) => if (select(f)) Some(l) else None)
  }

  def edge(s: Solid, select: TopoDS_Edge => Option[Length]): OceOperation = {
    Chamfer(s, (f: TopoDS_Face, e: TopoDS_Edge) => select(e))
  }
  def edge(s: Solid, l: Length, select: TopoDS_Edge => Boolean): OceOperation = {
    Chamfer(s, (f: TopoDS_Face, e: TopoDS_Edge) => if (select(e)) Some(l) else None)
  }

}

object ThickSolid {
  
  def apply(s: Solid, l: Length, toRemove: TopoDS_Shape => Iterable[TopoDS_Face]): OceOperation = {
    OceOperation(s, (shape, unit) => {
      val mf = new utils.oce.ThickSolid(shape, l, unit)
      mf.add(toRemove(shape))
      mf.result
    })
  }
  
  def face(s: Solid, l: Length, toRemove: TopoDS_Face => Boolean): OceOperation = {
    OceOperation(s, (shape, unit) => {
      val mf = new utils.oce.ThickSolid(shape, l, unit)
      for (f <- utils.oce.TopoExplorerUnique.faces(shape) if toRemove(f)) {
        mf.add(f)
      }
      mf.result
    })
  }

}

//not directly an OceOperation as it tends to give error ...
case class OceOffset(l: Length, s: Solid) extends Operation(Seq(s)) {

  def setChildren(c: Seq[Solid]) = {
    assert(c.length == 1)
    OceOffset(l, c.head)
  }

  private def distribute(l: Length, s: Solid): Solid = s match {
    case Empty => Empty
    case Union(args @ _*) => Union(args.map(distribute(l, _)): _*)
    case Intersection(args @ _*) => Intersection(args.map(distribute(l, _)): _*)
    case Difference(pos, neg @ _*) => Difference(distribute(l, pos), neg.map(distribute(-l, _)): _*)
    case Translate(x, y, z, s) => Translate(x, y, z, distribute(l,s))
    case Rotate(x, y, z, s) => Rotate(x, y, z, distribute(l,s))
    case Mirror(x, y, z, s) => Mirror(x, y, z, distribute(l,s))
    case Scale(x, y, z, s) if x == y && y == z => Scale(x, y, z, distribute(l/x,s))
    case other => OceOffset(l, other).asOceOperation
  }

  //FIXME tends to crash and it works better is we distribute the operation ...
  //However, it does not always give the expeected result
  def distribute: Solid = distribute(l, s)

  def asOceOperation: OceOperation = OceOperation(s, (shape, unit) => (new utils.oce.Offset(shape, l, unit)).result)

}

object Offset {

  def apply(l: Length, s: Solid) = {
    OceOffset(l, s)
  }

}

object LinearExtrude {

  def apply(x: Length, y: Length, z: Length, s: Solid): OceOperation = {
    val u = x.unit
    apply(Vector(x to u, y to u, z to u, u), s)
  }
  
  def apply(direction: Vector, s: Solid): OceOperation = {
    val zero = direction.unit(0)
    apply(Point(zero, zero, zero), direction, s)
  }

  def apply(origin: Point, direction: Vector, s: Solid): OceOperation = {
    OceOperation(s, (shape, unit) => {
      new utils.oce.Prism(shape, origin, direction, unit).result
    })
  }

}
