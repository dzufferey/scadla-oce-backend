package scadla.utils.oce

import org.jcae.opencascade.jni._

object TopoExplorer {

  def iterator(shape: TopoDS_Shape) = new Iterator[TopoDS_Shape] {
    protected val it = new TopoDS_Iterator(shape)
    def next = {
      val v = it.value
      it.next
      v
    }
    def hasNext = it.more()
  }

  private class TopoIterator[T <: TopoDS_Shape](shape: TopoDS_Shape, kind: TopAbs_ShapeEnum) extends Iterator[T] {
    protected val it = new TopExp_Explorer(shape, kind)
    def next = {
      val v = it.current.asInstanceOf[T]
      it.next
      v
    }
    def hasNext = it.more
  }

  def vertices(shape: TopoDS_Shape): Iterator[TopoDS_Vertex] = new TopoIterator[TopoDS_Vertex](shape, TopAbs_ShapeEnum.VERTEX)

  def edges(shape: TopoDS_Shape): Iterator[TopoDS_Edge] = new TopoIterator[TopoDS_Edge](shape, TopAbs_ShapeEnum.EDGE)

  def wires(shape: TopoDS_Shape): Iterator[TopoDS_Wire] = new TopoIterator[TopoDS_Wire](shape, TopAbs_ShapeEnum.WIRE)

  def faces(shape: TopoDS_Shape): Iterator[TopoDS_Face] = new TopoIterator[TopoDS_Face](shape, TopAbs_ShapeEnum.FACE)

  def shells(shape: TopoDS_Shape): Iterator[TopoDS_Shell] = new TopoIterator[TopoDS_Shell](shape, TopAbs_ShapeEnum.SHELL)

  def solids(shape: TopoDS_Shape): Iterator[TopoDS_Solid] = new TopoIterator[TopoDS_Solid](shape, TopAbs_ShapeEnum.SOLID)

  def compSolids(shape: TopoDS_Shape): Iterator[TopoDS_CompSolid] = new TopoIterator[TopoDS_CompSolid](shape, TopAbs_ShapeEnum.COMPSOLID)

}

