package scadla.backends.oce

import org.jcae.opencascade.jni._

object TopoExplorer {

  def iterator(shape: TopoDS_Shape) = new Iterator[TopoDS_Shape] {
    protected val it = new TopoDS_Iterator(shape)
    def next = {
      val v = it.value
      it.next
      v
    }
    def hasNext =  it.more()
  }

  def vertices(shape: TopoDS_Shape) = new Iterator[TopoDS_Vertex] {
    protected val it = new TopExp_Explorer(shape, TopAbs_ShapeEnum.VERTEX)
    def next = {
      val v = it.current.asInstanceOf[TopoDS_Vertex]
      it.next
      v
    }
    def hasNext =  it.more
  }

  def edges(shape: TopoDS_Shape) = new Iterator[TopoDS_Edge] {
    protected val it = new TopExp_Explorer(shape, TopAbs_ShapeEnum.EDGE)
    def next = {
      val v = it.current.asInstanceOf[TopoDS_Edge]
      it.next
      v
    }
    def hasNext = it.more
  }

  def wires(shape: TopoDS_Shape) = new Iterator[TopoDS_Wire] {
    protected val it = new TopExp_Explorer(shape, TopAbs_ShapeEnum.WIRE)
    def next = {
      val v = it.current.asInstanceOf[TopoDS_Wire]
      it.next
      v
    }
    def hasNext = it.more
  }

  def faces(shape: TopoDS_Shape) = new Iterator[TopoDS_Face] {
    protected val it = new TopExp_Explorer(shape, TopAbs_ShapeEnum.FACE)
    def next = {
      val v = it.current.asInstanceOf[TopoDS_Face]
      it.next
      v
    }
    def hasNext = it.more
  }

  def shells(shape: TopoDS_Shape) = new Iterator[TopoDS_Shell] {
    protected val it = new TopExp_Explorer(shape, TopAbs_ShapeEnum.SHELL)
    def next = {
      val v = it.current.asInstanceOf[TopoDS_Shell]
      it.next
      v
    }
    def hasNext = it.more
  }

  def solids(shape: TopoDS_Shape) = new Iterator[TopoDS_Solid] {
    protected val it = new TopExp_Explorer(shape, TopAbs_ShapeEnum.SOLID)
    def next = {
      val v = it.current.asInstanceOf[TopoDS_Solid]
      it.next
      v
    }
    def hasNext = it.more
  }

  def compSolids(shape: TopoDS_Shape) = new Iterator[TopoDS_CompSolid] {
    protected val it = new TopExp_Explorer(shape, TopAbs_ShapeEnum.COMPSOLID)
    def next = {
      val v = it.current.asInstanceOf[TopoDS_CompSolid]
      it.next
      v
    }
    def hasNext = it.more
  }

  def compound(shape: TopoDS_Shape) = new Iterator[TopoDS_Compound] {
    protected val it = new TopExp_Explorer(shape, TopAbs_ShapeEnum.COMPOUND)
    def next = {
      val v = it.current.asInstanceOf[TopoDS_Compound]
      it.next
      v
    }
    def hasNext = it.more
  }

}

