package scadla.backends.oce

import org.jcae.opencascade.jni._
import scala.collection.mutable.HashSet

object TopoExplorerUnique {

  def iterator(shape: TopoDS_Shape) = new Iterator[TopoDS_Shape] {
    protected val it = new TopoDS_Iterator(shape)
    protected val seen = new HashSet[TopoDS_Shape]()
    protected var nextElement: TopoDS_Shape = null
    protected def findNext {
      while (nextElement == null && it.more()) {
        val v = it.value
        it.next
        if (!seen(v)) {
          seen += v
          nextElement = v
        }
      }
    }
    def next = {
      val v = nextElement
      findNext
      v
    }
    def hasNext = nextElement != null
    //get the first element
    findNext
  }

  private class UniqueTopoIterator[T <: TopoDS_Shape](shape: TopoDS_Shape, kind: TopAbs_ShapeEnum) extends Iterator[T] {
    protected val it = new TopExp_Explorer(shape, kind)
    protected val seen = new HashSet[T]()
    protected var nextElement: Option[T] = None
    protected def findNext {
      while (nextElement.isEmpty && it.more()) {
        val v = it.current.asInstanceOf[T]
        it.next
        if (!seen(v)) {
          seen += v
          nextElement = Some(v)
        }
      }
    }
    def next = {
      val v = nextElement.get
      nextElement = None
      findNext
      v
    }
    def hasNext = nextElement.isDefined
    //get the first element
    findNext
  }

  def vertices(shape: TopoDS_Shape): Iterator[TopoDS_Vertex] = new UniqueTopoIterator[TopoDS_Vertex](shape, TopAbs_ShapeEnum.VERTEX)

  def edges(shape: TopoDS_Shape): Iterator[TopoDS_Edge] = new UniqueTopoIterator[TopoDS_Edge](shape, TopAbs_ShapeEnum.EDGE)

  def wires(shape: TopoDS_Shape): Iterator[TopoDS_Wire] = new UniqueTopoIterator[TopoDS_Wire](shape, TopAbs_ShapeEnum.WIRE)

  def faces(shape: TopoDS_Shape): Iterator[TopoDS_Face] = new UniqueTopoIterator[TopoDS_Face](shape, TopAbs_ShapeEnum.FACE)

  def shells(shape: TopoDS_Shape): Iterator[TopoDS_Shell] = new UniqueTopoIterator[TopoDS_Shell](shape, TopAbs_ShapeEnum.SHELL)

  def solids(shape: TopoDS_Shape): Iterator[TopoDS_Solid] = new UniqueTopoIterator[TopoDS_Solid](shape, TopAbs_ShapeEnum.SOLID)

  def compSolids(shape: TopoDS_Shape): Iterator[TopoDS_CompSolid] = new UniqueTopoIterator[TopoDS_CompSolid](shape, TopAbs_ShapeEnum.COMPSOLID)

  def compounds(shape: TopoDS_Shape): Iterator[TopoDS_Compound] = new UniqueTopoIterator[TopoDS_Compound](shape, TopAbs_ShapeEnum.COMPOUND)

}
