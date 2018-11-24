package scadla.utils.oce

import scadla._
import squants.space.{LengthUnit, Length, Millimeters}
import org.jcae.opencascade.jni._
import ExtendedOps._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object Slice {

  def apply(shape: TopoDS_Shape, point: Point, normal: Vector, unit: LengthUnit = Millimeters): Seq[TopoDS_Face] = {
    val pc = planeCoeff(point, normal, unit)
    val plane = new BRepBuilderAPI_MakeFace(pc).shape()
    val section = new BRepAlgoAPI_Section(shape, plane)
    section.build()
    if (section.isDone()) {
      val slice = section.shape()
      postprocess(pc, slice)
    } else {
      sys.error("slice failed")
    }
  }

  def postprocess(planeCoeff: Array[Double], shape: TopoDS_Shape): Seq[TopoDS_Face] = {
    //org.jcae.opencascade.Utilities.dumpTopology(shape, Console.out)
    def mkFacesFromWires(wires: Seq[TopoDS_Wire]): Seq[TopoDS_Face] = {
      wires.map( w => new BRepBuilderAPI_MakeFace(planeCoeff, w).shape().asInstanceOf[TopoDS_Face] )
    }
    //assume we have a set of edgeds forming disjoint wires
    def mkFacesFromEdges(edges: Iterable[TopoDS_Edge]): Seq[TopoDS_Face] = {
      var toProcess = edges.toSet
      var acc: List[TopoDS_Wire] = Nil
      while (!toProcess.isEmpty) {
        val wire = new BRepBuilderAPI_MakeWire()
        var e = toProcess.head
        toProcess = toProcess.tail
        val start = e.start
        var end = e.end
        wire.add(e)
        while (start != end) {
          toProcess.find(e =>  e.start == end || e.end == end) match {
            case Some(e) =>
              wire.add(e)
              toProcess = toProcess.filter( _ != e )
              end = if (e.start == end) e.end else e.start
            case None =>
              Logger.logAndThrow("scadla.utils.oce.Slice", Error, "cannot close the loop")
          }
        }
        acc ::= wire.shape().asInstanceOf[TopoDS_Wire]
      }
      mkFacesFromWires(acc)
    }
    shape.shapeType match {
      case TopAbs_ShapeEnum.COMPOUND =>
        //need to dig further
        mkFacesFromEdges(shape.edges)
      case TopAbs_ShapeEnum.EDGE =>
        //make a wire
        val e = shape.asInstanceOf[TopoDS_Edge]
        assert(e.start == e.end)
        mkFacesFromEdges(List(e))
      case TopAbs_ShapeEnum.WIRE =>
        val w = shape.asInstanceOf[TopoDS_Wire]
        mkFacesFromWires(List(w))
      case TopAbs_ShapeEnum.FACE =>
        Seq(shape.asInstanceOf[TopoDS_Face])
      case _ =>
        sys.error("unexpected shape:" + shape)
    }
  }

}
