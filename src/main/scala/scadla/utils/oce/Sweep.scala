package scadla.utils.oce

import squants.space.{Angle, Length, Millimeters, LengthUnit, Radians}
import org.jcae.opencascade.jni._
import org.jcae.opencascade.Utilities
import scadla.{Point, Vector}
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import ExtendedOps._

abstract class Sweep(shape: TopoDS_Shape, point: Point, normal: Vector, unit: LengthUnit) {

  protected val volumeUnit = (unit(1) * unit(1) * unit(1)).unit

  protected def process(s: TopoDS_Shape): TopoDS_Shape


  protected def fuse(_a: TopoDS_Shape, _b: TopoDS_Shape): TopoDS_Shape = {
    ///Logger("scadla.utils.oce.Sweep", Notice, "======")
    //Logger("scadla.utils.oce.Sweep", Notice, "fuse A")
    val a = copy(_a)
    //Utilities.dumpTopology(a, Console.out)
    //Logger("scadla.utils.oce.Sweep", Notice, "======")
    //Logger("scadla.utils.oce.Sweep", Notice, "fuse B")
    val b = copy(_b)
    //Utilities.dumpTopology(b, Console.out)
    val fuser = new BRepAlgoAPI_Fuse(a, b)
    fuser.build()
    if (!fuser.isDone){
      Logger("scadla.utils.oce.Sweep", Error, "failed to fuse the result, throwing shapes out and hoping for the best")
      null
    } else {
      val res = fuser.shape()
      if (res.isValid) {
        Logger("scadla.utils.oce.Sweep", Debug, "fuse OK")
        res
      } else {
        Logger("scadla.utils.oce.Sweep", Error, "fuse result is invalid, throwing shapes out and hoping for the best")
        empty
      }
      //Logger("scadla.utils.oce.Sweep", Notice, "===========")
      //Logger("scadla.utils.oce.Sweep", Notice, "fuse result")
      //Utilities.dumpTopology(res, Console.out)
    }
  }

  protected def reduce(shapes: Iterator[TopoDS_Shape]): TopoDS_Shape = {
    val solids = shapes.flatMap(_.solids)
    var acc: TopoDS_Shape = null
    while (solids.hasNext) {
      var n = solids.next
      if (n.isValid) {
        val vol = n.asInstanceOf[TopoDS_Solid].volume(volumeUnit)
        Logger("scadla.utils.oce.Sweep", Debug, "solid with volume "+vol)
        if ( vol > volumeUnit(0.0) ) {
          acc = if (acc == null) n else fuse(acc, n)
        } else {
          Logger("scadla.utils.oce.Sweep", Notice, "degenerated solid, skipping")
        }
      } else {
        Logger("scadla.utils.oce.Sweep", Notice, "invalid shape, skipping")
      }
    }
    if (acc == null) {
      Logger("scadla.utils.oce.Sweep", Notice, "empty")
      empty
    } else {
      acc
    }
  }

  def result = {
    val faces = Slice(shape, point, normal, unit).iterator
    val shapes = faces.map(process)
    val res = reduce(shapes)
    res
  }
}

class Prism(shape: TopoDS_Shape, start: Point, direction: Vector, unit: LengthUnit = Millimeters) extends Sweep(shape, start, direction, unit) {
    
  assert((direction.norm to unit) > 0.0, "direction ill-defined")
  protected val dir = Array[Double](direction.x to unit, direction.y to unit, direction.z to unit)

  protected def process(shape: TopoDS_Shape) = {
    //Logger("scadla.utils.oce.Prism", Notice, "mkPrism before")
    //Utilities.dumpTopology(shape, Console.out)
    val mf = new BRepPrimAPI_MakePrism(shape, dir)
    val res = mf.shape
    if (res == null) {
      Logger("scadla.utils.oce.Prism", Error, "BRepPrimAPI_MakePrism returned null")
      empty
    } else {
      //Logger("scadla.utils.oce.Prism", Notice, "mkPrism after")
      //Utilities.dumpTopology(res, Console.out)
      res
    }
  }

}

class Revolution(shape: TopoDS_Shape, start: Point, axis: Vector, angle: Angle,
                 origin: Point = new Point(Millimeters(0), Millimeters(0), Millimeters(0)),
                 unit: LengthUnit = Millimeters) extends Sweep(shape, start, (start - origin).cross(axis).toUnitVector, unit) {

  protected val a = {
    assert((axis.norm to unit) > 0.0, "axis ill-defined")
    val u = (axis to unit).toUnitVector
    Array[Double](origin.x to unit, origin.y to unit, origin.z to unit,
                  u.x to unit, u.y to unit, u.z to unit)
  }

  protected def process(shape: TopoDS_Shape) = {
    val mf = new BRepPrimAPI_MakeRevol(shape, a, angle to Radians)
    mf.shape
  }

  override def result = {
    if (angle.to(Radians) == 0.0) {
      shape
    } else {
      super.result
    }
  }

}

//points in the spine may have a tangent vectors
class Pipe(shape: TopoDS_Shape, spine: Seq[(Point,Option[Vector])], unit: LengthUnit = Millimeters, tolerance: Double = 1e-7) extends Sweep(shape, spine(0)._1, Pipe.getFirstTangent(spine), unit) {

  val wire = {
    val points = Array.ofDim[Double](3 * spine.size)
    val tgs = Array.ofDim[Double](3 * spine.size)
    val hasTgs = Array.ofDim[Boolean](3 * spine.size)
    spine.zipWithIndex.foreach{ case ((p, t), i) => 
      points(3 * i) = p.x to unit
      points(3 * i + 1) = p.y to unit
      points(3 * i + 2) = p.z to unit
      t match {
        case Some(v) =>
          assert(v.norm.to(unit) > 0.0, "tangent ill-defined")
          tgs(3 * i) = v.x to unit
          tgs(3 * i + 1) = v.y to unit
          tgs(3 * i + 2) = v.z to unit
          hasTgs(i) = true
        case None =>
          tgs(3 * i) = 0.0
          tgs(3 * i + 1) = 0.0
          tgs(3 * i + 2) = 0.0
          hasTgs(i) = false
      }
    }
    val interp = new GeomAPI_Interpolate(points, false, tolerance)
    if (hasTgs.exists(x => x)) {
      interp.Load(tgs, hasTgs)
    }
    interp.Perform()
    val curve = interp.Curve()
    val edge = new BRepBuilderAPI_MakeEdge(curve).shape().asInstanceOf[TopoDS_Edge]
    new BRepBuilderAPI_MakeWire(edge).shape().asInstanceOf[TopoDS_Wire]
  }

  protected def process(shape: TopoDS_Shape) = {
    val mf = new BRepOffsetAPI_MakePipe(wire, shape)
    mf.shape
  }

}

object Pipe {

  def getFirstTangent(spine: Seq[(Point,Option[Vector])]): Vector = {
    spine(0)._2.getOrElse( (spine(0)._1 - spine(1)._1).toUnitVector )
  }
    
  //spine has no tangent
  def apply(shape: TopoDS_Shape, spine: Seq[Point], unit: LengthUnit = Millimeters, tolerance: Double = 1e-7) = {
    new Pipe(shape, spine.map(x => (x, None)), unit, tolerance)
  }

}
