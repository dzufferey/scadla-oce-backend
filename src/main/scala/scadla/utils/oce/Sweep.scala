package scadla.utils.oce

import squants.space.{Angle, Length, Millimeters, LengthUnit, Radians}
import org.jcae.opencascade.jni._
import scadla.{Point, Vector}
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import ExtendedOps.ShapeOps

class Sweep(shape: TopoDS_Shape) {

  protected def reduce(shapes: TraversableOnce[TopoDS_Shape]): TopoDS_Shape = {
    val empty = new BRepBuilderAPI_MakeSolid().shape()
    shapes.fold(empty)( (x,y) => new BRepAlgoAPI_Fuse(x, y).shape() )
  }

  protected def findShellOrFace(shape: TopoDS_Shape) = {
    if (shape.isInstanceOf[TopoDS_Face] || shape.isInstanceOf[TopoDS_Shell]) {
      Seq(shape)
    } else {
      Logger("scadla.utils.oce.Sweep", Info, "The shape is not a face or shell, try to extract a face or shell")
      //explore the shape to extract faces/shells
      val shells = shape.shells
      if (shells.length > 0) {
        shells
      } else {
        shape.faces
      }
    }
  }
}

class Prism(shape: TopoDS_Shape, direction: Vector, unit: LengthUnit = Millimeters) extends Sweep(shape) {
    
  protected val dir = Array[Double](direction.x to unit, direction.y to unit, direction.z to unit)

  protected def mkPrism(shape: TopoDS_Shape) = {
    val mf = new BRepPrimAPI_MakePrism(shape, dir)
    mf.shape
  }

  def result = {
    val shapes = findShellOrFace(shape).map(mkPrism)
    reduce(shapes)
  }

}

class Revolution(shape: TopoDS_Shape, axis: Vector, angle: Angle, unit: LengthUnit = Millimeters) extends Sweep(shape) {

  protected val a = {
    val u = (axis to unit).toUnitVector
    Array[Double](u.x to unit, u.y to unit, u.z to unit)
  }

  protected def mkRevol(shape: TopoDS_Shape) = {
    val mf = new BRepPrimAPI_MakeRevol(shape, a, angle to Radians)
    mf.shape
  }

  def result = {
    val shapes = findShellOrFace(shape).map(mkRevol)
    reduce(shapes)
  }

}

class Pipe(shape: TopoDS_Shape, spine: Seq[Point], unit: LengthUnit = Millimeters, tolerance: Double = 1e-7) extends Sweep(shape) {

  val wire = {
    val points = Array.ofDim[Double](3 * spine.size)
    spine.zipWithIndex.foreach{ case (p, i) => 
      points(3 * i) = p.x to unit
      points(3 * i + 1) = p.y to unit
      points(3 * i + 2) = p.z to unit
    }
    val interp = new GeomAPI_Interpolate(points, false, tolerance)
    interp.Perform()
    val curve = interp.Curve()
    val edge = new BRepBuilderAPI_MakeEdge(curve).shape().asInstanceOf[TopoDS_Edge]
    new BRepBuilderAPI_MakeWire(edge).shape().asInstanceOf[TopoDS_Wire]
  }
  
  protected def mkPipe(shape: TopoDS_Shape) = {
    val mf = new BRepOffsetAPI_MakePipe(wire, shape)
    mf.shape
  }

  def result = {
    val shapes = findShellOrFace(shape).map(mkPipe)
    reduce(shapes)
  }

}
