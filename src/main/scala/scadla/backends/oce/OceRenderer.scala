package scadla.backends.oce

import scadla._
import scadla.backends.RendererAux
import squants.space.Length
import squants.space.Angle
import org.jcae.opencascade.jni._

//TODO TopoDS_Solid or TopoDS_Shape ?

class OceRenderer extends RendererAux[TopoDS_Shape] {

  def empty: TopoDS_Shape = ???

  def union(objs: Seq[TopoDS_Shape]): TopoDS_Shape = {
    if (objs.length == 0) {
      empty
    } else if (objs.length == 1) {
      objs.head
    } else {
      objs.reduceLeft( (x,y) => new BRepAlgoAPI_Fuse(x, y).shape() )
    }
  }

  def intersection(objs: Seq[TopoDS_Shape]): TopoDS_Shape = {
    if (objs.length == 0) {
      empty
    } else if (objs.length == 1) {
      objs.head
    } else {
      objs.reduceLeft( (x,y) => new BRepAlgoAPI_Common(x, y).shape() )
    }
  }

  def difference(pos: TopoDS_Shape, negs: Seq[TopoDS_Shape]): TopoDS_Shape = {
    negs.foldLeft(pos)( (p,n) => new BRepAlgoAPI_Cut(p, n).shape() )
  }

  def minkowski(objs: Seq[TopoDS_Shape]): TopoDS_Shape = ???

  def hull(objs: Seq[TopoDS_Shape]): TopoDS_Shape = ???

  def polyhedron(p: Polyhedron): TopoDS_Shape = ???

  def cube(width: Length, depth: Length, height: Length): TopoDS_Shape = {
    val lowerLeft = Array[Double](0.0, 0.0, 0.0)
    val upperRight = Array[Double](width.toMillimeters, depth.toMillimeters, height.toMillimeters)
    new BRepPrimAPI_MakeBox(lowerLeft, upperRight).shape
  }

  def sphere(radius: Length): TopoDS_Shape = {
    val origin = Array[Double](0.0, 0.0, 0.0)
    new BRepPrimAPI_MakeSphere(origin, radius.toMillimeters).shape
  }

  def cylinder(radiusBot: Length, radiusTop: Length, height: Length): TopoDS_Shape = {
    val rb = radiusBot.toMillimeters
    val rt = radiusTop.toMillimeters
    val h = height.toMillimeters
    assert(rb >= 0.0)
    assert(rt >= 0.0)
    assert(h >= 0.0)
    val origin = Array[Double](0,0,0, 0,0,1) // {X, Y, Z, directionX, directionY, directionZ}
    if (rb == 0.0 && rt == 0.0) {
      empty
    } else if (rb == rt) {
      new BRepPrimAPI_MakeCylinder(origin, rb, h, math.Pi*2).shape
    } else {
      new BRepPrimAPI_MakeCone(origin, rb, rt, h, math.Pi*2).shape
    }
  }

  def fromFile(path: String, format: String): TopoDS_Shape = ???

  def multiply(m: Matrix, obj: TopoDS_Shape): TopoDS_Shape = ???

  def toMesh(aux: TopoDS_Shape): Polyhedron = ???

}
