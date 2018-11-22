package scadla.utils

import scadla._
import squants.space.{LengthUnit, Length, Millimeters}
import org.jcae.opencascade.jni._
import oce.ExtendedOps._
import java.io.BufferedWriter

package object oce {

  def copy[A <: TopoDS_Shape](shape: A, copyGeom: Boolean = true, copyMesh: Boolean = false): A = {
    new BRepBuilderAPI_Copy(shape, copyGeom, copyMesh).shape().asInstanceOf[A]
  }

  def empty: TopoDS_Shape = {
    val ms = new BRepBuilderAPI_MakeSolid()
    ms.shape() //FIXME this returns null, find a better way, use Compound instead?
  }

  def continuityToString(kind: GeomAbs_Shape) = kind match {
    case GeomAbs_Shape.C0 => "C0"
    case GeomAbs_Shape.G1 => "G1"
    case GeomAbs_Shape.C1 => "C1"
    case GeomAbs_Shape.G2 => "G2"
    case GeomAbs_Shape.C2 => "C2"
    case GeomAbs_Shape.C3 => "C3"
    case GeomAbs_Shape.CN => "CN"
  }

  def kindToString(kind: TopAbs_ShapeEnum) = kind match {
    case TopAbs_ShapeEnum.SHAPE =>      "Shape"
    case TopAbs_ShapeEnum.VERTEX =>     "Vertex"
    case TopAbs_ShapeEnum.EDGE =>       "Edge"
    case TopAbs_ShapeEnum.WIRE =>       "Wire"
    case TopAbs_ShapeEnum.FACE =>       "Face"
    case TopAbs_ShapeEnum.SHELL =>      "Shell"
    case TopAbs_ShapeEnum.SOLID =>      "Solid"
    case TopAbs_ShapeEnum.COMPSOLID =>  "CompSolid"
    case TopAbs_ShapeEnum.COMPOUND =>   "Compound"
  }

  def kindToString(kind: GeomAbs_CurveType) = kind match {
    case GeomAbs_CurveType.GeomAbs_Line =>          "Line"
    case GeomAbs_CurveType.GeomAbs_Circle =>        "Circle"
    case GeomAbs_CurveType.GeomAbs_Ellipse =>       "Ellipse"
    case GeomAbs_CurveType.GeomAbs_Hyperbola =>     "Hyperbola"
    case GeomAbs_CurveType.GeomAbs_Parabola =>      "Parabola"
    case GeomAbs_CurveType.GeomAbs_BezierCurve =>   "BezierCurve"
    case GeomAbs_CurveType.GeomAbs_BSplineCurve =>  "BSplineCurve"
    case GeomAbs_CurveType.GeomAbs_OtherCurve =>    "OtherCurve"
  }

  def kindToString(kind: GeomAbs_SurfaceType) = kind match {
    case GeomAbs_SurfaceType.GeomAbs_Plane =>                 "Plane"
    case GeomAbs_SurfaceType.GeomAbs_Cylinder =>              "Cylinder"
    case GeomAbs_SurfaceType.GeomAbs_Cone =>                  "Cone"
    case GeomAbs_SurfaceType.GeomAbs_Sphere =>                "Sphere"
    case GeomAbs_SurfaceType.GeomAbs_Torus =>                 "Torus"
    case GeomAbs_SurfaceType.GeomAbs_BezierSurface =>         "BezierSurface"
    case GeomAbs_SurfaceType.GeomAbs_BSplineSurface =>        "BSplineSurface"
    case GeomAbs_SurfaceType.GeomAbs_SurfaceOfRevolution =>   "SurfaceOfRevolution"
    case GeomAbs_SurfaceType.GeomAbs_SurfaceOfExtrusion =>    "SurfaceOfExtrusion"
    case GeomAbs_SurfaceType.GeomAbs_OffsetSurface =>         "OffsetSurface"
    case GeomAbs_SurfaceType.GeomAbs_OtherSurface =>          "OtherSurface"
  }

  def hasGeometry(shape: TopoDS_Shape) = shape.shapeType match {
    case TopAbs_ShapeEnum.EDGE | 
         TopAbs_ShapeEnum.WIRE |
         TopAbs_ShapeEnum.FACE => true
    case _ => false
  }

  def toPoint(p: Array[Double], unit: LengthUnit) = Point(unit(p(0)), unit(p(1)), unit(p(2)))
  
  def toVector(p: Array[Double], unit: LengthUnit) = Vector(p(0), p(1), p(2), unit)

  def connectedAndSameDirection(c1: Adaptor3d_Curve, c2: Adaptor3d_Curve, toleranceP: Double = 1e-7, toleranceD: Double = 1e-1) = {
    val unit = Millimeters //actual unit does not matter
    val p1 = Array.ofDim[Double](3)
    val d1 = Array.ofDim[Double](3)
    c1.d1(c1.lastParameter, p1, d1)
    val p2 = Array.ofDim[Double](3)
    val d2 = Array.ofDim[Double](3)
    c2.d1(c2.firstParameter, p2, d2)
    (toPoint(p1, unit) to toPoint(p2, unit)).norm <= unit(toleranceP) &&
    (toVector(d1, unit).toUnitVector - toVector(d2, unit).toUnitVector).norm <= unit(toleranceD)
  }

  def atLeastC1(g: GeomAbs_Shape) = g match {
    case GeomAbs_Shape.C0 => false
    case _ => true
  }

  def c1Continuous(edges: Iterator[TopoDS_Edge]): Boolean = {
    if (!edges.hasNext) {
      return false
    }
    val first = edges.next
    val firstAdaptor = new BRepAdaptor_Curve(first)
    if (first.isDegenerate || !atLeastC1(firstAdaptor.continuity)) {
      return false
    }

    var current = first
    var currentAdaptor = firstAdaptor
    while (edges.hasNext) {
      val previous = current
      val previousAdaptor = currentAdaptor
      current = edges.next
      currentAdaptor = new BRepAdaptor_Curve(current)
      if (current.isDegenerate || !atLeastC1(currentAdaptor.continuity)) {
        return false
      }
      if (!connectedAndSameDirection(previousAdaptor, currentAdaptor)) {
        return false
      }
    }
    if (!connectedAndSameDirection(currentAdaptor, firstAdaptor)) {
      return false
    } else {
      return true
    }
  }

  def c1Continuous(edges: Iterable[TopoDS_Edge]): Boolean = c1Continuous(edges.iterator)

  // return [a,b,c,d] with ax + by + cx + d = 0
  def planeCoeff(point: Point, normal: Vector, unit: LengthUnit): Array[Double] = {
    val n = normal.to(unit).toUnitVector
    val a = n.x.to(unit)
    val b = n.y.to(unit)
    val c = n.z.to(unit)
    val d = -1 * (a * point.x + b * point.y + c * point.z).to(unit)
    Array[Double](a, b, c, d)
  }

}
