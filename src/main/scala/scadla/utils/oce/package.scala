package scadla.utils

import scadla._
import squants.space.{Length, Angle, Area, Volume}
import squants.space.Millimeters
import org.jcae.opencascade.jni._
import oce.ExtendedOps._

package object oce {
  def continuityToString(kind: GeomAbs_Shape) = kind match {
    case GeomAbs_Shape.C0 => "C0"
    case GeomAbs_Shape.G1 => "G1"
    case GeomAbs_Shape.C1 => "C1"
    case GeomAbs_Shape.G2 => "G2"
    case GeomAbs_Shape.C2 => "C2"
    case GeomAbs_Shape.C3 => "C3"
    case GeomAbs_Shape.CN => "CN"
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


  def toPoint(p: Array[Double]) = Point(Millimeters(p(0)), Millimeters(p(1)), Millimeters(p(2)))
  
  def toVector(p: Array[Double]) = Vector(p(0), p(1), p(2), Millimeters)

  def connectedAndSameDirection(c1: Adaptor3d_Curve, c2: Adaptor3d_Curve, toleranceP: Double = 1e-7, toleranceD: Double = 1e-1) = {
    val p1 = Array.ofDim[Double](3)
    val d1 = Array.ofDim[Double](3)
    c1.d1(c1.lastParameter, p1, d1)
    val p2 = Array.ofDim[Double](3)
    val d2 = Array.ofDim[Double](3)
    c2.d1(c2.firstParameter, p2, d2)
    (toPoint(p1) to toPoint(p2)).norm <= Millimeters(toleranceP) &&
    (toVector(d1).toUnitVector - toVector(d2).toUnitVector).norm <= Millimeters(toleranceD)
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

}
