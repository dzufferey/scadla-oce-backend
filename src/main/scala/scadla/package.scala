package scadla.utils

import scadla._
import squants.space.{Length, Angle, Area, Volume}
import squants.space.Millimeters
import org.jcae.opencascade.jni._

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

  def toPoint(p: Array[Double]) = Point(Millimeters(p(0)), Millimeters(p(1)), Millimeters(p(2)))
  
  def toVector(p: Array[Double]) = Vector(p(0), p(1), p(2), Millimeters)

}
