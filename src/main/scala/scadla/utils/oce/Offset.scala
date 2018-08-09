package scadla.utils.oce

import squants.space.{Length, Millimeters, LengthUnit}
import org.jcae.opencascade.jni._

class Offset(solid: TopoDS_Shape, distance: Length, tolerance: Length = Millimeters(1e-7), unit: LengthUnit = Millimeters) {

  def result = {
    if (distance.to(unit) == 0.0) {
      solid
    } else {
      val mf = new BRepOffsetAPI_MakeOffsetShape(solid, distance.to(unit), tolerance.to(unit))
      mf.shape
    }
  }

}

class ThickSolid(solid: TopoDS_Shape, distance: Length, tolerance: Length = Millimeters(1e-7), unit: LengthUnit = Millimeters) {

  protected var faces: List[TopoDS_Face] = Nil

  def add(face: TopoDS_Face) = {
    faces = face :: faces
  }
  
  def add(fs: Iterable[TopoDS_Face]) = {
    faces = fs.foldLeft(faces){ case (lst, f) => f :: lst }
  }

  def result = {
    if (faces.isEmpty) {
      solid
    } else {
      assert(distance.to(unit) != 0.0)
      val mf = new BRepOffsetAPI_MakeThickSolid(solid, faces.toArray, distance.to(unit), tolerance.to(unit))
      mf.shape
    }
  }

}
