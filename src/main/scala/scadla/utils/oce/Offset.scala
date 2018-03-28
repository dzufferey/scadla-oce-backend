package scadla.utils.oce

import squants.space.{Length, Angle}
import squants.space.Millimeters
import org.jcae.opencascade.jni._

class Offset(solid: TopoDS_Shape, distance: Length, tolerance: Length = Millimeters(1e-7)) {

  def result = {
    if (distance.toMillimeters == 0.0) {
      solid
    } else {
      val mf = new BRepOffsetAPI_MakeOffsetShape(solid, distance.toMillimeters, tolerance.toMillimeters)
      mf.shape
    }
  }

}

class ThickSolid(solid: TopoDS_Shape, distance: Length, tolerance: Length = Millimeters(1e-7)) {

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
      assert(distance.toMillimeters != 0.0)
      val mf = new BRepOffsetAPI_MakeThickSolid(solid, faces.toArray, distance.toMillimeters, tolerance.toMillimeters)
      mf.shape
    }
  }

}
