package scadla.utils.oce

import squants.space.{Length, Millimeters, LengthUnit}
import org.jcae.opencascade.jni._

class Chamfer(solid: TopoDS_Shape, unit: LengthUnit = Millimeters) {

  protected val mf = if (solid != null) new BRepFilletAPI_MakeChamfer(solid) else null
  protected var trivial = true

  def add(face: TopoDS_Face, edge: TopoDS_Edge, dist: Length) = {
    trivial = false
    mf.add(dist.to(unit), edge, face)
  }

  def result = {
    if (trivial) {
      solid
    } else {
      val res = mf.shape
      if (res == null) {
        sys.error("Chamfer failed")
      } else {
        res
      }
    }
  }

}

object Chamfer {

  def apply(solid: TopoDS_Shape, filter: (TopoDS_Face, TopoDS_Edge) => Option[Length], unit: LengthUnit = Millimeters) = {
    val mf = new Chamfer(solid, unit)
    for (f <- TopoExplorerUnique.faces(solid);
         e <- TopoExplorerUnique.edges(f);
         l <- filter(f,e)) {
      mf.add(f, e, l)
    }
    mf.result
  }

  def wire(solid: TopoDS_Shape, filter: (TopoDS_Face, TopoDS_Wire) => Option[Length], unit: LengthUnit = Millimeters) = {
    val mf = new Chamfer(solid, unit)
    for (f <- TopoExplorerUnique.faces(solid);
         w <- TopoExplorerUnique.wires(f);
         l <- filter(f,w);
         e <- TopoExplorerUnique.edges(f)) {
      mf.add(f, e, l)
    }
    mf.result
  }

  def face(solid: TopoDS_Shape, filter: TopoDS_Face => Option[Length], unit: LengthUnit = Millimeters) = {
    val mf = new Chamfer(solid, unit)
    for (f <- TopoExplorerUnique.faces(solid);
         l <- filter(f);
         e <- TopoExplorerUnique.edges(f)) {
      mf.add(f, e, l)
    }
    mf.result
  }

}
