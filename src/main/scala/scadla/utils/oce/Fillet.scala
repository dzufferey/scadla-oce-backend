package scadla.utils.oce

import squants.space.{Length, Millimeters, LengthUnit}
import org.jcae.opencascade.jni._

class Fillet(solid: TopoDS_Shape, unit: LengthUnit = Millimeters) {

  protected val mf = new BRepFilletAPI_MakeFillet(solid)
  protected var trivial = true

  def add(radius: Length, edge: TopoDS_Edge) = {
    trivial = false
    mf.add(radius.to(unit), edge)
  }

  def result = {
    if (trivial) solid
    else mf.shape
  }

}

object Fillet {

  def apply(solid: TopoDS_Shape, filter: TopoDS_Edge => Option[Length], unit: LengthUnit = Millimeters) = {
    val mf = new Fillet(solid, unit)
    for (e <- TopoExplorerUnique.edges(solid);
         l <- filter(e)) {
      mf.add(l, e)
    }
    mf.result
  }
  
  def wire(solid: TopoDS_Shape, filter: TopoDS_Wire => Option[Length], unit: LengthUnit = Millimeters) = {
    val mf = new Fillet(solid, unit)
    for (w <- TopoExplorerUnique.wires(solid);
         l <- filter(w);
         e <- TopoExplorerUnique.edges(w)) {
      mf.add(l, e)
    }
    mf.result
  }
  
  def face(solid: TopoDS_Shape, filter: TopoDS_Face => Option[Length], unit: LengthUnit = Millimeters) = {
    val mf = new Fillet(solid, unit)
    for (f <- TopoExplorerUnique.faces(solid);
         l <- filter(f);
         e <- TopoExplorerUnique.edges(f)) {
      mf.add(l, e)
    }
    mf.result
  }

}
