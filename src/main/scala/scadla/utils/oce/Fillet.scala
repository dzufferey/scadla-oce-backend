package scadla.utils.oce

import squants.space.{Length, Angle}
import squants.space.Millimeters
import org.jcae.opencascade.jni._

class Fillet(solid: TopoDS_Shape) {

  protected val mf = new BRepFilletAPI_MakeFillet(solid)

  def add(radius: Length, edge: TopoDS_Edge) = mf.add(radius.toMillimeters, edge)

  def result = mf.shape

}

object Fillet {

  def apply(solid: TopoDS_Shape, filter: TopoDS_Edge => Option[Length]) = {
    val mf = new Fillet(solid)
    for (e <- TopoExplorerUnique.edges(solid);
         l <- filter(e)) {
      mf.add(l, e)
    }
    mf.result
  }

}
