package scadla.utils.oce

import squants.space.{Length, Angle}
import squants.space.Millimeters
import org.jcae.opencascade.jni._

class Chamfer(solid: TopoDS_Shape) {

  protected val mf = new BRepFilletAPI_MakeChamfer(solid)

  def add(face: TopoDS_Face, edge: TopoDS_Edge, dist: Length) = {
    mf.add(dist.toMillimeters, edge, face)
  }

  def result = mf.shape

}

object Chamfer {
  
  def apply(solid: TopoDS_Shape, filter: (TopoDS_Shape, TopoDS_Face, TopoDS_Edge) => Option[Length]) = {
    val mf = new Chamfer(solid)
    for (f <- TopoExplorerUnique.faces(solid);
         e <- TopoExplorerUnique.edges(f);
         l <- filter(solid,f,e)) {
      mf.add(f, e, l)
    }
    mf.result
  }

  def apply(solid: TopoDS_Shape, filter: TopoDS_Face => Iterable[(TopoDS_Edge, Length)]) = {
    val mf = new Chamfer(solid)
    for (f <- TopoExplorerUnique.faces(solid);
         (e,l) <- filter(f)) {
      mf.add(f, e, l)
    }
    mf.result
  }
}
