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
  
  def apply(solid: TopoDS_Shape, filter: (TopoDS_Face, TopoDS_Edge) => Option[Length]) = {
    val mf = new Chamfer(solid)
    for (f <- TopoExplorerUnique.faces(solid);
         e <- TopoExplorerUnique.edges(f);
         l <- filter(f,e)) {
      mf.add(f, e, l)
    }
    mf.result
  }

  def wire(solid: TopoDS_Shape, filter: (TopoDS_Face, TopoDS_Wire) => Option[Length]) = {
    val mf = new Chamfer(solid)
    for (f <- TopoExplorerUnique.faces(solid);
         w <- TopoExplorerUnique.wires(f);
         l <- filter(f,w);
         e <- TopoExplorerUnique.edges(f)) {
      mf.add(f, e, l)
    }
    mf.result
  }
  
  def face(solid: TopoDS_Shape, filter: TopoDS_Face => Option[Length]) = {
    val mf = new Chamfer(solid)
    for (f <- TopoExplorerUnique.faces(solid);
         l <- filter(f);
         e <- TopoExplorerUnique.edges(f)) {
      mf.add(f, e, l)
    }
    mf.result
  }

}
