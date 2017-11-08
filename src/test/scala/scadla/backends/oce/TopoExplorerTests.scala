package scadla.backends.oce

import org.scalatest._
import org.jcae.opencascade.jni._

class TopoExplorerTest extends FunSuite {

  def getCube = {
    val lowerLeft = Array[Double](0, 0, 0)
    val upperRight = Array[Double](5, 10, 20)
    new BRepPrimAPI_MakeBox(lowerLeft, upperRight).shape
  }

  def getSphere = {
    val radius = 5.0
    val origin = Array[Double](0, 0, 0)
    new BRepPrimAPI_MakeSphere(origin, radius).shape
  }

  test("vertices in a cube") {
    val c = getCube
    var set = Set[TopoDS_Vertex]()
    var i = 0
    for (v <- TopoExplorer.vertices(c)) {
      i = i + 1
      set = set + v
    }
    assert(set.size == 8)
    assert(i == 48) //each vertex is explored 6 times: 3 faces, 2 edges per face.
  }

}
