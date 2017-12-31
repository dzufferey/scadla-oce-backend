package scadla.backends.oce

import org.scalatest._
import org.jcae.opencascade.jni._
import TestCommon._

class TopoExplorerTest extends FunSuite {

  test("vertices in a cube") {
    val c = getCube()
    var set = Set[TopoDS_Vertex]()
    var i = 0
    for (v <- TopoExplorer.vertices(c)) {
      i = i + 1
      set = set + v
    }
    assert(set.size == 8)
    assert(i == 48) //each vertex is explored 6 times: 3 faces, 2 edges per face.
    i = 0
    for (v <- TopoExplorerUnique.vertices(c)) {
      i = i + 1
    }
    assert(i == 8)
  }

}
