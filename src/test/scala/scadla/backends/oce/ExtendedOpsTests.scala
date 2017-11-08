package scadla.backends.oce

import org.scalatest._
import scadla._
import org.jcae.opencascade.jni._
import ExtendedOps._
import squants.space.Length
import scadla.EverythingIsIn.{millimeters, radians}

class ExtendedOpsTest extends FunSuite {

  def getVertex(x: Length, y: Length, z: Length) = {
    val a = Array[Double](x.toMillimeters, y.toMillimeters, z.toMillimeters)
    new BRepBuilderAPI_MakeVertex(a).shape().asInstanceOf[TopoDS_Vertex]
  }
  
  test("vertex and points") {
    import scala.language.postfixOps
    import squants.space.LengthConversions._
    assert(getVertex(1,2,3).x == (1 mm))
    assert(getVertex(1,2,3).y == (2 mm))
    assert(getVertex(1,2,3).z == (3 mm))
    assert(getVertex(1,2,3).asPoint == Point(1,2,3))
  }

}
