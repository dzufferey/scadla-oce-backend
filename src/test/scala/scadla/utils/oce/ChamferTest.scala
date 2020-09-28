package scadla.utils.oce

import scadla.{Cube, Rotate}
import scadla.backends.Viewer
import scadla.backends.oce.OceRenderer
import scadla.utils.CenteredCube
import org.jcae.opencascade.jni._
import org.scalatest.funsuite.AnyFunSuite
import squants.space.Length
import scala.language.postfixOps
import squants.space.LengthConversions._
import scadla.EverythingIsIn.{millimeters, radians}
import ExtendedOps._

class ChamferTest extends AnyFunSuite {

  test("test 01") {
    val tree = Cube(1,1,1)
    val r = new OceRenderer
    val shape = r.render(tree).asInstanceOf[TopoDS_Solid]
    assert(shape.isValid)
    val shapeWithChamfer = Chamfer(shape, (face,edge) => Some(0.2 mm))
    assert(shapeWithChamfer.isValid)
    //val obj = r.toMesh(shapeWithChamfer)
    //Viewer.default(obj)
  }

  test("test 02") {
    val tree = Rotate(1, 1, 1, Cube(1,1,1))
    val r = new OceRenderer
    val shape = r.render(tree).asInstanceOf[TopoDS_Solid]
    assert(shape.isValid)
    val shapeWithChamfer = Chamfer(shape, (face,edge) => Some(0.2 mm))
    assert(shapeWithChamfer.isValid)
    //val obj = r.toMesh(shapeWithChamfer)
    //Viewer.default(obj)
  }

  test("test 03") {
    val tree = CenteredCube(1,1,3)
    val r = new OceRenderer
    val shape = r.render(tree).asInstanceOf[TopoDS_Solid]
    assert(shape.isValid)
    val shapeWithChamfer = Chamfer(shape, (face,edge) => Some(0.2 mm))
    assert(shapeWithChamfer.isValid)
    //val obj = r.toMesh(shapeWithChamfer)
    //Viewer.default(obj)
  }

}
