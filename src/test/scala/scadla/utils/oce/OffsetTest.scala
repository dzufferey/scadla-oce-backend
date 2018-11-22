package scadla.utils.oce

import scadla._
import scadla.backends.Viewer
import scadla.backends.oce.OceRenderer
import scadla.utils.CenteredCube
import org.jcae.opencascade.jni._
import org.scalatest._
import squants.space.Length
import scala.language.postfixOps
import squants.space.LengthConversions._
import scadla.EverythingIsIn.{millimeters, radians}
import ExtendedOps._
  
class OffsetTest extends FunSuite {
  
  test("test 00") {
    val tree = scadla.Offset(0.0, Union(
      CenteredCube(3,3,3),
      Translate(1.5, 0, 0, Sphere(1.0))
    ))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 01a") {
    val tree = scadla.Offset(0.3, Cube(3,3,3))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 01b") {
    val tree = scadla.Offset(0.3, CenteredCube(3,3,3))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 01c") {
    val tree = scadla.Offset(0.3, Sphere(1))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 01d") {
    val tree = scadla.Offset(0.3, Translate(1.5, 0, 0, Sphere(1.0)))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 01e") {
    val tree = scadla.Offset(0.3, Union(
      CenteredCube(1.5,1.5,1.5),
      Sphere(1.0)
    ))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 01f") {
    val tree = Union(
      scadla.Offset(0.3, CenteredCube(1.5,1.5,1.5)),
      scadla.Offset(0.3, Sphere(1.0))
    )
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 01g") {
    val tree = scadla.Offset(0.3, Intersection(
      CenteredCube(3,3,3),
      Translate(1.5, 0, 0, Sphere(1.0))
    ))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 01h") {
    val tree = scadla.Offset(0.3, Difference(
      CenteredCube(3,3,3),
      Translate(1.5, 0, 0, Sphere(1.0))
    ))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 01") {
    val tree = scadla.Offset(0.3, Union(
      CenteredCube(3,3,3),
      Translate(1.5, 0, 0, Sphere(1.0))
    ))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 02") {
    val tree = scadla.Offset(-0.3, Union(
      CenteredCube(3,3,3),
      Translate(1.5, 0, 0, Sphere(1.0))
    ))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 02a") {
    val tree = scadla.Offset(-0.3, Intersection(
      CenteredCube(3,3,3),
      Translate(1.5, 0, 0, Sphere(1.0))
    ))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 02b") {
    val tree = scadla.Offset(-0.3, Difference(
      CenteredCube(3,3,3),
      Translate(1.5, 0, 0, Sphere(1.0))
    ))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

  test("test 03") {
    val tree = scadla.ThickSolid(Cube(3,3,3), 0.1, _.faces.take(2))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    //val obj = r.toMesh(shape)
    //Viewer.default(obj)
  }

}
