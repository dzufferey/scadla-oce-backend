package scadla.utils.oce

import scadla._
import scadla.backends.Viewer
import scadla.backends.oce.OceRenderer
import org.jcae.opencascade.jni._
import org.scalatest._
import scadla.EverythingIsIn.{millimeters, radians}
import squants.space.Millimeters
import ExtendedOps._

class PrismTest extends FunSuite {
  
  test("test 01") {
    val tree = Cube(1,1,1)
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    val shape2 = new Prism(shape, Vector(0, 0, 5, Millimeters)).result
    assert(shape2.isValid)
    //r.toIGES(shape2, "test.igs")
    //val obj = r.toMesh(shape2)
    //Viewer.default(obj)
  }

  test("test 02") {
    val tree = Cube(1,1,1)
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    val shape2 = new Prism(shape, Vector(2, 0, 5, Millimeters)).result
    assert(shape2.isValid)
    //r.toIGES(shape2, "test.igs")
    //val obj = r.toMesh(shape2)
    //Viewer.default(obj)
  }

  //TODO not the expected result ...
  test("test 03") {
    val tree = Difference(Cylinder(1,1), Cylinder(0.8,1))
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    val shape2 = new Prism(shape, Vector(2, 0, 5, Millimeters)).result
    assert(shape2.isValid)
    //r.toIGES(shape2, "test.igs")
    //val obj = r.toMesh(shape2)
    //Viewer.default(obj)
  }

  test("test 04") {
    val tree = Sphere(1)
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    val shape2 = new Prism(shape, Vector(0, 0, 5, Millimeters)).result
    assert(shape2.isValid)
    //r.toIGES(shape2, "test.igs")
    //val obj = r.toMesh(shape2)
    //Viewer.default(obj)
  }

}

class RevolutionTest extends FunSuite {

  //TODO not the expected result ...
  test("test 01") {
    val tree = Cube(1,1,1)
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    val shape2 = new Revolution(shape, Vector(0, 0, 5, Millimeters), 1.0).result
    assert(shape2.isValid)
    //r.toIGES(shape2, "test.igs")
    //val obj = r.toMesh(shape2)
    //Viewer.default(obj)
  }

}
