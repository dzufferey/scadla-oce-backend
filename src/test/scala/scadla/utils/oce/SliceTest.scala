package scadla.utils.oce

import scadla._
import scadla.backends.Viewer
import scadla.backends.oce.OceRenderer
import org.jcae.opencascade.jni._
import org.jcae.opencascade.Utilities
import org.scalatest.funsuite.AnyFunSuite
import scadla.EverythingIsIn.{millimeters, radians}
import squants.space.Millimeters
import ExtendedOps._

class SliceTest extends AnyFunSuite {
  
  test("test 01") {
    val tree = Cube(1,1,1)
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    for (shape2 <- Slice(shape, Point(0, 0, 0.5), Vector(0, 0, 1, Millimeters))) {
      assert(shape2.isValid)
      //Utilities.dumpTopology(shape2, Console.out)
    }
  }
  
  test("test 02") {
    val tree = Cube(1,1,1)
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    for (shape2 <- Slice(shape, Point(0, 0, 1), Vector(0, 0, 1, Millimeters))) {
      assert(shape2.isValid)
      //Utilities.dumpTopology(shape2, Console.out)
    }
  }

  test("test 03") {
    val tree = Cylinder(1,1)
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    for (shape2 <- Slice(shape, Point(0, 0, 0.5), Vector(0, 0, 1, Millimeters))) {
      assert(shape2.isValid)
      //Utilities.dumpTopology(shape2, Console.out)
    }
  }

  test("test 04") {
    val tree = Cylinder(1,1)
    val r = new OceRenderer
    val shape = r.render(tree)
    assert(shape.isValid)
    for (shape2 <- Slice(shape, Point(0, 0, 0), Vector(0, 0, 1, Millimeters))) {
      assert(shape2.isValid)
      //Utilities.dumpTopology(shape2, Console.out)
    }
  }

}
