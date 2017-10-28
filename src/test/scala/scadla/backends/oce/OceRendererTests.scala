package scadla.backends.oce

import scadla._
import scadla.backends.Viewer
import scadla.utils.CenteredCube
import org.scalatest._
import squants.space.Length
import scadla.EverythingIsIn.{millimeters, radians}

class OceRendererTest extends FunSuite {

  test("rendering a cube") {
    val r = new OceRenderer
    val obj = r(Cube(1, 1, 1))
    assert(obj.faces.size == 12)
    def isUnit(l: Length) = {
      l.toMillimeters == 0.0 || l.toMillimeters == 1.0
    }
    def unitPoint(p: Point): Boolean = p match {
      case Point(x,y,z) => isUnit(x) && isUnit(y) && isUnit(z)
    }
    assert(obj.faces.forall{ case Face(p0,p1,p2) => unitPoint(p1) && unitPoint(p2) && unitPoint(p0) })
  }

  def renderAndShow(s: Solid) {
    val r = new OceRenderer
    val obj = r(s)
    Viewer.default(obj)
  }

/*
  test("test 01") {
    //val tree = Cube(1,1,1)
    //val tree = Sphere(4)
    //val tree = Cylinder(1.0, 5.0, 5.0)
    //val tree = Intersection(Sphere(5.0), Cylinder(1.0, 5.0, 5.0))
    val tree = {
      val c = Translate(0,0,-3, Cylinder(1.0, 6.0))
      Difference(
        Intersection(
            CenteredCube(5.0,5.0,5.0),
            Sphere(2.0)
        ),
        c,
        Rotate(math.Pi/2, 0, 0, c),
        Rotate(0, math.Pi/2, 0, c)
      )
    }
    renderAndShow(tree)
  }
*/
}
