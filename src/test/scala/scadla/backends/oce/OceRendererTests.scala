package scadla.backends.oce

import scadla._
import scadla.InlineOps._
import scadla.backends.Viewer
import scadla.utils.CenteredCube
import scadla.utils.oce.ExtendedOps._
import scadla.utils.oce.{Fillet => _, Chamfer => _, _}
import org.scalatest._
import squants.space.Length
import scadla.EverythingIsIn.{millimeters, radians}
import org.jcae.opencascade.jni._

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

  test("rendering a sphere") {
    val r = new OceRenderer
    val obj = r(Sphere(1))
    def unitPoint(p: Point): Boolean = {
      (p.toVector.norm.toMillimeters - 1) <= 1e-3
    }
    assert(obj.faces.forall{ case Face(p0,p1,p2) => unitPoint(p1) && unitPoint(p2) && unitPoint(p0) })
  }

  def render(s: Solid, show: Boolean = false) {
    val r = new OceRenderer
    val obj = r(s)
    if (show) {
      Viewer.default(obj)
    }
  }

  test("test 01") {
    val tree = {
      val center = CenteredCube(3.2,3.2,3.2) * Sphere(2.0)
      val c = Translate(0,0,-3, Cylinder(1.0, 6.0))
      val carved = center - c - c.rotateX(math.Pi/2) - c.rotateY(math.Pi/2)
      Fillet.shape(carved, 0.1, s => for( w <- s.wires;
                                          l <- w.subLoops if l.c1Continuous;
                                          e <- l.edges ) yield e )
    }
    render(tree)
  }

  test("test 02") {
    val tree = Fillet(Cube(1,1,1), 0.2, (_: TopoDS_Edge) => true)
    render(tree)
  }

  test("test 03") {
    val tree = Chamfer(Cube(1,1,1), 0.2, (_: TopoDS_Face, _: TopoDS_Edge) => true)
    render(tree)
  }

  test("test 04") {
    val h = scadla.utils.extrusion.H(20, 20, 3)(100)
    val h1 = Fillet.shape(h, 1, s => {
        def checkAngles(e: TopoDS_Edge) = {
          implicit val tolerance: Length = 1e-3
          val fs = e.adjacentFacesIn(s)
          val n1 = fs.next.normal()
          val f2 = fs.next
          assert(!fs.hasNext)
          val p1 = e.start.asPoint + n1
          f2.pointOnFace(p1)
        }
        s.edges.filter(checkAngles)
      })
    render(h1)
  }

}
