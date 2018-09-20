package scadla.backends.oce

import scadla._
import scadla.InlineOps._
import scadla.backends.Viewer
import scadla.utils.CenteredCube
import scadla.utils.oce.ExtendedOps._
import scadla.utils.oce.{Fillet => _, Chamfer => _, Offset => _, _}
import org.scalatest._
import squants.space.{Length, Millimeters, SquareCentimeters}
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
    val shape = r.render(s)
    assert(shape.isValid)
    if (show) {
      val obj = r.toMesh(shape)
      Viewer.default(obj)
      r.toIGES(s, "test.igs")
      //r.toSTEP(s, "test.stp")
      scadla.backends.stl.Printer.storeBinary(obj, "test.stl")
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
    render(tree, false)
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

  test("test 05") {
    val tree = Fillet(Cube(1,1,1), 2, (_: TopoDS_Edge) => true)
    render(tree)
  }

  test("disjoint intersection") {
    val tree = CenteredCube(2,2,2) * Sphere(1.0).moveX(5.0)
    render(tree)
  }

  test("disjoint union") {
    val tree = CenteredCube(2,2,2) + Sphere(1.0).moveX(5.0)
    render(tree)
  }

  test("disjoint difference") {
    val tree = CenteredCube(2,2,2) - Sphere(1.0).moveX(5.0)
    render(tree)
  }

  test("releaux triangle") {
    val r = 10
    val s = Sphere(r)
    val tree = s.moveX(-r/2) * s.moveX(r/2) * s.move(0, r/2 * math.sqrt(3),0) * s.move(0, r/2 / math.sqrt(3), r/2 * math.sqrt(3))
    render(tree)
  }

  test("a more complex example") {
    implicit val tolerance = Millimeters(1e-3)
    //
    val drain = CenteredCube(8, 8, 10).rotateZ(math.Pi/4)
    val c1 = Offset(2, CenteredCube.xy(28, 36, 140)) + drain.moveY(-10) + drain.moveY(10)
    val c2 = Offset(2, CenteredCube.xy(38, 52, 140)) + drain.move(-8, -12, 0) + drain.move(-8, 12, 0) + drain.move(8, -12, 0) + drain.move(8, 12, 0)
    //
    val screwRadius = 2
    val screw = Cylinder(screwRadius, 15).rotateX(-math.Pi/2)
    val base = Cylinder(20, 120).moveY(-7) * Cube(40, 10, 120).moveX(-20) - screw.moveZ(25) - screw.moveZ(105)
    val unitY = Vector(0,1,0,Millimeters)
    val back = Chamfer(base, screwRadius, (face, edge) => face.normal().dot(unitY).to(SquareCentimeters) > 0 && edge.isClosed)
    //
    val plate = Offset( 5, Cube(80, 61, 1).moveX(-40))
    val overall = back.moveY(-5) + plate + plate.moveZ(119) - c1.move(22, 35, 0) - c2.move(-18, 33, 0)
    //
    render(overall, false)
  }

}
