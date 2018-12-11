package scadla.backends.oce

import scadla._
import scadla.InlineOps._
import scadla.backends.Viewer
import scadla.utils.{CenteredCube, Trapezoid}
import scadla.utils.oce.ExtendedOps._
import scadla.utils.oce.{Fillet => _, Chamfer => _, Offset => _, _}
import org.scalatest._
import squants.space.{Angle, Length, Degrees, Millimeters, SquareCentimeters}
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

  test("a more complex example 1") {
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

  test("a more complex example 2") {
    implicit val tolerance = Millimeters(1e-3)
    //
    val x = 350
    val y = 250
    val z1 = 3
    val z2 = 5
    val gap = 3
    val border = 5
    val radius = 2
    val splineWidth = 2 * radius + 1
    val splineGap = splineWidth + 6
    val angle = math.Pi / 4
    //
    val tb = Cube(x, border, z2)
    val lr = Cube(border, y, z2)
    val frame1 = tb + tb.moveY(y - border) + lr + lr.moveX(x - border) + Cube(x, y, z1)
    //val frame = Fillet(frame1, radius/2, edge => edge.point(0.5).z >= z2) //FIXME makes the JVM segfault!!!
    def isCorner(edge: TopoDS_Edge) = {
      val p = edge.point(0.5)
      (p.x <= 0 || p.x >= x) && (p.y <= 0 || p.y >= y)
    }
    /*
    def isCorner2(edge: TopoDS_Edge) = {
      val p = edge.point(0.5)
      (p.x <= border/2 || p.x >= x-border/2) && (p.y <= border/2 || p.y >= y-border/2) && p.z >= z2
    }
    */
    val frame = Fillet(frame1, 2*radius, isCorner)
    //val frame = Fillet(frame2, radius, isCorner2)
    //
    val splineArea = Cube(x - 2*border - 2*gap, y - 2*border - 2*gap, z2).move(border + gap, border + gap, 0)
    val spline = Cube(x / math.cos(angle), splineWidth, z2).rotateZ(-angle)
    var splines: List[Solid] = Nil
    var i = border + gap + splineWidth - 3 //XXX fiddle around to avoid the error during the fillet, we are still loosing two of them ...
    val yMax = y + x / math.tan(angle) - splineWidth
    while (i <= yMax) {
      val s = spline.moveY(i) * splineArea
      val f = Fillet(s, radius, edge => edge.point(0.5).z >= z2)
      splines ::= f
      i += splineGap
    }
    //
    val overall = frame ++ splines
    //val r = new OceRenderer
    //val shape = r.render(overall)
    //val vol = shape.solids.map(_.volume()).reduce(_ + _)
    //Console.println("volume: " + vol)
    //render(overall, true)
    // negative
    val box = Cube(x + 20, y + 20, z2 + 1).move(-10, -10, 0)
    val diff = box - overall
    render(diff, false)
  }

  //does not result in the cleanest BRep but it works
  test("something with polyhedron") {
    val tx: (Length, Length, Angle) = (46, 50, 0.0)
    val ty: (Length, Length, Angle) = (46, 50, 0.0)
    // Trapezoid is a polyhedron
    val trap = (Trapezoid(tx, ty, 71) * Cube(50, 50, 20)).move(-25, -25, 0)
    val tree = Intersection(
      Cylinder(25, 23, 71) + trap,
      Cube(50, 25, 100).moveX(-25)
    )
    render(tree, false)
    //let us push that a bit further
    val screw = Cylinder(2, 20) + Cylinder(4, 20).moveZ(20) // for mounting to the base M4
    val slotHeight = 5
    val slot = (Cylinder(1.5,3) + Cylinder(1.5, 3).moveY(slotHeight) + CenteredCube.x(3, slotHeight, 3)).rotateX(Degrees(90)) //for mounting the motor (NEMA 17)
    val sideTriangle = Cube(2.4, 80, 80).rotateX(Degrees(-45)) * Cube(3, 50, 43 + slotHeight)
    val width = 43 + 2 * 2.4
    val base = Union(
      Cube(width, 50, 15), //lower part
      Cube(width, 3, 43 + slotHeight).move(0, 47, 15), //backstop
      sideTriangle.moveZ(15),
      sideTriangle.move(width-2.4, 0, 15)
    )
    val tree2 = Difference(
      base.moveX(-width/2),
      tree.rotateX(Degrees(90)).rotateZ(Degrees(180)).moveZ(-13), //shape of part below
      screw.move(- 18, 12, -5), // back mounting screw
      screw.move(+ 18, 12, -5), // back mounting screw
      screw.moveZ(8).rotateY(Degrees( 45)).move(0, 40, -15), // front mounting screw
      screw.moveZ(8).rotateY(Degrees(-45)).move(0, 40, -15), // front mounting screw
      Cylinder(11.5, 5).rotateX(Degrees(90)).move(0, 50, 15+42/2), //motor flange 1
      Cube(23, 5, 50).move(-11.5, 47, 15+42/2), //motor flange 2
      slot.move(-31/2, 50, 15 + 5.5), //motor mounting screw
      slot.move( 31/2, 50, 15 + 5.5), //motor mounting screw
      slot.move(-31/2, 50, 15 + 42 - 5.5), //motor mounting screw
      slot.move( 31/2, 50, 15 + 42 - 5.5), //motor mounting screw
      Empty
    )
    render(tree2, false)
  }

}
