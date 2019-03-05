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

class OceRendererLongerTest extends FunSuite {

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
    val topDia: Length = 45.4
    val botDia: Length = 50
    val tr: (Length, Length, Angle) = (topDia, 50, 0.0)
    // Trapezoid is a polyhedron
    val fullTrap = Trapezoid(tr, tr, 71).move(-botDia/2, -botDia/2, 0)
    val trap = fullTrap * CenteredCube.xy(50, 50, 20)
    val tree = Intersection(
      Cylinder(botDia/2, topDia/2, 71) + trap + Cylinder(botDia/2 + 4, topDia/2, 8).moveZ(16) * fullTrap,
      Cube(botDia, botDia/2, 100).moveX(-botDia/2)
    )
    render(tree, false)
    //let us push that a bit further
    val shaftsDistance: Length = 52 //actually closer to 62
    val screw = Cylinder(2, 20) + Cylinder(4, 40).moveZ(20) // for mounting to the base M4
    val slotHeight: Length = 5
    val slot = (Cylinder(1.5,3) + Cylinder(1.5, 3).moveY(slotHeight) + CenteredCube.x(3, slotHeight, 3)).rotateX(Degrees(90)) //for mounting the motor (NEMA 17)
    val sideTriangle = Cube(2.4, 80, 80).rotateX(Degrees(-45)) * Cube(3, 50, 43 + slotHeight)
    val width: Length = 43 + 2 * 2.4
    val zGap: Length = 15
    val toMotorShaft: Length = 43.0 / 2 + 2
    val baseHeight: Length = shaftsDistance - toMotorShaft - zGap
    val base = Union(
      Cube(width, 50, baseHeight).moveZ(-baseHeight), //lower part
      Cube(width, 3, 43 + slotHeight).moveY(47), //backstop
      sideTriangle,
      sideTriangle.moveX(width-2.4)
    )
    val backScrewOffset = 2
    val tree2 = Difference(
      base.moveX(-width/2),
      tree.rotateX(Degrees(90)).rotateZ(Degrees(180)).moveZ(-baseHeight-zGap), //shape of part below
      screw.move(- 18, 12, - baseHeight + botDia/2 - zGap -20 + backScrewOffset), // back mounting screw TODO countersink?
      screw.move(+ 18, 12, - baseHeight + botDia/2 - zGap -20 + backScrewOffset), // back mounting screw TODO countersink?
      screw.moveZ(8).rotateY(Degrees( 45)).move(0, 40, -baseHeight-zGap), // front mounting screw
      screw.moveZ(8).rotateY(Degrees(-45)).move(0, 40, -baseHeight-zGap), // front mounting screw
      Cylinder(11.5, 5).rotateX(Degrees(90)).move(0, 50, 42/2), //motor flange 1
      Cube(23, 5, 50).move(-11.5, 47, 42/2), //motor flange 2
      slot.move(-31.0/2, 50, 5.5), //motor mounting screw
      slot.move( 31.0/2, 50, 5.5), //motor mounting screw
      slot.move(-31.0/2, 50, 42 - 5.5), //motor mounting screw
      slot.move( 31.0/2, 50, 42 - 5.5), //motor mounting screw
      Empty
    )
    render(tree2, false)
  }

}
