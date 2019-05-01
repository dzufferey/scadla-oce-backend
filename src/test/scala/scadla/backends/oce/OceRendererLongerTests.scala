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

// some dogfooding

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
  
  test("a more complex example 3") {
    import scadla.utils.extrusion._
    val alpha: Angle = Degrees(15)
    val beta: Angle = Degrees(10)
    val thickness: Length = 1
    val side: Length = 25
    val side2: Length = side + 5
    val cornerRadius: Length = 3
    val extra = 10
    //
    val l1 = Fillet(L(side+extra, side+extra, thickness+extra)(100), cornerRadius, edge => {
        val (start, end) = edge.extremities
        start.x == thickness+extra && start.y == thickness+extra && end.x == thickness+extra && end.y == thickness+extra
      })
    val l2 = L(side*2+extra, side*2+extra, thickness+extra)(100).move(-thickness, -thickness, -20)
    val screw = Cylinder(1.7, 12)
    val ls = Union(
        l1,
        l2,
        screw.rotateX(Degrees(-90)).move(extra+side-10,extra,10),
        screw.rotateY(Degrees(90)).move(extra,extra+side-10,10)
      )
    val base = Difference(
        Cube(side2, side2, side),
        ls.move(-extra, -extra, side*alpha.sin + 3).rotate(-alpha, beta, 0.0),
        screw.move(side2/2, side2/2, 0) //bottom screw
      )
    val finished = Fillet(base, cornerRadius, edge => {
      val zero: Length = 0
      val s = edge.start
      val e = edge.end
      (s.x == zero && s.y == zero && edge.end.z > zero) || (s.x == s.y && e.x == e.y)
    })
    val all = finished + finished.mirror(1, 0, 0).moveX(-5)
    render(all, false)
    //part 2
    val screwRadius = 1.4
    val size = 21.3
    val delta = screwRadius + 0.1
    val roundedCube = roundedCubeXY(size,size,size+1, 2.5)
    val s = Cylinder(screwRadius, size+2).moveZ(-size/2-1)
    val ls2 = Union(
        l1,
        s.rotateX(math.Pi/2).move(extra + size/2 + thickness, extra + size/2 + thickness, 4+delta),
        s.rotateX(math.Pi/2).move(extra + size/2 + thickness, extra + size/2 + thickness, 13+delta),
        s.rotateY(math.Pi/2).move(extra + size/2 + thickness, extra + size/2 + thickness, 4-delta),
        s.rotateY(math.Pi/2).move(extra + size/2 + thickness, extra + size/2 + thickness, 13-delta)
      )
    val otherSide = Difference(
        roundedCube,
        ls2.move(-extra+2.5, -extra+4.5, 1).rotate(alpha, -beta, 0.0)
      )
    render(otherSide + otherSide.mirror(1, 0, 0).moveX(-5), false)
  }

  test("a more complex example 4") {
    val rodRadius: Length = 4
    val pillarSide: Length = 70
    val α = Degrees(50)
    val β = Degrees(-10)
    val rods = {
      val r = Cylinder(rodRadius, pillarSide).moveZ(rodRadius).rotateY(Degrees(90))
      val rs = r.rotateZ(α) + r.rotateZ(β).moveY(-rodRadius)
      rs + rs.mirror(1,0,0)
    }
    val side = pillarSide + 10
    val base = Difference(
      roundedCubeXY(side, side, 40, 3).move(-side/2, -side/2, 0),
      roundedCubeXY(pillarSide, pillarSide, 50, 3).move(-pillarSide/2, -pillarSide/2, 12),
      rods.moveZ(2+rodRadius)
    )
    val h: Length = 3
    def findEdge(e: TopoDS_Edge): Boolean = {
      val k = e.kind == GeomAbs_CurveType.GeomAbs_Line
      val p = parallel(e.tangent(), Vector.z)
      val x = ( (e.start.x.to(Millimeters) - 5).abs <= 1e-5 ||
                (e.start.x.to(Millimeters) + 5).abs <= 1e-5 )
      k && p && x
    }
    val screwStuff = {
      val base = Cube(10, 20, h).move(-5,-20,0) + Cube(20, 5, h).move(-10,-25,0) + Cylinder(6, h) - Cylinder(2, h)
      val chamfered = Chamfer.edge(base, 2, e => e.kind == GeomAbs_CurveType.GeomAbs_Circle && (e.start.asPoint - Point(0,0,0)).norm == Millimeters(2))
      val filleted = Fillet(chamfered, 4, findEdge)
      filleted.moveY(20).rotateX(Degrees(90))
    }
    val obj = base ++ (0 to 3).map( i => screwStuff.move(0, pillarSide/2 + h, 40).rotateZ(Degrees(90)*i) )
    render(obj, false)
    val pad = {
      val body = Fillet(Cylinder(10, 20), 5, e => e.kind == GeomAbs_CurveType.GeomAbs_Circle)
      val oridented = body.rotateX(Degrees(90)).moveZ(2)
      val cut = oridented * CenteredCube.xy(50, 50, 50)
      cut - Cylinder(rodRadius, 15).rotateX(Degrees(90)).moveZ(rodRadius + 2)
    }
    render(pad, true)
  }

}
