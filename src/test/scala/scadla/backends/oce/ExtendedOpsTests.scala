package scadla.backends.oce

import org.scalatest._
import scadla._
import org.jcae.opencascade.jni._
import ExtendedOps._
import squants.space.Length
import scala.language.postfixOps
import squants.space.LengthConversions._
import scadla.EverythingIsIn.{millimeters, radians}

class ExtendedOpsTest extends FunSuite {

  def getVertex(x: Length, y: Length, z: Length) = {
    val a = Array[Double](x.toMillimeters, y.toMillimeters, z.toMillimeters)
    new BRepBuilderAPI_MakeVertex(a).shape().asInstanceOf[TopoDS_Vertex]
  }

  implicit val toleranceL = 1e-10 mm
  implicit val toleranceS = (1e-10 mm) * (1 mm)
  implicit val toleranceV = (1e-10 mm) * (1 mm) * (1 mm)
  
  test("vertex and points") {
    assert(getVertex(1,2,3).x == (1 mm))
    assert(getVertex(1,2,3).y == (2 mm))
    assert(getVertex(1,2,3).z == (3 mm))
    assert(getVertex(1,2,3).asPoint == Point(1,2,3))
  }

  test("edges length") {
    val origin = Array[Double](0, 0, 0, 0, 0, 1)
    val cone = new BRepPrimAPI_MakeCone(origin, 2, 0, 5, math.Pi*2).shape
    for (e <- TopoExplorer.edges(cone)) {
      val l = e.length
      assert(l ≈ (0 mm) ||
             l ≈ (5.385164807135 mm) ||
             l ≈ (12.566370614359172 mm))
    }
  }

  test("volume and surface") {
    val origin = Array[Double](0, 0, 0)
    val upperRight = Array[Double](5, 10, 20)
    val box = new BRepPrimAPI_MakeBox(origin, upperRight).shape
    for (f <- TopoExplorer.faces(box)) {
      val a = f.area
      assert(a ≈ ((5 mm) * (10 mm)) ||
             a ≈ ((5 mm) * (20 mm)) ||
             a ≈ ((10 mm) * (20 mm)) )
    }
    for (s <- TopoExplorer.shells(box)) {
      val a = s.area
      val expected = ((5 mm) * (10 mm) + (5 mm) * (20 mm) + (10 mm) * (20 mm)) * 2
      assert(a ≈ expected)
    }
    for (s <- TopoExplorer.solids(box)) {
      val v = s.volume
      assert(v ≈ ((5 mm) * (10 mm) * (20 mm)))
    }
  }

}
