package scadla.backends.oce

import org.scalatest._
import scadla._
import org.jcae.opencascade.jni._
import ExtendedOps._
import squants.space.Length
import scala.language.postfixOps
import squants.space.LengthConversions._
import scadla.EverythingIsIn.{millimeters, radians}
import TestCommon._

class ExtendedOpsTest extends FunSuite {

  test("vertex and points") {
    assert(getVertex(1,2,3).x == (1 mm))
    assert(getVertex(1,2,3).y == (2 mm))
    assert(getVertex(1,2,3).z == (3 mm))
    assert(getVertex(1,2,3).asPoint == Point(1,2,3))
  }

  test("edges length") {
    val cone = getCone()
    for (e <- TopoExplorer.edges(cone)) {
      val l = e.length
      assert(l ≈ (0 mm) ||
             l ≈ (5.385164807135 mm) ||
             l ≈ (12.566370614359172 mm))
    }
  }

  test("volume and surface") {
    val box = getCube()
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

  test("edge: isClosed") {
    val box = getCube()
    for (e <- TopoExplorer.edges(box)) {
      assert(!e.isClosed)
    }
    val sphere = getSphere()
    for (e <- TopoExplorer.edges(sphere)) {
      assert(e.isDegenerate || e.isClosed)
    }
    val cone = getCone()
    var open = 0
    var closed = 0
    var degenerate = 0
    for (e <- TopoExplorerUnique.edges(cone)) {
      if (e.isDegenerate) {
        degenerate += 1
      } else if (e.isClosed) {
        closed += 1
      } else {
        open += 1
      }
    }
    assert(degenerate == 1)
    assert(closed == 1)
    assert(open == 1)
  }

  test("face: isPlane") {
    val box = getCube()
    for (e <- TopoExplorer.faces(box)) {
      assert(e.isPlane)
    }
    val sphere = getSphere()
    for (e <- TopoExplorer.faces(sphere)) {
      assert(!e.isPlane)
    }
  }

  test("face: normal, umbilic, curvature") {
    val box = getCube()
    for (e <- TopoExplorerUnique.faces(box)) {
      assert(e.isUmbilic)
      assert(e.isCurvatureDefined())
      assert(e.meanCurvature() ≈ (0 mm))
      assert(e.gaussianCurvature() ≈ (0 mm))
      assert(e.normal() == Vector.x ||
             e.normal() == Vector.y ||
             e.normal() == Vector.z )
    }
    val cone = getCone(1, 1)
    for (e <- TopoExplorerUnique.faces(cone)) {
      assert(e.isCurvatureDefined())
      assert(e.gaussianCurvature() ≈ (0 mm))
      if (e.isUmbilic) {
        assert(e.meanCurvature() ≈ (0 mm))
        assert(e.normal() == Vector.z)
      } else {
        val n = e.normal()
        assert(n.x ≈ (-math.sqrt(2)/2 mm))
        assert(n.y ≈ (0 mm))
        assert(n.z ≈ (math.sqrt(2)/2 mm))
      }
    }
    val sphere = getSphere()
    for (e <- TopoExplorer.faces(sphere)) {
      assert(e.isUmbilic)
      assert(e.isCurvatureDefined())
      assert(e.meanCurvature() ≈ (-1.0/5 mm))
      assert(e.gaussianCurvature() ≈ (1.0/25 mm))
      for (u <- Seq(0.0, 0.5, 1.0);
           v <- Seq(0.0, 0.5, 1.0)) {
        val n = e.normal(u, v)
        assert(n.x ≈ (-1 mm) || n.x ≈ (0 mm) || n.x ≈ (1 mm))
        assert(n.y ≈ (-1 mm) || n.y ≈ (0 mm) || n.y ≈ (1 mm))
        assert(n.z ≈ (-1 mm) || n.z ≈ (0 mm) || n.z ≈ (1 mm))
      }
    }
  }

}
