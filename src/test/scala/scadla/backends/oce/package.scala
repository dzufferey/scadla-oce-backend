package scadla.backends

import scadla._
import scadla.utils.oce.ExtendedOps._
import squants.space._

package object oce {

  def parallel(v1: Vector, v2: Vector): Boolean = {
    v1.toUnitVector.dot(v2.toUnitVector) == Millimeters(1)*Millimeters(1) //TODO unit
  }

  def roundedCubeXY(x: Length, y: Length, z: Length, r: Length) = {
    Fillet(Cube(x,y,z), r, edge => parallel(edge.tangent(), Vector.z))
  }

  def render(s: Solid, show: Boolean = false): Unit = {
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

}
