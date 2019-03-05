package scadla.backends

import scadla._
import scadla.utils.oce.ExtendedOps._

package object oce {

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

}
