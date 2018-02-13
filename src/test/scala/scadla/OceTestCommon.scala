package scadla

import scadla._
import org.jcae.opencascade.jni._
import squants.space.Length
import scala.language.postfixOps
import squants.space.LengthConversions._

object OceTestCommon {
  
  implicit val toleranceL = 1e-10 mm
  implicit val toleranceS = (1e-10 mm) * (1 mm)
  implicit val toleranceV = (1e-10 mm) * (1 mm) * (1 mm)
  
  def getVertex(x: Length, y: Length, z: Length) = {
    val a = Array[Double](x.toMillimeters, y.toMillimeters, z.toMillimeters)
    new BRepBuilderAPI_MakeVertex(a).shape().asInstanceOf[TopoDS_Vertex]
  }
  
  def getCube(x: Double = 5.0, y: Double = 10.0, z: Double = 20.0) = {
    val lowerLeft = Array[Double](0, 0, 0)
    val upperRight = Array[Double](x, y, z)
    new BRepPrimAPI_MakeBox(lowerLeft, upperRight).shape
  }

  def getSphere(radius: Double = 5.0) = {
    val origin = Array[Double](0, 0, 0)
    new BRepPrimAPI_MakeSphere(origin, radius).shape
  }

  def getCone(radius: Double = 2.0, height: Double = 5.0) = {
    val origin = Array[Double](0, 0, 0, 0, 0, 1)
    new BRepPrimAPI_MakeCone(origin, radius, 0, height, math.Pi*2).shape
  }

}
