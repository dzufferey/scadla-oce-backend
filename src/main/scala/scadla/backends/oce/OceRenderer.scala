package scadla.backends.oce

import scadla._
import scadla.backends.RendererAux
import squants.space.Length
import squants.space.Angle
import squants.space.Millimeters
import org.jcae.opencascade.jni._

class OceRenderer extends RendererAux[TopoDS_Shape] {

  var deviation = 2e-2

  def empty: TopoDS_Shape = {
      new BRepBuilderAPI_MakeSolid().shape()
  }

  def union(objs: Seq[TopoDS_Shape]): TopoDS_Shape = {
    if (objs.length == 0) {
      empty
    } else if (objs.length == 1) {
      objs.head
    } else {
      objs.reduceLeft( (x,y) => new BRepAlgoAPI_Fuse(x, y).shape() )
    }
  }

  def intersection(objs: Seq[TopoDS_Shape]): TopoDS_Shape = {
    if (objs.length == 0) {
      empty
    } else if (objs.length == 1) {
      objs.head
    } else {
      objs.reduceLeft( (x,y) => new BRepAlgoAPI_Common(x, y).shape() )
    }
  }

  def difference(pos: TopoDS_Shape, negs: Seq[TopoDS_Shape]): TopoDS_Shape = {
    negs.foldLeft(pos)( (p,n) => new BRepAlgoAPI_Cut(p, n).shape() )
  }

  def minkowski(objs: Seq[TopoDS_Shape]): TopoDS_Shape = {
    sys.error("oce backend does not support minkowski sum")
  }

  def hull(objs: Seq[TopoDS_Shape]): TopoDS_Shape = {
    sys.error("oce backend does not support convex hull")
  }

  def polyhedron(p: Polyhedron): TopoDS_Shape = {
    if (p.faces.isEmpty) {
      empty
    } else {
      //TODO identify the connected components (one solid per component)
      val (points, faces) = p.indexed 
      val vertices = points.map{ case Point(x,y,z) =>
        val a = Array[Double](x.toMillimeters, y.toMillimeters, z.toMillimeters)
        new BRepBuilderAPI_MakeVertex(a).shape().asInstanceOf[TopoDS_Vertex]
      }
      val edges = scala.collection.mutable.Map[(Int,Int), TopoDS_Edge]()
      def getEdge(i: Int, j: Int) = {
        if (edges contains (i -> j)) {
          edges(i -> j)
        } else {
          val b = new BRepBuilderAPI_MakeEdge(vertices(i), vertices(j))
          val e = b.shape.asInstanceOf[TopoDS_Edge]
          edges(i -> j) = e
          e
        }
      }
      val sewist = new BRepBuilderAPI_Sewing
      faces.foreach{ case (a,b,c) =>
        val e1 = getEdge(a,b)
        val e2 = getEdge(b,c)
        val e3 = getEdge(c,a)
        val w = new BRepBuilderAPI_MakeWire(e1, e2, e3).shape.asInstanceOf[TopoDS_Wire]
        val f = new BRepBuilderAPI_MakeFace(w).shape.asInstanceOf[TopoDS_Face]
        sewist.add(f)
      }
      sewist.perform
      val s = sewist.sewedShape
      s.shapeType match {
        case TopAbs_ShapeEnum.SHELL =>
          new BRepBuilderAPI_MakeSolid(s.asInstanceOf[TopoDS_Shell]).shape
        case TopAbs_ShapeEnum.SOLID | TopAbs_ShapeEnum.COMPSOLID | TopAbs_ShapeEnum.COMPOUND =>
          s
        case _ =>
          sys.error("unexpected shape:" + s)
      }
    }
  }

  def cube(width: Length, depth: Length, height: Length): TopoDS_Shape = {
    val lowerLeft = Array[Double](0.0, 0.0, 0.0)
    val upperRight = Array[Double](width.toMillimeters, depth.toMillimeters, height.toMillimeters)
    new BRepPrimAPI_MakeBox(lowerLeft, upperRight).shape
  }

  def sphere(radius: Length): TopoDS_Shape = {
    val origin = Array[Double](0.0, 0.0, 0.0)
    new BRepPrimAPI_MakeSphere(origin, radius.toMillimeters).shape
  }

  def cylinder(radiusBot: Length, radiusTop: Length, height: Length): TopoDS_Shape = {
    val rb = radiusBot.toMillimeters
    val rt = radiusTop.toMillimeters
    val h = height.toMillimeters
    assert(rb >= 0.0)
    assert(rt >= 0.0)
    assert(h >= 0.0)
    val origin = Array[Double](0,0,0, 0,0,1) // {X, Y, Z, directionX, directionY, directionZ}
    if (rb == 0.0 && rt == 0.0) {
      empty
    } else if (rb == rt) {
      new BRepPrimAPI_MakeCylinder(origin, rb, h, math.Pi*2).shape
    } else {
      new BRepPrimAPI_MakeCone(origin, rb, rt, h, math.Pi*2).shape
    }
  }

  def fromFile(path: String, format: String): TopoDS_Shape = format.toLowerCase match {
    case "stl" | "obj" | "amf" =>
      val poly = FromFile(path, format).load
      polyhedron(poly)
    case "brep" | "brp" =>
      BRepTools.read(path, new BRep_Builder())
    case "iges" | "igs" =>
      // https://dev.opencascade.org/doc/overview/html/occt_user_guides__iges.html
      val reader = new IGESControl_Reader
      reader.readFile(path.getBytes())
      reader.nbRootsForTransfer
      reader.transferRoots
      val result = reader.oneShape
      if (result == null) empty
      else result
    case "step" | "stp" =>
      // https://dev.opencascade.org/doc/overview/html/occt_user_guides__step.html
      val reader = new STEPControl_Reader
      reader.readFile(path.getBytes())
      reader.nbRootsForTransfer
      reader.transferRoots
      val result = reader.oneShape
      if (result == null) empty
      else result
    case _ => 
      sys.error("format '" + format + "' not supported")
  }

  def multiply(m: Matrix, obj: TopoDS_Shape): TopoDS_Shape = {
    val trsf = new GP_Trsf
    trsf.setValues(m.m00, m.m01, m.m02, m.m03,
                   m.m10, m.m11, m.m12, m.m13,
                   m.m20, m.m21, m.m22, m.m23)
    assert(m.m30 == 0.0 && m.m31 == 0.0 && m.m32 == 0.0 && m.m33 == 1.0)
    new BRepBuilderAPI_Transform(obj, trsf).shape
  }


  def toMesh(shape: TopoDS_Shape): Polyhedron = {
    BRepTools.clean(shape)
    val mesher = new BRepMesh_IncrementalMesh(shape, deviation)
    val explorer = new TopExp_Explorer(shape, TopAbs_ShapeEnum.FACE);
    val builder = Seq.newBuilder[Face]
    while(explorer.more) {
      val face = explorer.current.asInstanceOf[TopoDS_Face]
      val loc = new TopLoc_Location
      val tri = BRep_Tool.triangulation(face, loc)
      val n = tri.triangles.size / 3
      def getPoint(index: Int) = {
        Point(Millimeters(tri.nodes()(3*index)),
              Millimeters(tri.nodes()(3*index+1)),
              Millimeters(tri.nodes()(3*index+2)))
      }
      def getFace(index: Int) = {
        if (face.orientation == TopAbs_Orientation.FORWARD) {
          Face(getPoint(tri.triangles()(3*index  )),
               getPoint(tri.triangles()(3*index+1)),
               getPoint(tri.triangles()(3*index+2)))
        } else {
          Face(getPoint(tri.triangles()(3*index+1)),
               getPoint(tri.triangles()(3*index  )),
               getPoint(tri.triangles()(3*index+2)))
        }
      }
      var i = 0
      while (i < n) {
        builder += getFace(i)
        i += 1
      }
      explorer.next
    }
    Polyhedron(builder.result)
  }

  //TODO save brep, iges, step

}
