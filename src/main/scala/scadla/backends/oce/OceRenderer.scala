package scadla.backends.oce

import scadla._
import scadla.utils.oce.TopoExplorerUnique
import scadla.backends.RendererAux
import squants.space.Length
import squants.space.Angle
import squants.space.Millimeters
import org.jcae.opencascade.jni._

class OceRenderer extends RendererAux[TopoDS_Shape] {

  var deviation = 2e-2

  override def isSupported(s: Solid): Boolean = s match {
    case OceShape(_) => true
    case s: Shape => super.isSupported(s)
    case t: Transform => super.isSupported(t)
    case OceOperation(s, _) => isSupported(s)
    case o: Operation => super.isSupported(o)
    case _ => false
  }

  def shape(s: Shape): TopoDS_Shape = s match {
    case Empty => empty
    case Cube(x,y,z) => cube(x,y,z)
    case Sphere(r) => sphere(r)
    case Cylinder(top, bot, height) => cylinder(top, bot, height)
    case p @ Polyhedron(_) => polyhedron(p)
    case FromFile(name, format) => fromFile(name, format)
    case OceShape(s) => s
    case s => sys.error("oce backend does not support " + s)
  }

  def empty = new BRepBuilderAPI_MakeSolid().shape()

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

  def operation(o: Operation, args: Seq[TopoDS_Shape]): TopoDS_Shape = o match {
    case _: Union => union(args)
    case _: Intersection => intersection(args)
    case _: Difference => difference(args.head, args.tail)
    case OceOperation(_, op) => op(args.head)
    case o => sys.error("oce backend does not support " + o)
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


  def transform(t: Transform, obj: TopoDS_Shape) = {
    val m = t.matrix
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
    val builder = Seq.newBuilder[Face]
    val explorer = TopoExplorerUnique.faces(shape)
    while(explorer.hasNext) {
      val face = explorer.next
      val loc = new TopLoc_Location
      val tri = BRep_Tool.triangulation(face, loc)
      val nodes = tri.nodes
      val nPnt = nodes.size / 3
      var i = 0
      val tmp = Array.ofDim[Double](3)
      val pnt = Array.ofDim[Point](nPnt)
      while (i < nPnt) {
        tmp(0) = nodes(3*i)
        tmp(1) = nodes(3*i+1)
        tmp(2) = nodes(3*i+2)
        val trf = loc.transformation
        trf.transforms(tmp)
        pnt(i) = Point(Millimeters(tmp(0)), Millimeters(tmp(1)), Millimeters(tmp(2)))
        i += 1
      }
      def getFace(index: Int) = {
        if (face.orientation == TopAbs_Orientation.FORWARD) {
          Face(pnt(tri.triangles()(3*index  )),
               pnt(tri.triangles()(3*index+1)),
               pnt(tri.triangles()(3*index+2)))
        } else {
          Face(pnt(tri.triangles()(3*index+1)),
               pnt(tri.triangles()(3*index  )),
               pnt(tri.triangles()(3*index+2)))
        }
      }
      val triangles = tri.triangles
      val nTri = triangles.size / 3
      i = 0
      while (i < nTri) {
        builder += getFace(i)
        i += 1
      }
    }
    Polyhedron(builder.result)
  }

  //TODO save brep, iges, step

}
