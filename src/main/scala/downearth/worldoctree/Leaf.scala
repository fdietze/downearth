package downearth.worldoctree

import downearth._
import downearth.util._
import downearth.generation.{MaterialManager, WorldDefinition}
import downearth.world.World

import simplex3d.math.{Vec2i, Vec3i}
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import downearth.rendering.{Update, TextureMeshBuilder, ObjMesh}


/**
 * User: arne
 * Date: 27.05.13
 * Time: 00:58
 */

// TODO: eventuell Leaf von Polyeder erben lassen um eine Refernz zu sparen.
class Leaf(val h:Polyeder) extends NodeUnderMesh {
  override def hasChildren = false

  override def getChild(i:Int) = throw new NoSuchElementException("a leaf in a tree doesn't have children")
  // kann kein deadNode sein
  def isSet(info:NodeInfo,pos:NodeInfo) = true

  def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:Node) = insertnode

  override def apply(info:NodeInfo, p:Vec3i) = this

  override def updated(info:NodeInfo, p:Vec3i, newLeaf:Leaf) = {
    if(h == newLeaf.h)
      this
    else{
      // wenn das Blatt einen größeren Bereich abdeckt der voll,
      // bzw leer ist:
      if(info.size >= 2) {
        val replacement = new InnerNode(this)
        replacement.updated(info, p, newLeaf)
      }
      else {
        newLeaf
      }
    }
  }

  override def toString = h.toString

  override def equals(that:Any) = {
    that match {
      case l:Leaf =>
        h == l.h
      case _ =>
        false
    }
  }

  // erzeugt aus Zwei aneinender grenzenden Hexaedern die Polygone, die nicht verdeckt werden.
  // performance kritischer bereich, weil es für jedes benachberte Hexaederpaar aufgerufen wird
  def addSurface(from:Polyeder, to:Polyeder, pos:Vec3i, dir:Int ,meshBuilder:TextureMeshBuilder) = {
    if(to != UndefHexaeder) {
      assert(from != EmptyHexaeder)

      import meshBuilder._

      val axis = dir >> 1
      val direction = dir & 1

      //die beiden achsesen, die nicht axis sind
      val axisa = 1-((axis+1) >> 1)
      val axisb = (2 - (axis >> 1))

      var vertexCounter = 0

      // val Vector(t0,t1,t2,t3,t4,t5) = from.planetriangles(axis, direction)
      val occluderVertices = to.planetriangles(axis,1-direction)
      val occludingCoords = new collection.mutable.ArrayBuffer[Vec2](6) // es sind nie mehr als 6 Vertices

      for(ov ← occluderVertices){
        if( ov(axis) == 1-direction )
          occludingCoords += Vec2(ov(axisa),ov(axisb))
      }

      @inline def triangleMax(v0:Vec3, v1:Vec3, v2:Vec3) = {
        (v0(axis) == direction) && (v1(axis) == direction) && (v2(axis) == direction)
      }

      @inline def addVertices(v0:Vec3, v1:Vec3, v2:Vec3) {
        val matid = if( material >= 0 ) material else WorldDefinition.materialAtBlock(pos).id
        val matCount = MaterialManager.materialCount.toDouble

        vertexBuilder += (Vec3(pos) + v0)
        texCoordBuilder += Vec2( v0(axisa)/matCount + matid/matCount , v0(axisb) )

        vertexBuilder += (Vec3(pos) + v1)
        texCoordBuilder += Vec2( v1(axisa)/matCount + matid/matCount , v1(axisb) )

        vertexBuilder += (Vec3(pos) + v2)
        texCoordBuilder += Vec2( v2(axisa)/matCount + matid/matCount , v2(axisb) )

        vertexCounter += 3

        normalBuilder += normalize(cross(v2-v1,v0-v1))
      }

      val t = from.planetriangles(axis, direction)

      for( i ← 0 to 3 by 3) {
        val t0 = t(0+i)
        val t1 = t(1+i)
        val t2 = t(2+i)

        // liegen zwei vertices eines polygons zusammen, hat das polygon keine oberfläche und muss nicht
        // gezeichnet werden
        if( t0 != t1 && t1 != t2 && t0 != t2 ){
          if( to == EmptyHexaeder || !triangleMax(t0,t1,t2) )
            addVertices(t0, t1, t2)
          else{
            val v0 = Vec2( t0(axisa), t0(axisb) )
            val v1 = Vec2( t1(axisa), t1(axisb) )
            val v2 = Vec2( t2(axisa), t2(axisb) )
            val flatTriangle = Vector(v0,v1,v2)
            if( !occludes2d(occludee=flatTriangle,occluder=occludingCoords) ) {
              addVertices(t0, t1, t2)
            }
          }
        }
      }

      vertexCounter
    }
    else
      0
  }

  override def genPolygons(info:NodeInfo, meshBuilder:TextureMeshBuilder,worldaccess:(Vec3i => Polyeder)):Int = {
    import info.{pos => nodepos, size => nodesize}
    assert(meshBuilder != null)
    var vertexCounter = 0

    if(nodesize == 1) {
      if( h != EmptyHexaeder ){
        for( i <- (0 to 5) ){
          val p2 = nodepos.clone
          p2(i >> 1) += ((i&1)<<1)-1

          val to = worldaccess(p2)

          vertexCounter += addSurface(h,to,info.pos,i,meshBuilder)
        }
      }
    }
    else {
      if(h == FullHexaeder) {
        for( dir <- (0 to 5) ) {
          val axis = dir >> 1

          val axisa = 1-((axis+1) >> 1)
          val axisb = (2 - (axis >> 1))

          //TODO: Oberfläche eines Octanten als Quadtree abfragen

          for( spos <- Vec2i(0) until Vec2i(info.size) ){
            val p1 = nodepos.clone
            p1( axisa ) += spos(0)
            p1( axisb ) += spos(1)
            p1( axis )  += (nodesize-1) * (dir&1)
            val p2 = p1.clone
            p2( axis ) += ((dir & 1) << 1)-1
            val other = worldaccess(p2)

            vertexCounter += addSurface(h,other,p1,dir,meshBuilder)
          }
        }
      }
    }
    vertexCounter
  }

  override def patchWorld(info:NodeInfo, p:Vec3i, newLeaf:Leaf, vertpos:Int, vertcount:Int) : (NodeUnderMesh, Update) = {
    val replacement = updated(info, p, newLeaf)

    val builder = new TextureMeshBuilder
    replacement.genPolygons(info,builder, v => World.octree(v).h )
    val update = Update(vertpos,vertcount,builder.result)
    (replacement,update)
  }

  override def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int) : Update = {
    val builder = new TextureMeshBuilder
    genPolygons(info, builder, v => World.octree(v).h )
    Update(vertpos,vertcount,builder.result)
  }

  override def genMesh(info:NodeInfo, dstnodesize: Int, worldaccess:(Vec3i => Polyeder) ):NodeOverMesh = {
    (new MeshNode(this)).genMesh(info,dstnodesize,worldaccess)
  }

  def draw(info:NodeInfo,test:FrustumTest){}

  override def getPolygons( info:NodeInfo, pos:Vec3i, from:Int, to:Int): (Int,Int) = {
    (from,to)
  }

  def material = -1
}


// Leaf that is set by a user, contains a material
class UserLeaf(_h:Polyeder, m:Byte) extends Leaf(_h) {
  require( _h != null )
  override def material:Int = m

  override def equals(that:Any) = {
    that match {
      case l:UserLeaf =>
        h == l.h && material == l.material
      case _ =>
        false
    }
  }
}

object Leaf {
  def apply(h:Polyeder) = {
    h match{
      case EmptyHexaeder => EmptyLeaf
      case FullHexaeder => FullLeaf
      case _ => new Leaf(h)
    }
  }

  def apply(h:Polyeder, material:Int) = {
    if(h eq EmptyHexaeder)
      EmptyLeaf
    else
      new UserLeaf(h, material.toByte )
  }
}

case object EmptyLeaf extends Leaf(EmptyHexaeder) {
  def fill(info:NodeInfo, func: (Vec3i) => Leaf ) : NodeUnderMesh = {
    if(info.size >= 2){

      val array = new Array[NodeUnderMesh](8)

      var i = 0
      while( i < 8 ) {
        array(i) = EmptyLeaf.fill(info(i),func)
        i += 1
      }
      new InnerNode(array).merge
    }
    else {
      func(info.pos)
    }
  }
}

case object FullLeaf extends Leaf(FullHexaeder)

class ObjLeaf(val mesh:ObjMesh) extends Leaf(EmptyHexaeder)
