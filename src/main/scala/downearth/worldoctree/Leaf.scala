package downearth.worldoctree

import downearth._
import downearth.util._
import downearth.generation.{WorldFunction, WorldDefinition}

import simplex3d.math.{ReadVec3i, Vec2i, Vec3i}
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import downearth.rendering._
import downearth.resources.MaterialManager
import simplex3d.math.floatx.{Vec2f, Vec3f}
import downearth.rendering.Update
import downearth.rendering.UpdateInfo
import downearth.rendering.TextureMeshBuilder


/**
 * User: arne
 * Date: 27.05.13
 * Time: 00:58
 */

// TODO: eventuell Leaf von Polyeder erben lassen um eine Refernz zu sparen.
class Leaf(val h:Polyeder) extends NodeUnderMesh {
  override def hasChildren = false

  override def finishedGeneration = true

  override def getChild(i:Int) = throw new NoSuchElementException("a leaf in a tree doesn't have children")
  // kann kein deadNode sein
  def isSet(info:PowerOfTwoCube,pos:PowerOfTwoCube) = true

  def insertNode(info:PowerOfTwoCube, insertinfo:PowerOfTwoCube, insertnode:Node) = insertnode

  override def apply(info:PowerOfTwoCube, p:ReadVec3i) = this

  override def updated(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, newLeaf:Leaf) = {
    if(h == newLeaf.h)
      this
    else{
      // wenn das Blatt einen größeren Bereich abdeckt der voll bzw leer ist:
      if(info.size >= 2) {
        val replacement = new InnerNodeUnderMesh(this)
        replacement.updated(info, octree, p, newLeaf)
      }
      else {
        newLeaf
      }
    }
  }

  override def toString = s"Leaf(${h.toString})"

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
  def addSurface(from:Polyeder, to:Polyeder, pos:ReadVec3i, dir:Int ,meshBuilder:TextureMeshBuilder, worldFunction: WorldFunction = WorldDefinition) = {
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

      for(ov <- occluderVertices){
        if( ov(axis) == 1-direction )
          occludingCoords += Vec2(ov(axisa),ov(axisb))
      }

      @inline def triangleMax(v0:Vec3, v1:Vec3, v2:Vec3) = {
        (v0(axis) == direction) && (v1(axis) == direction) && (v2(axis) == direction)
      }

      @inline def addVertices(v0:Vec3, v1:Vec3, v2:Vec3) {
        val matid = if( material >= 0 ) material else worldFunction.materialAtBlock(pos).id
        val matCount = 4//MaterialManager.materialCount.toDouble

        vertexBuilder += Vec3f(pos) + Vec3f(v0)
        texCoordBuilder += Vec2f( (v0(axisa)/matCount + matid/matCount).toFloat , v0(axisb).toFloat )

        vertexBuilder += Vec3f(pos) + Vec3f(v1)
        texCoordBuilder += Vec2f( (v1(axisa)/matCount + matid/matCount).toFloat , v1(axisb).toFloat )

        vertexBuilder += Vec3f(pos) + Vec3f(v2)
        texCoordBuilder += Vec2f( (v2(axisa)/matCount + matid/matCount).toFloat , v2(axisb).toFloat )

        vertexCounter += 3

        val normal = Vec3f(normalize(cross(v2-v1,v0-v1)))
        normalBuilder += normal
        normalBuilder += normal
        normalBuilder += normal
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
            val flatTriangle = Array(v0,v1,v2)
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

  override def genPolygons(info:PowerOfTwoCube, meshBuilder:TextureMeshBuilder,worldaccess:(Vec3i => Polyeder)):Int = {
    import info.{pos => nodepos, size => nodesize}
    assert(meshBuilder != null)
    var vertexCounter = 0

    if(nodesize == 1) {
      if( h != EmptyHexaeder ){
        for( i <- (0 to 5) ){
          val p2 = Vec3i(nodepos)
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
            val p1 = Vec3i(nodepos)
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
    vertexCounter * TextureMesh.byteStride
  }

  override def patchWorld(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, newLeaf:Leaf, vertpos:Int, vertcount:Int) : (NodeUnderMesh, Update) = {
    val replacement = updated(info, octree, p, newLeaf)

    val builder = new TextureMeshBuilder
    replacement.genPolygons(info,builder, v => octree(v).h )
    val update = Update(vertpos,vertcount,builder.result)
    (replacement,update)
  }
  //TODO: (?) NodeUnderMesh soll die eigene Größe (Vertices) kennen, und nicht die der Kinder
  override def patchWorld(info:PowerOfTwoCube, insertInfo:PowerOfTwoCube, newNode:NodeUnderMesh, insertVertexCount:Int, currentOffset:Int, currentVertexCount:Int) : UpdateInfo = {
    if(info == insertInfo)
      UpdateInfo(newNode, currentOffset, currentVertexCount, insertVertexCount )
    else {
      val replacement = new InnerNodeUnderMesh(this)
      replacement.patchWorld(info, insertInfo, newNode, insertVertexCount, currentOffset, currentVertexCount)
    }
  }

  override def repolyWorld(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, vertpos:Int, vertcount:Int) : Update = {
    require(vertpos >= 0)
    require(vertcount >= 0)
    val builder = new TextureMeshBuilder
    genPolygons(info, builder, v => octree(v).h )
    Update(vertpos,vertcount,builder.result)
  }

  override def genMesh(info:PowerOfTwoCube, worldaccess:(Vec3i => Polyeder) ) = {
    (new MeshNode(this)).genMesh(info, worldaccess)
  }

  def draw(info:PowerOfTwoCube,test:FrustumTest){}

  override def getPolygons( info:PowerOfTwoCube, pos:ReadVec3i, from:Int, to:Int): (Int,Int) = {
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

  def fill[V3 >: ReadVec3i](cube:PowerOfTwoCube, fill: V3 => Leaf ) : NodeUnderMesh = {
    if(cube.size == 1){
      fill(cube.pos)
    }
    else {
      val array = new Array[NodeUnderMesh](8)

      var i = 0
      while( i < 8 ) {
        array(i) = EmptyLeaf.fill(cube(i),fill)
        i += 1
      }
      new InnerNodeUnderMesh(array).merge
    }
  }

}

case object FullLeaf extends Leaf(FullHexaeder)

class ObjLeaf(val mesh:ObjMesh) extends Leaf(EmptyHexaeder)
