package downearth.worldoctree

import simplex3d.math._
import simplex3d.math.double._


import interval.Interval3
import scala.Predef._
import downearth.util._
import simplex3d.math.doublex.functions._

trait CuboidLike {
  def pos:Vec3i
  def vsize:Vec3i
  def isDegenerate = vsize.x == 0 || vsize.y == 0 || vsize.z == 0
  def isCube = vsize.x == vsize.y && vsize.y == vsize.z
  require(!isDegenerate)

  def upperPos = pos+vsize
  def indexInRange(p:Vec3i) = downearth.util.indexInRange(p,pos,vsize)
  def indexInRange(p:CuboidLike):Boolean = indexInRange(p.pos) && indexInRange(p.pos+p.vsize-1)
  def center = pos + (vsize / 2)
  def coordinates = pos until upperPos
  def volume = vsize.x * vsize.y * vsize.z

  def inside(that:CuboidLike) = {
    all(greaterThanEqual(this.pos, that.pos)) &&
    all(lessThanEqual(this.upperPos, that.upperPos))
  }

  // Listet alle die Koordinaten auf, die innerhalb von beiden Bereichen sind.
  def intersection(that:PowerOfTwoCube):Iterable[Vec3i] = {
    val pos1 = max(pos,that.pos)
    val pos2 = min(upperPos,that.upperPos)
    pos1 until pos2
  }

  // front to back Traversal order seen from point p
  def traversalOrder(camera:ReadVec3):Array[Int] = {
    val dir = center-camera
    val  x = if( dir.x < 0 ) 1 else 0
    val  y = if( dir.y < 0 ) 2 else 0
    val  z = if( dir.z < 0 ) 4 else 0

    val nx = 1 - x
    val ny = 2 - y
    val nz = 4 - z

    val order = new Array[Int](8)

    order(0) =  x |  y |  z
    order(1) =  x |  y | nz
    order(2) =  x | ny |  z
    order(3) =  x | ny | nz
    order(4) = nx |  y |  z
    order(5) = nx |  y | nz
    order(6) = nx | ny |  z
    order(7) = nx | ny | nz

    return order
  }

  def longestedge:Int = {
    // Z als erstes, da es Sinn macht als erstes Horizontal zu zerteilen
    if( vsize.z >= vsize.x && vsize.z >= vsize.y ) 2
    else if( vsize.x >= vsize.y && vsize.x >= vsize.z ) 0
    else 1 // if( vsize.y >= vsize.x && vsize.y >= vsize.z )
  }

  def shortestedge:Int = {
    if( vsize.x <= vsize.y && vsize.x <= vsize.z ) 0
    else if( vsize.y <= vsize.x && vsize.y <= vsize.z ) 1
    else 2 // if( size.z <= size.x && size.z <= size.y )
  }

  def splitlongest:Array[Cuboid] = {
    var halfsize = Vec3i(0)
    var offset = Vec3i(0)
    val longest = longestedge

    if( longest == 0 ) {
      halfsize = vsize / Vec3i(2,1,1)
      offset = Vec3i(halfsize(0), 0, 0)
    } else if( longest == 1 ) {
      halfsize = vsize / Vec3i(1,2,1)
      offset = Vec3i(0, halfsize(1), 0)
    } else {// if( longest == 2 )
      halfsize = vsize / Vec3i(1,1,2)
      offset = Vec3i(0, 0, halfsize(2))
    }
    Array(Cuboid(pos, halfsize), Cuboid(pos + offset, halfsize))
  }

  def splitOct = ???

  def toInterval3 = Interval3(Vec3(pos), Vec3(pos + vsize))
}

trait CubeLike extends CuboidLike {
  def size:Int
  def vsize = Vec3i(size)
  require(isCube)
}

case class Cuboid(pos:Vec3i, vsize:Vec3i) extends CuboidLike

case class Cube(pos:Vec3i, size:Int) extends CubeLike {
  override def volume = size*size*size
}

case class PowerOfTwoCube(pos:Vec3i, size:Int) extends CubeLike {
  // Wenn die Kinder als Array3D gespeichert werden würden, dann wäre dies die
  // Berechnung ihres Index. Das Array3D wird nicht mehr verwendet, aber an
  // vielen stellen wird noch sein Verhalten imitiert.
  def indexVec(p:Vec3i, nodepos:Vec3i = pos, nodesize:Int = size) = ((p-nodepos)*2)/nodesize

  // macht aus dem Vec3i index einen flachen index, der auf ein Array
  // angewendet werden kann
  def flat(ivec:Vec3i) = ivec.x+(ivec.y<<1)+(ivec.z<<2)

  // macht aus einem flachen Index wieder ein Vec3i-Index
  def index2vec(idx:Int) = Vec3i((idx & 1),(idx & 2) >> 1,(idx & 4) >> 2)

  // Erzeugung des Cube vom Kindknoten, aus einem Vektor-Index
  def apply(p:Vec3i):(Int,PowerOfTwoCube) = {
    require( indexInRange(p) )
    val v = indexVec(p,pos,size)
    val index = flat(v)
    val hsize = size >> 1
    (index,PowerOfTwoCube(pos+v*hsize,hsize) )
  }

  // Erzeugung des Cube vom Kindknoten, aus einem flachen Index
  def apply(index:Int):PowerOfTwoCube = {
    val v = index2vec(index)
    val hsize = size >> 1
    PowerOfTwoCube(pos+v*hsize,hsize)
  }

  def split = Array.tabulate(8)(apply)
}
