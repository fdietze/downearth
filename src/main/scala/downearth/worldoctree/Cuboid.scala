package downearth.worldoctree

import simplex3d.math._
import simplex3d.math.double._


import interval.Interval3
import scala.Predef._
import downearth.util._
import simplex3d.math.doublex.functions._

trait ChildAccess[T] {
  // macht aus dem Vec3i index einen flachen index, der auf ein Array
  // angewendet werden kann
  def flat(ivec:ReadVec3i) = ivec.x+(ivec.y<<1)+(ivec.z<<2)

  // macht aus einem flachen Index wieder ein Vec3i-Index

  @inline def index2vec(idx:Int) = Vec3i((idx & 1),(idx & 2) >> 1,(idx & 4) >> 2)
  @inline def index2vecX(idx:Int) = (idx & 1)
  @inline def index2vecY(idx:Int) = (idx & 2) >> 1
  @inline def index2vecZ(idx:Int) = (idx & 4) >> 2

  def apply(i:Int):T = ???
  def apply(p:ReadVec3i):(Int,T) = ???

  def splitX:Array[T]   = ???
  def splitY:Array[T]   = ???
  def splitZ:Array[T]   = ???
  def splitLongest:Array[T] = ???
  def splitOct:Array[T] = ???
  def fullTree:(T,Any) = ???
}

case class Cuboid(pos:ReadVec3i, vsize:ReadVec3i) extends CuboidLike with ChildAccess[Cuboid] {
  // Erzeugung des Cuboid vom Kindknoten, aus einem flachen Index
  override def apply(i:Int):Cuboid = {
    val v = index2vec(i)
    val (a,b) = halves(vsize)
    val relpos = Vec3i(
      a.x*( i & 1    ),
      a.y*((i & 2)>>1),
      a.z*((i & 4)>>2)
    )
    val hsize = Vec3i(
      b.x*( i & 1    ) + a.x*(1-( i & 1    )),
      b.y*((i & 2)>>1) + a.y*(1-((i & 2)>>1)),
      b.z*((i & 4)>>2) + a.z*(1-((i & 4)>>2))
    )
    Cuboid(pos+v*relpos,hsize)
  }

  def splitlongest = {
    var halfsize = Vec3i(0)
    var offset = Vec3i(0)
    val longest = longestEdgeAxis

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

  override def splitOct = Array.tabulate(8)(apply)
}

case class Cube(pos:ReadVec3i, size:Int) extends CubeLike with ChildAccess[Cuboid] {
  // Erzeugung des Cuboid vom Kindknoten, aus einem flachen Index
  override def apply(i:Int) = {
    val v = index2vec(i)
    val (a,b) = halves(size)
    val relpos = Vec3i(
      a*( i & 1    ),
      a*((i & 2)>>1),
      a*((i & 4)>>2)
    )
    val hsize = Vec3i(
      b*( i & 1    ) + a*(1-( i & 1    )),
      b*((i & 2)>>1) + a*(1-((i & 2)>>1)),
      b*((i & 4)>>2) + a*(1-((i & 4)>>2))
    )
    Cuboid(pos+v*relpos,hsize)
  }

  override def splitOct = Array.tabulate(8)(apply)
}

object PowerOfTwoCube {
  def apply(pos:ReadVec3i, size:Int) = new PowerOfTwoCube(pos.x, pos.y, pos.z, size)
}

case class PowerOfTwoCube(posX:Int, posY:Int, posZ:Int, size:Int) extends PowerOfTwoCubeLike with ChildAccess[PowerOfTwoCube] {

  def pos = ConstVec3i(posX, posY, posZ)

  // Erzeugung des Cuboid vom Kindknoten, aus einem flachen Index
  override def apply(index:Int) = {
    val vx = index2vecX(index)
    val vy = index2vecY(index)
    val vz = index2vecZ(index)
    val hsize = size >> 1
    val newPosX = posX + vx*hsize
    val newPosY = posY + vy*hsize
    val newPosZ = posZ + vz*hsize
    new PowerOfTwoCube(newPosX, newPosY, newPosZ, hsize)
  }

  // Erzeugung des Cube vom Kindknoten, aus einem Vektor-Index
  override def apply(p:ReadVec3i):(Int,PowerOfTwoCube) = {
    assert( indexInRange(p), s"Index not in Range: $p not in $this" )
    val v = indexVec(p,pos,size)
    val index = flat(v)
    val hsize = size >> 1
    (index,PowerOfTwoCube(pos+v*hsize,hsize) )
  }

  override def splitOct = Array.tabulate(8)(apply)

  override def fullTree = if( longestEdgeLength == 1 )
      (this, 0)
    else
      (this,splitOct.map(_.fullTree))
}

trait CuboidLike {
  def pos:ReadVec3i
  def vsize:ReadVec3i
  def isDegenerate = vsize.x == 0 || vsize.y == 0 || vsize.z == 0
  def positiveVolume = vsize.x >= 0 || vsize.y >= 0 || vsize.z >= 0
  def isCube = vsize.x == vsize.y && vsize.y == vsize.z
  assert(!isDegenerate, s"Cuboid cannot be degenerate: $this")
  assert(positiveVolume, s"Cuboid needs a Positive Volume: $this")

  def upperPos = pos + vsize
  def indexInRange(p:ReadVec3i) = downearth.util.indexInRange(p,pos,vsize)
  def indexInRange(p:CuboidLike):Boolean = indexInRange(p.pos) && indexInRange(p.pos+p.vsize-1)
  def center = pos + (vsize / 2)
  def coordinates = pos until upperPos
  def volume = vsize.x * vsize.y * vsize.z

  def inside(that:CuboidLike):Boolean = {
    all(greaterThanEqual(this.pos, that.pos)) &&
    all(lessThanEqual(this.upperPos, that.upperPos))
  }

  def vertices = Array[ReadVec3i](
    pos,
  //Vec3i(pos.x          ,pos.y          ,pos.z),
    ConstVec3i(pos.x + vsize.x,pos.y          ,pos.z),
    ConstVec3i(pos.x          ,pos.y + vsize.y,pos.z),
    ConstVec3i(pos.x + vsize.x,pos.y + vsize.y,pos.z),
    ConstVec3i(pos.x          ,pos.y          ,pos.z + vsize.z),
    ConstVec3i(pos.x + vsize.x,pos.y          ,pos.z + vsize.z),
    ConstVec3i(pos.x          ,pos.y + vsize.y,pos.z + vsize.z),
    ConstVec3i(pos.x + vsize.x,pos.y + vsize.y,pos.z + vsize.z)
  )

  def overlaps(that:CuboidLike):Boolean = {
    this.vertices.exists(that.indexInRange) ||
    that.vertices.exists(this.indexInRange)
  }

  def intersection(that:CuboidLike) = {
    val newPos = max(this.pos, that.pos)
    val newSize = min(this.upperPos, that.upperPos) - newPos
    Cuboid(newPos, newSize)
  }

  // Listet alle die Koordinaten auf, die innerhalb von beiden Bereichen sind.
  def intersectionCoordinates(that:CuboidLike) = {
    val low = max(pos,that.pos)
    val high = min(upperPos,that.upperPos)
    low until high
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

  def longestEdgeAxis:Int = {
    // Z als erstes, da es Sinn macht als erstes Horizontal zu zerteilen
    if( vsize.z >= vsize.x && vsize.z >= vsize.y ) 2
    else if( vsize.x >= vsize.y && vsize.x >= vsize.z ) 0
    else 1 // if( vsize.y >= vsize.x && vsize.y >= vsize.z )
  }

  def shortestEdgeAxis:Int = {
    if( vsize.x <= vsize.y && vsize.x <= vsize.z ) 0
    else if( vsize.y <= vsize.x && vsize.y <= vsize.z ) 1
    else 2 // if( size.z <= size.x && size.z <= size.y )
  }

  def longestEdgeLength = max(max(vsize.x, vsize.y),vsize.z)
  def shortestEdgeLength = min(min(vsize.x, vsize.y),vsize.z)

  def withBorder(thickness:Int = 1) = Cuboid(pos - thickness, vsize + thickness*2)
  def toCuboid = Cuboid(pos, vsize)
  def toInterval3 = Interval3(Vec3(pos), Vec3(pos + vsize))
}

trait CubeLike extends CuboidLike {
  def size:Int
  def vsize = ConstVec3i(size)

  override def volume = size*size*size

  override def longestEdgeAxis = 0
  override def shortestEdgeAxis = 0
  override def longestEdgeLength = size
  override def shortestEdgeLength = size

  def overlaps(sphere:Sphere) = {
    @inline def squared(x:Int): Int = x * x

    val C1 = this.pos
    val C2 = this.upperPos
    val S = sphere.pos
    val R = sphere.radius
    var dist_squared = R * R

    if (S.x < C1.x) dist_squared -= squared(S.x - C1.x)
    else if (S.x > C2.x) dist_squared -= squared(S.x - C2.x)
    if (S.y < C1.y) dist_squared -= squared(S.y - C1.y)
    else if (S.y > C2.y) dist_squared -= squared(S.y - C2.y)
    if (S.z < C1.z) dist_squared -= squared(S.z - C1.z)
    else if (S.z > C2.z) dist_squared -= squared(S.z - C2.z)

    dist_squared > 0
  }
}

trait PowerOfTwoCubeLike extends CubeLike{
  assert( isPowerOfTwo(size), s"Edge length not a power of two: $this" )

  // Wenn die Kinder als Array3D gespeichert werden würden, dann wäre dies die
  // Berechnung ihres Index. Das Array3D wird nicht mehr verwendet, aber an
  // vielen stellen wird noch sein Verhalten imitiert.
  def indexVec(p:ReadVec3i, nodepos:ReadVec3i = pos, nodesize:Int = size) = ((p-nodepos)*2)/nodesize
}


case class Sphere(pos:ReadVec3i, radius:Int) {
  def overlaps(cube:CubeLike) = cube overlaps this
}
