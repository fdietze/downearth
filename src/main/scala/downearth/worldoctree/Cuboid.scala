package downearth.worldoctree

import simplex3d.math._
import simplex3d.math.double._


import interval.Interval3
import scala.Predef._
import downearth.util._
import simplex3d.math.doublex.functions._
import scala.Array

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

case class Cube(posX:Int, posY:Int, posZ:Int, size:Int) extends CubeLike with ChildAccess[Cuboid] {
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
  def diagonalLength = length(upperPos - pos)
  def boundingSphere = Sphere(center, diagonalLength / 2)

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
    val thisUpperX = this.pos.x + this.vsize.x
    val thisUpperY = this.pos.y + this.vsize.y
    val thisUpperZ = this.pos.z + this.vsize.z
    val thatUpperX = that.pos.x + that.vsize.x
    val thatUpperY = that.pos.y + that.vsize.y
    val thatUpperZ = that.pos.z + that.vsize.z

    this.pos.x <= thatUpperX && thisUpperX >= that.pos.x &&
    this.pos.y <= thatUpperY && thisUpperY >= that.pos.y &&
    this.pos.z <= thatUpperZ && thisUpperZ >= that.pos.z
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

object Cube {
  val diagonal = math.sqrt(3.0)
  val halfDiagonal = diagonal * 0.5
  def apply(pos:ReadVec3i, size:Int) = new Cube(pos.x, pos.y, pos.z, size)
}

trait CubeLike extends CuboidLike {

  def posX:Int
  def posY:Int
  def posZ:Int

  def pos = ConstVec3i(posX, posY, posZ)

  def size:Int
  def vsize = ConstVec3i(size)

  override def volume = size*size*size
  override def boundingSphere = Sphere(center, Cube.halfDiagonal*size)

  override def longestEdgeAxis = 0
  override def shortestEdgeAxis = 0
  override def longestEdgeLength = size
  override def shortestEdgeLength = size
  def radius = size / 2

  def overlaps(that:CubeLike):Boolean = {
    val thisUpperX = this.posX + this.size
    val thisUpperY = this.posY + this.size
    val thisUpperZ = this.posZ + this.size
    val thatUpperX = that.posX + that.size
    val thatUpperY = that.posY + that.size
    val thatUpperZ = that.posZ + that.size

    this.posX <= thatUpperX && thisUpperX >= that.posX &&
    this.posY <= thatUpperY && thisUpperY >= that.posY &&
    this.posZ <= thatUpperZ && thisUpperZ >= that.posZ
  }

  def overlaps(sphere:Sphere) = {
    @inline def squared(x:Double): Double = x * x

    val C1X = this.posX
    val C1Y = this.posY
    val C1Z = this.posZ
    val C2X = this.posX + this.size
    val C2Y = this.posY + this.size
    val C2Z = this.posZ + this.size
    val S = sphere.center
    val R = sphere.radius
    var dist_squared = R * R

    if (S.x < C1X) dist_squared -= squared(S.x - C1X)
    else if (S.x > C2X) dist_squared -= squared(S.x - C2X)
    if (S.y < C1Y) dist_squared -= squared(S.y - C1Y)
    else if (S.y > C2Y) dist_squared -= squared(S.y - C2Y)
    if (S.z < C1Z) dist_squared -= squared(S.z - C1Z)
    else if (S.z > C2Z) dist_squared -= squared(S.z - C2Z)

    dist_squared > 0
  }
}

trait PowerOfTwoCubeLike extends CubeLike {
  assert( isPowerOfTwo(size), s"Edge length not a power of two: $this" )

  // Wenn die Kinder als Array3D gespeichert werden würden, dann wäre dies die
  // Berechnung ihres Index. Das Array3D wird nicht mehr verwendet, aber an
  // vielen stellen wird noch sein Verhalten imitiert.
  def indexVec(p:ReadVec3i, nodepos:ReadVec3i = pos, nodesize:Int = size) = ((p-nodepos)*2)/nodesize
}


case class Sphere(center:ReadVec3, radius:Double) {
  def overlaps(cube:CubeLike) = cube overlaps this
}

case class Cone(apex_location:ReadVec3, direction_normal:ReadVec3, angle:Double) { cone =>
  val angleCos = cos(angle)
  val angleSin = sin(angle)
  val angleTan = tan(angle)

  def test(sphere:Sphere) = {
    // http://www.cbloom.com/3d/techdocs/culling.txt
    /*
    val V = sphere.center - cone.apex_location
    val a = dot(V,cone.direction_normal)
    val b = a * cone.angleTan
    val c = sqrt( dot(V,V) - a*a )
    val d = c - b
    val e = d * cone.angleCos

    if ( e >= sphere.radius ) 0        // totally outside
    else if ( e <= -sphere.radius ) 1  // totally inside
    else -1                            // partially inside
    */
    // without sqrt:
    // http://blog.julien.cayzac.name/2009/12/frustum-culling-sphere-cone-test-with.html
    val V = sphere.center - cone.apex_location
    val a = dot(V, cone.direction_normal)
    val p = a*cone.angleSin
    val q = cone.angleCos*cone.angleCos * dot(V, V) - a*a
    val r = q - sphere.radius*sphere.radius
    if (p<sphere.radius || q>0) {
      if (r < 2 * sphere.radius * p) -1 // the sphere is partially included
      else if (q<0) 1 // the sphere is totally included
      else 0 // cull the sphere
    }
    else{
      if ( -r < 2 * sphere.radius * p) -1 // the sphere is partially included
      else if (q<0) 1 // the sphere is totally included
      else 0 // cull the sphere
    }
  }
}

case class Frustum(planes:Array[Vec4]) {
  assert(planes.size == 6)
}
