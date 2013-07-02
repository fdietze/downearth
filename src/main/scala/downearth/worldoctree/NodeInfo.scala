package downearth.worldoctree

import simplex3d.math._
import simplex3d.math.double._


import interval.Interval3
import scala.Predef._
import downearth.util._
import simplex3d.math.doublex.functions._

// NodeInfo enthält die Metainformationen für einen Knoten im Octree, also
// Position in Weltkoordanaten und Größe. Zudem hat die Klasse noch Methoden,
// um Metainformationen der Kindknoten berechnen zu können.
case class NodeInfo(pos:Vec3i, size:Int) {
	def upperPos = pos+size
  def center = pos + (size >> 2)

  // Wenn die Kinder als Array3D gespeichert werden würden, dann wäre dies die
	// Berechnung ihres Index. Das Array3D wird nicht mehr verwendet, aber an 
	// vielen stellen wird noch sein Verhalten imitiert.
	def indexVec(p:Vec3i,nodepos:Vec3i = pos,nodesize:Int = size) = ((p-nodepos)*2)/nodesize
	
	// macht aus dem Vec3i index einen flachen index, der auf ein Array 
	// angewendet werden kann
	def flat(ivec:Vec3i) = ivec.x+(ivec.y<<1)+(ivec.z<<2)
	
	// macht aus einem flachen Index wieder ein Vec3i-Index
	def index2vec(idx:Int) =
		Vec3i((idx & 1),(idx & 2) >> 1,(idx & 4) >> 2)
	
	// Erzeugung des NodeInfo vom Kindknoten, aus einem Vektor-Index
	def apply(p:Vec3i):(Int,NodeInfo) = {
		require( indexInRange(p) )
		val v = indexVec(p,pos,size)
		val index = flat(v)
		val hsize = size >> 1
		(index,NodeInfo(pos+v*hsize,hsize) )
	}
	
	// Erzeugung des NodeInfo vom Kindknoten, aus einem flachen Index
	def apply(index:Int):NodeInfo = {
		val v = index2vec(index)
		val hsize = size >> 1
		NodeInfo(pos+v*hsize,hsize)
	}

  def split = List.tabulate(8)(apply)

  def indexInRange(p:Vec3i) = downearth.util.indexInRange(p,pos,size)
	
	def indexInRange(p:NodeInfo):Boolean = indexInRange(p.pos) && indexInRange(p.pos+p.size-1)
	
	// Listet alle die Koordinaten auf, die innerhalb von beiden Bereichen sind.
	def intersection(that:NodeInfo):Iterable[Vec3i] = {
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

    val nx = if( dir.x < 0 ) 0 else 1
    val ny = if( dir.y < 0 ) 0 else 2
    val nz = if( dir.z < 0 ) 0 else 4

    val v1 =  x |  y |  z
    val v2 =  x |  y | nz
    val v3 =  x | ny |  z
    val v4 =  x | ny | nz
    val v5 = nx |  y |  z
    val v6 = nx |  y | nz
    val v7 = nx | ny |  z
    val v8 = nx | ny | nz

    Array(v1,v2,v3,v4,v5,v6,v7,v8)
  }

  def inside(that:NodeInfo) = {
    all(greaterThanEqual(this.pos, that.pos)) &&
    all(lessThanEqual(this.upperPos, that.upperPos))
  }
	
	def toCuboid = Cuboid(pos, Vec3i(size))
	def toInterval3 = Interval3(Vec3(pos), Vec3(pos+size))
}

//TODO: @deprecated("move everything to NodeInfo")
case class Cuboid(pos:Vec3i, size:Vec3i) {
	def toInterval3 = Interval3(Vec3(pos), Vec3(pos + size))
	def toNodeinfo = {
		assert(isCube)
		NodeInfo(pos, size.x)
	}

  def isCube = size.x == size.y && size.y == size.z

	def indexInRange(p:Vec3i) = downearth.util.indexInRange(p,pos,size)
	def indexInRange(p:NodeInfo):Boolean = indexInRange(p.pos) && indexInRange(p.pos+p.size-1)
	
	def longestedge:Int = {
		// Z als erstes, da es Sinn macht als erstes Horizontal zu zerteilen
		if( size.z >= size.x && size.z >= size.y )
			2
		else if( size.x >= size.y && size.x >= size.z )
			0
		else// if( size.y >= size.x && size.y >= size.z )
			1
	}

	def shortestedge:Int = {
		if( size.x <= size.y && size.x <= size.z )
			0
		else if( size.y <= size.x && size.y <= size.z )
			1
		else // if( size.z <= size.x && size.z <= size.y )
			2
	}


	def splitlongest:Array[Cuboid] = {
		var halfsize = Vec3i(0)
		var offset = Vec3i(0)
		
		if( longestedge == 0 ) {
			halfsize = size / Vec3i(2,1,1)
			offset = Vec3i(halfsize(0), 0, 0)
		}

		else if( longestedge == 1 ) {
			halfsize = size / Vec3i(1,2,1)
			offset = Vec3i(0, halfsize(1), 0)
		}

		else if( longestedge == 2 ) {
			halfsize = size / Vec3i(1,1,2)
			offset = Vec3i(0, 0, halfsize(2))
		}

		Array(Cuboid(pos, halfsize), Cuboid(pos + offset, halfsize))
	}
	def octsplit:Array[Cuboid] = {
		assert( isCube )
		val childsize = size.x >> 1
		(for( v <- Vec3i(0) until Vec3i(2) ) yield
			Cuboid(pos + v*childsize, Vec3i(childsize))
		).toArray
	}
	
	def nodeinfos = {
		val nodeinfosize = size(shortestedge)
		for(
			x <- 0 until size.x by nodeinfosize;
			y <- 0 until size.y by nodeinfosize;
			z <- 0 until size.z by nodeinfosize
		) yield {
			NodeInfo(pos + Vec3i(x,y,z), nodeinfosize)
		}
	}
}

