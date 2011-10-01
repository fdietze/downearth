package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import Util._

// NodeInfo enthält die Metainformationen für einen Knoten im Octree, also
// Position in Weltkoordanaten und Größe. Zudem hat die Klasse noch Methoden,
// um Metainformationen der Kindknoten berechnen zu können.
case class NodeInfo(pos:Vec3i, size:Int) {
	def upperPos = pos+size
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
		assert( indexInRange(p) )
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
	
	def indexInRange(p:Vec3i) = Util.indexInRange(p,pos,size)
	
	def indexInRange(p:NodeInfo):Boolean = indexInRange(p.pos) && indexInRange(p.pos+p.size-1)
	
	// Listet alle die Koordinaten auf, die innerhalb von beiden Bereichen sind.
	def intersection(that:NodeInfo):Iterable[Vec3i] = {
		val pos1 = max(pos,that.pos)
		val pos2 = min(upperPos,that.upperPos)
		pos1 until pos2
	}
}

