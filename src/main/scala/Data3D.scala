package openworld

import simplex3d.math.Vec3i
import simplex3d.math.double.functions.{lessThan,lessThanEqual,greaterThanEqual}
import simplex3d.math.{all}

import Util._

object Data3D{
	// Abbildung der flachen Indices der Vertizes auf ihre 3D Indizes als Vec3i
	val vectorIndices = for(i <- 0 until 8) yield Vec3i(i&1,(i&2)>>1,(i&4)>>2)
}

import Data3D._

trait Data3D[A]{
	def vsize:Vec3i
	def apply(v:Vec3i):A
	def update(v:Vec3i,value:A)
	//def indexInRange(i:Vec3i) = all(lessThan(i,vsize)) && all(greaterThanEqual(i,Vec3i(0)))
	def indexInRange(i:Vec3i) = {
		i.x >= 0 &&
		i.y >= 0 &&
		i.z >= 0 &&
		i.x < vsize.x &&
		i.y < vsize.y &&
		i.z < vsize.z
	}
	
	def fill( foo: Vec3i => A ){
		for( v <- Vec3i(0) until vsize ){
			val f = foo(v)
			assert(f != null)
			this(v) = f
		}
	}
}

class Array3D[@specialized(Byte,Short,Float,Double) A:ClassManifest](val vsize:Vec3i, val data:Array[A]) 
extends Data3D[A] with Iterable[A] with Serializable{
	def this(vsize:Vec3i) =  this(vsize, new Array[A](vsize.x * vsize.y * vsize.z) )

	import vsize.{x ⇒ sx,y ⇒ sy, z ⇒ sz}
	val volume = sx*sy*sz
	
	def index(pos:Vec3i) = pos.x + sx*(pos.y + sy*pos.z)
	
	def apply(v:Vec3i):A = {
		assert( indexInRange(v) )
		data( index(v) )
	}

	def update(v:Vec3i,i:Int,value:A){
		update(v + Vec3i(i&1,(i&2)>>1,(i&4)>>2),value)
	}

	def update(v:Vec3i,value:A){
		assert( indexInRange(v) )
		data( index(v) ) = value
	}

	def update(v:Vec3i, data:IndexedSeq[A]){
		assert( indexInRange(v) )
		for( i <- 0 until 8 )
			update( v,i,data(i) )
	}
	
	// Wird für den HexaederMC verwerdet, und extrahiert die 8 Datenpunkte, die für die Generierung eines Hexaeders relevant sind
	def extract(pos:Vec3i) = {
		vectorIndices map ( v ⇒ apply(v+pos) )
	}
	
	import collection.Iterator
	
	def iterator = data.iterator
	
	override def toString = data.mkString
	
	override def clone = {
		new Array3D(vsize,data.clone)
	}
}
