package xöpäx

import simplex3d.math.Vec3i
import simplex3d.math.float.functions.{lessThan,lessThanEqual,greaterThanEqual}
import simplex3d.math.{all}
/******************************************************************************/
/******* assert(x<sx && y < sy && z < sz && 0 <= x && 0 <= y && 0 <= z) *******/
/******* @specialized(Byte,Short,Float,Double)                          *******/
/******************************************************************************/

import Util._

trait Data3D[A]{
	def vsize:Vec3i
	def apply(v:Vec3i):A
	def update(v:Vec3i,value:A)
	def indexInRange(i:Vec3i) = all(lessThan(i,vsize)) && all(greaterThanEqual(i,Vec3i(0)))
	
	def fill( foo: Vec3i => A ){
		for( v <- Vec3i(0) until vsize ){
			val f = foo(v)
			assert(f != null)
			this(v) = f
		}
	}
}

class Array3D[@specialized(Byte,Short,Float,Double) A:ClassManifest](val vsize:Vec3i) 
extends Data3D[A] with Iterable[A] with Serializable{

	import vsize.{x => sx,y => sy, z => sz}

	val volume = sx*sy*sz
	private val data = new Array[A](volume)
	
	def index(pos:Vec3i) = pos.x + sx*(pos.y + sy*pos.z)
	
	def apply(v:Vec3i):A = {
		assert( indexInRange(v) )
		data( index(v) )
	}

	// 8 Umliegende Datensätze auslesen
	/*
	def apply(v:Vec3i,i:Int) = {
		apply(v + Vec3i(i&1,(i&2)>>1,(i&4)>>2))
	}*/

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
	
	import collection.Iterator
	
	def iterator = data.iterator
	
	override def toString = data.mkString
}


