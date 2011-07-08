package xöpäx

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import simplex3d.data._
import simplex3d.data.float._

import Util._

object Octree{
	def apply[A:ClassManifest](data:Data3D[A]) = {
		
		import Util.{log2, isPowerOfTwo}
		
		assert( data.vsize.x == data.vsize.y && data.vsize.y == data.vsize.z && isPowerOfTwo(data.vsize.x) )
		val size = data.vsize.x
		
		val octree = new Octree[A](log2(size),data(Vec3i(0)))
		
		import Util._
		
		for (v <- Vec3i(0) until data.vsize)
			octree(v) = data(v)
		
		octree
	}
}

trait Node[A] extends Serializable{
	def apply(pos:Vec3i) : A
	def updated(pos:Vec3i,nv:A):Node[A]
}

case class Leaf[A:ClassManifest](val center:Vec3i, val radius:Int, val v:A) extends Node[A]{
	def apply(pos:Vec3i) = v
	def updated(pos:Vec3i,nv:A) = {
		if(v == nv)
			this
		else{
			if(radius > 0) {
				// go deeper into the tree?
				val replacement = new InnderNode(center,radius,v)
				replacement.updated(pos,nv)
			}
			else {
				new Leaf[A](center,0,nv)
			}
		}	
	}
	
	override def toString = if(v == null) "null" else v.toString
	
	override def equals(that:Any) = {
		that match {
			case l:Leaf[A] =>
				v == l.v
			case _ =>
				false
		}
	}
}

class InnderNode[A:ClassManifest](val center:Vec3i, val radius:Int, val v:A) extends Node[A]{
	//val data = new Data3D[Node[A]](Vec3i(2))
	val data = new Array[Node[A]](8)
	//initiali the 8 child nodes
	
	def index2vec(idx:Int) = 
		Vec3i((idx & 1),(idx & 2) >> 1,(idx & 4) >> 2)
	
	for(fidx <- 0 until 8){
		val topleft = center - (Vec3i(1,1,1) * (radius/2))
		val childCenter = topleft + (index2vec(fidx)*radius)
		data(fidx) = new Leaf[A](childCenter,radius/2,v)
	}
	
	def flatIndex(pos:Vec3i) = 
		(if(! (pos.x < center.x)) 1 else 0) |
		(if(! (pos.y < center.y)) 2 else 0) |
		(if(! (pos.z < center.z)) 4 else 0)
	
	def apply(pos:Vec3i) = {
		val index = flatIndex(pos)
		data(index)(pos)
	}
	
	import collection.JavaConverters._

	def updated(pos:Vec3i,v:A) = {
		val index = flatIndex(pos)
		data(index) = data(index).updated(pos,v)
		
		val first = data(0)
		// if all elements are equal
		var merge = true
		for(i <- data )
			merge = merge && (i == first)
			
		//if (  (true /: data.data) ( _ && first == _ ) )
		// merge them to a leaf
		if(merge)
			new Leaf[A](center,radius,v)
		else 
			this
	}
	
	override def toString = data.mkString("(",",",")")
}

case class NodeInfo[A](pos:Vec3i,size:Int,value:A)

class Octree[A:ClassManifest](logOfSize:Int,init:A) extends Data3D[A] with Serializable with Iterable[NodeInfo[A]]{
	val vsize = Vec3i(1 << logOfSize)
	
	var root:Node[A] = new Leaf(Vec3i(1,1,1)*(vsize/2),vsize.x/2,init)
	
	def apply(pos:Vec3i) = root(pos)
	def update(pos:Vec3i,v:A) {
		root = root.updated(pos,v)
	}
			
	override def toString = "Octree("+root.toString+")"
	
	def iterator = new Iterator[NodeInfo[A]]{
		var history = List(root)
		var height = logOfSize
		
		def hasNext = history != Nil
		def next = history.head match{
		case n:Leaf[A] => 
			history = history.tail

			val pos = n.center-math.max(n.radius,1)
			NodeInfo(pos, math.max(n.radius*2,1),n.v)
		case n:InnderNode[A] => 
			history = n.data ++: history.tail
			next
		}
	}
}

