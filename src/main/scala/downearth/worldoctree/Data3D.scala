package downearth.worldoctree

import simplex3d.math.{Vec2i, Vec3i}

import reflect.ClassTag
import downearth.util._

object Data3D{
	// Abbildung der flachen Indices der Vertizes auf ihre 3D Indizes als Vec3i
	val vectorIndices = Array.tabulate(8)( (i) => Vec3i(i&1,(i&2)>>1,(i&4)>>2) )
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

  def fill( elem: Vec3i => A ) {
    for( pos <- Vec3i(0) until vsize ) {
      this(pos) = elem(pos)
    }
  }

  def fill(elem: Vec3i => A, areas:Iterable[CuboidLike], offset:Vec3i) {
    for( area <- areas; rawPos <- area.coordinates ) {
      val pos = offset + rawPos
      this(pos) = elem(pos)
    }
  }

  def fillBorder( elem: Vec3i => A ) {
    for( x <- Seq(0, vsize.x-1); yzpos <- Vec2i(0) until vsize.yz ) {
      val pos = Vec3i(x,yzpos)
      this(pos) = elem(pos)
    }
    for( y <- Seq(0, vsize.y-1); xzpos <- Vec2i(0) until vsize.xz ) {
      val pos = Vec3i(xzpos.x, y, xzpos.y)
      this(pos) = elem(pos)
    }
    for( z <- Seq(0, vsize.z-1); xypos <- Vec2i(0) until vsize.xy ) {
      val pos = Vec3i(xypos, z)
      this(pos) = elem(pos)
    }
  }
}

class Array3D[@specialized(Byte,Short,Float,Double) A:ClassTag](val vsize:Vec3i, val data:Array[A])
extends Data3D[A] with Iterable[A] {
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
		assert( indexInRange(v), s"$v not in range $vsize")
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

  def alignString[T](a:Array[T]) = a.toList.grouped(vsize.z).map(_.mkString(", ")).grouped(vsize.y).map(_.mkString("\n")).mkString("\n\n")
  override def toString = alignString(data)
  def toStringRounded(n:Int) = alignString(data.map(s"%.${n}f" format _))

	override def clone = {
		new Array3D(vsize,data.clone)
	}
}
