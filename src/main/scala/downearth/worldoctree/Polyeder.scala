package downearth.worldoctree

import simplex3d.math.double._
import simplex3d.math.double.functions._

import Polyeder.mask
import downearth.ConvexHull3d

class Polyeder10(x:Long = 0xF0F0F0F0 & mask,
                 y:Long = 0xFF00FF00 & mask,
                 z:Long = 0xFFFF0000 & mask) 
                     extends APolyeder {
	var X:Int = 0xF0F0F0F0 & mask
	var Y:Int = 0xFF00FF00 & mask
	var Z:Int = 0xFFFF0000 & mask
	var bx = (x >> 32).toByte
	var by = (y >> 32).toByte
	var bz = (z >> 32).toByte
	
	def numVerts = 10
	
	def readX(i:Int) = ((bx.toLong << 32 | X) >> (i << 2)).toInt & 15
	def writeX(i:Int,v:Int){
		var l = X | bx.toLong << 32
		l = (l & ~(15L << (i << 2))) | v.toLong << (i << 2)
		X = l.toInt
		bx = (l >> 32).toByte
	}
	def readY(i:Int) = ((by.toLong << 32 | Y) >> (i << 2)).toInt & 15
	def writeY(i:Int,v:Int) {
		var l = Y | by.toLong << 32
		l = (l & ~(15L << (i << 2))) | v.toLong << (i << 2)
		Y = l.toInt
		by = (l >> 32).toByte
	}
	def readZ(i:Int) = ((bz.toLong << 32 | Z) >> (i << 2)).toInt & 15
	def writeZ(i:Int,v:Int){
		var l = Z | bz.toLong << 32
		l = (l & ~(15L << (i << 2))) | v.toLong << (i << 2)
		Z = l.toInt
		bz = (l >> 32).toByte
	}
	
	def vertices = {
		val verts = (Vector fill 10)(Vec3(0))
		var i = 0
		while(i < 8) {
			verts(i).x = ( X >> (i << 2) ) & 15
			verts(i).y = ( Y >> (i << 2) ) & 15
			verts(i).z = ( Z >> (i << 2) ) & 15
			i += 1
		}
		while(i < 10) {
			val j = (i - 8) << 2
			verts(i).x = ( bx >> j ) & 15
			verts(i).y = ( by >> j ) & 15
			verts(i).z = ( bz >> j ) & 15
			i += 1
		}
		vertices
	}
	
	def noVolume = volume == 0
	
	def noHexader = throw new NoSuchMethodException("eindeutige zuordnug von Vertices zu Seitenflächen existiert im Polyeder10 nicht")
	
	override def planemax(axis:Int, direction:Int) = noHexader
	override def planecoords(axis:Int, direction:Int) = noHexader
	override def planetriangles(axis:Int, direction:Int) = noHexader
	
	def notImplemented = throw new NoSuchMethodException("(noch) nicht implementiert")
	
	def outerTriangles(axis:Int, direction:Int) = notImplemented
	def innerTriangles:Seq[Vec3] = notImplemented
	
	def allTriangles = ConvexHull3d giftWrap vertices

  def toMessage = ???
}
