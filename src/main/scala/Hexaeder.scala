package openworld

import Util._
import simplex3d.math.{Vec3i,Vec3b,ConstVec2i,all}
import simplex3d.math.float.functions.{lessThanEqual,greaterThanEqual,normalize,cross,dot,Pi,round}
import simplex3d.math.float.{Vec4,Vec3,Vec2,Mat3x4}

// Konstanten zur Verwendung im Hexaeder
object Hexaeder {
	// die Lookup-Tabelle für die Vertex-Indizes der sechs Seitenflächen
	val detail = Config.hexaederResolution
	assert( 0 < detail && detail <= 15 )
	
	val detailf =  detail.toFloat
	val mask = detail << 0  | detail << 4  | detail << 8  | detail << 12 | 
	           detail << 16 | detail << 20 | detail << 24 | detail << 28

	def apply(vertices: Seq[Vec3]): PartialHexaeder = {
		assert(vertices.size == 8)
		val h = new PartialHexaeder
		var i = 0
		for(v ← vertices){
			h(i) = v
			i += 1
		}
		h
	}
	
	def apply(v0:Vec3, v1:Vec3, v2:Vec3, v3:Vec3, v4:Vec3, v5:Vec3, v6:Vec3, v7:Vec3):Hexaeder =
		apply(Seq(v0,v1,v2,v3,v4,v5,v6,v7))
}

import Hexaeder._

// TODO ein immutable Hexaeder, denn im Octree dürfen Hexaeder nicht verändert werden ohne sie auszutauschen
case object FullHexaeder extends PartialHexaeder{
	override def toString = "[X]"
	private val m_normals = Array(Vec3( 1,0,0),Vec3(0, 1,0),Vec3(0,0, 1),Vec3(-1,0,0),Vec3(0,-1,0),Vec3(0,0,-1))
	override def normals = m_normals
	override def planemax(axis:Int, direction:Int) = true
	override def rotateZ = this
}

case object EmptyHexaeder extends Hexaeder{
	def apply(p:Int, axis:Int) = 0
	def apply(p:Int) = Vec3(0)
	def vertices = Nil

	def noVolume = true
	def planemax(axis:Int, direction:Int) = false
	def planecoords(axis:Int, direction:Int):Seq[Vec2] = Nil
	def planetriangles(axis:Int, direction:Int) = Nil
	def normals = Nil
	def rotateZ = this
	
	override def toString = "[ ]"
}

case object UndefHexaeder extends Hexaeder{
	def apply(p:Int, axis:Int) = 0
	def apply(p:Int) = Vec3(0)
	def vertices = Nil
	def noVolume = true
	def planemax(axis:Int, direction:Int) = false
	def planecoords(axis:Int, direction:Int):Seq[Vec2] = Nil
	def planetriangles(axis:Int, direction:Int) = Nil
	def normals = Nil
	def rotateZ = this
	override def toString = "[~]"
}

trait Hexaeder extends Serializable{
	def apply(p:Int,axis:Int):Float
	def apply(p:Int):Vec3
	def vertices:Seq[Vec3]

	def noVolume:Boolean
	def planemax(axis:Int, direction:Int):Boolean
	def planecoords(axis:Int, direction:Int):Seq[Vec2]
	def planetriangles(axis:Int, direction:Int):Seq[Vec3]
	def normals:Seq[Vec3]
	def rotateZ:Hexaeder
}

class PartialHexaeder(
	var X:Int = 0xF0F0F0F0 & mask,
	var Y:Int = 0xFF00FF00 & mask,
	var Z:Int = 0xFFFF0000 & mask)
		extends Hexaeder {
	
	//   Stores 8 Vertices * 3 Nibbles
	//   +---+---+---+---+---+---+---+---+---+---+---+---+
	// i | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |10 |11 |
	//   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	//   |x|x|x|x|x|x|x|x|y|y|y|y|y|y|y|y|z|z|z|z|z|z|z|z|
	//   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

	implicit def int2byte(i:Int) = i.toByte
	implicit def bool2int(b:Boolean) = if(b) 1 else 0
	
	override def toString = "%h %h %h".format(X,Y,Z)

	override def equals(that:Any) = {
		that match {
		case h:PartialHexaeder =>
			(X == h.X && Y == h.Y && Z == h.Z)
		case _ => 
			false
		}
	}
	
	def checkrange(p:Int) = 0 <= p && p < 8 // 8 Vertices
	def checkvalue(v:Float) = 0 <= v && v <= 1f
	def checkvertex(v:Vec3) = all(greaterThanEqual(v,Vec3.Zero)) && all(lessThanEqual(v,Vec3.One))
	def chechvertexi(v:Vec3i) = all( greaterThanEqual(v,Vec3i.Zero) ) && all(lessThanEqual(v,Vec3i(detail)))
	
	def writeX(i:Int,v:Int){
		assert(v <= detail)
		X = (X & ~(15 << (i << 2))) | v << (i << 2)
	}
	def readX(i:Int) = (X >> (i << 2)) & 15
	def writeY(i:Int,v:Int){
		assert(v <= detail)
		Y = (Y & ~(15 << (i << 2))) | v << (i << 2)
	}
	def readY(i:Int) = (Y >> (i << 2)) & 15
	def writeZ(i:Int,v:Int){
		assert(v <= detail)
		Z = (Z & ~(15 << (i << 2))) | v << (i << 2)
	}
	def readZ(i:Int) = (Z >> (i << 2)) & 15
	
	def readVertex(p:Int) = {
		Vec3i(readX(p),readY(p),readZ(p))
	}
	
	def writeVertex(p:Int, v:Vec3i) = {
		writeX(p,v.x)
		writeY(p,v.y)
		writeZ(p,v.z)
	}
	
	def readComponent(p:Int, axis:Int):Int = {
		axis match{
			case 0 => return readX(p)
			case 1 => return readY(p)
			case 2 => return readZ(p)
		}
	}

	def writeComponent(p:Int, axis:Int, v:Int) = {
		axis match{
			case 0 => writeX(p,v)
			case 1 => writeY(p,v)
			case 2 => writeZ(p,v)
		}
	}

	def apply(p:Int) = {
		assert(checkrange(p))
		Vec3(readVertex(p)) / detailf
	}

	def apply(p:Int,axis:Int) = {
		assert(checkrange(p))
		readComponent(p, axis) / detailf
	}
	
	def apply(p:Vec3b):Vec3 = apply(p.x + (p.y << 1) + (p.z << 2))
	def apply(p:Vec3b, axis:Int):Float = apply(p.x + (p.y << 1) + (p.z << 2), axis)

	
	def update(p:Int,v:Vec3) = {
		assert(checkrange(p))
		assert(checkvertex(v))
		val ivec = Vec3i(round(v*detailf))
		assert(chechvertexi(ivec))
		
		writeVertex(p, ivec)
	}

	def update(p:Int, axis:Int, v:Float) = {
		assert(checkrange(p))
		assert(checkvalue(v))
		writeComponent(p, axis, (round(v*detailf)).toInt)
	}

	def update(p:Vec3b,v:Vec3){ update(p.x + (p.y << 1) + (p.z << 2),v) }
	def update(p:Vec3b,axis:Int,v:Int){ update(p.x + (p.y << 1) + (p.z << 2),axis,v) }
	
	// gibt 2d koordinaten einer seitenfläche an.
	// wird zur überprüfung, ob die Seite Verdeckt wird verwendet
	def accessor(k:Vec3i,axis:Int):ConstVec2i = {
			axis match {
			case 0 => k.yz
			case 1 => k.xz
			case 2 => k.xy
		}
	}
	
	// gibt eine Collection mit allen normalen der oberfläche
	def normals = {
		import scala.collection.mutable.ArrayBuilder
		val normalBuilder = ArrayBuilder.make[Vec3]
		for(axis <- 0 to 2; direction <- 0 to 1){
			if(planemax(axis,direction)) {
				val normal = Vec3(0)
				normal(axis) = (direction << 1) - 1
				normalBuilder += normal
			}
			else {
				val v = planetriangles(axis,direction)
				normalBuilder += normalize(cross(v(2)-v(1),v(0)-v(1)))
				normalBuilder += normalize(cross(v(5)-v(4),v(3)-v(4)))
			}
		}
		normalBuilder.result
	}
	
	// Gibt eine Liste aller 8 Vertices zurück
	def vertices = (0 until 8) map apply
	def vertices_= ( verts: Seq[Vec3] ) {
		var i = 0
		for( v ← verts ) {
			update(i,v)
			i += 1
		}
	}
	/**
	 * @param axis Achse 0,1,2 für x,y,z
	 * @param direction 0,1 für nagativ, positiv
	 * @return Vektor mit indizes der Vertices einer Seitenfläche
	 */
	def plane(axis:Int, direction:Int) = planelookup(axis << 1 | direction)

	// gibt an, ob sich eine Seitenfläche am äußersten Rand seiner Zelle befindet,
	// d.h. alle Vertices in der jeweiligen Achsenrichtung den größten bzw kleinsten Wert haben
	def planemax(axis:Int, direction:Int) =	{
		var isAllMax = true
		for( vi <- plane(axis,direction) )
			isAllMax &&= readComponent(vi, axis) == direction*detail

		isAllMax
	}
	
	
	// TODO zusätzlich eine reduzierte Version von planetriangles zur Verfügung stellen:
	// planetriangles für polygone 
	// planetriangles2d für texturkoordinaten und occlusion
	def planecoords(axis:Int, direction:Int) = {
		val (axisa,axisb) = Util.otherAxis( axis )
		
		plane(axis, direction).map( p => Vec2(
				readComponent(p, axisa)/detailf,
				readComponent(p, axisb)/detailf )
			)
	}
	
	def planetriangles(axis:Int, direction:Int) = {
		val p = plane(axis, direction)
		val indices =
			if( direction == 1 )
				Vector(p(0),p(1),p(3),p(2))
			else
				Vector(p(1),p(0),p(2),p(3))
		
		val Vector(v0,v1,v2,v3) = indices.map(apply _)
		// koordinaten für zwei Dreiecke
		val triangleCoords =
			if(dot(v3-v1,cross(v2-v1,v0-v1)) > 0)
				Vector(v0,v1,v3,  v3,v1,v2)
			else
				Vector(v0,v1,v2,  v0,v2,v3)
		
		triangleCoords
	}
	

	// Testet, ob zwei Seitenflächen direkt aneinander liegen
	// wird benötigt, denn alle Hexaeder ohne Volumen sollen durch
	// den EmptyHexaeder ersetzt werden, damit sie im Octree auch
	// zusammengefasst werden können.
	def noVolume():Boolean = {
		val edges = (0 until 8)

		var logaxis = 0
		for( axis <- Seq(1,2,4) ) {
			 // Zerlege die vertices in zwei Mengen, die durch die Fläche getrennt wird,
			 // die durch die Achse als Normale definiert wird
			val (a,b) = edges.partition( x => (x & axis) == axis )

			if(
				apply(a(0),logaxis) == apply(b(0),logaxis) &&
				apply(a(1),logaxis) == apply(b(1),logaxis) &&
				apply(a(2),logaxis) == apply(b(2),logaxis) &&
				apply(a(3),logaxis) == apply(b(3),logaxis)
			){
				return true
			}
			logaxis += 1
		}
		return false
	}

	def rotateZ = {
		// def map(x:Int) = (0x000F000F & x) << 4 | (0x00F000F0 & x) << 8 | (0x0F000F00 & x) >> 8 | (0xF000F000 & x) >> 4
		val verts = vertices map ( v => Vec3(1-v.y, v.x, v.z) )
		val newverts = Vector(2,0,3,1,6,4,7,5) map verts

		def check(s:Seq[Vec3]):Boolean = {
			for(v ← s){
				if(! all(lessThanEqual(v,Vec3.One)))
					return false
				if(! all(greaterThanEqual(v,Vec3.Zero)))
					return false
			}
			return true
		}
		Hexaeder(newverts)
	}
}
