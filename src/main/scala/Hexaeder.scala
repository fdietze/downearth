package xöpäx

import simplex3d.math.{Vec3i,Vec3b,ConstVec2i,all}
import simplex3d.math.float.functions.{lessThan,lessThanEqual,greaterThanEqual,normalize,cross,dot}
import simplex3d.math.float.{Vec3,Vec2}
import simplex3d.data.DataArray._

import Util._

// Konstanten zur Verwendung im Hexaeder
object Hexaeder{
	// die Lookup-Tabelle für die Vertex-Indizes der sechs Seitenflächen
	val planelookup = Vector(
		Vector(0,2,4,6),
		Vector(1,3,5,7),
		Vector(1,0,5,4),
		Vector(3,2,7,6),
		Vector(0,1,2,3),
		Vector(4,5,6,7)
	)
}

import Hexaeder._
import scala.collection.mutable.WrappedArray

// TODO ein hexaeder der nicht geändert werden kann
case object FullHexaeder extends PartialHexaeder{
	override def toString = "[X]"
	private val m_normals = Array(Vec3( 1,0,0),Vec3(0, 1,0),Vec3(0,0, 1),Vec3(-1,0,0),Vec3(0,-1,0),Vec3(0,0,-1))
	override def normals = m_normals
	override def planemax(axis:Int, direction:Int) = true
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
}

class PartialHexaeder(
	var X:Int = 0x80808080,
	var Y:Int = 0x88008800,
	var Z:Int = 0x88880000)
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
	
	def detail =  8
	def detailf =  8f
	
	def checkrange(p:Int) = 0 <= p && p < 8 // 8 Vertices
	def checkvalue(v:Float) = 0 <= v && v <= 1f
	def checkvertex(v:Vec3) = all(greaterThanEqual(v,Vec3(0))) && all(lessThanEqual(v,Vec3(1f)))
	
	def writeX(i:Int,v:Int){ X = (X & ~(15 << (i*4))) | v << (i*4) }
	def readX(i:Int) = (X >> (i*4)) & 15
	def writeY(i:Int,v:Int){ Y = (Y & ~(15 << (i*4))) | v << (i*4) }
	def readY(i:Int) = (Y >> (i*4)) & 15
	def writeZ(i:Int,v:Int){ Z = (Z & ~(15 << (i*4))) | v << (i*4) }
	def readZ(i:Int) = (Z >> (i*4)) & 15
	
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
		writeVertex(p, Vec3i(v*detail))
	}

	def update(p:Int, axis:Int, v:Float) = {
		assert(checkrange(p))
		assert(checkvalue(v))
		writeComponent(p, axis, (v*detailf).toInt)
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
	// TODO: Effizienter implementieren
	def noVolume():Boolean = {
		val edges = (0 until 8)
		
		for( axis <- Seq(1,2,4) ) {
			 // Zerlege die vertices in zwei Mengen, die durch die Fläche getrennt wird,
			 // die durch die Achse als Normale definiert wird
			val (a,b) = edges.partition( x => (x & axis) == axis )
			if ((for( i <- 0 until 4 ) yield {
				// Prüfe, ob die Komponenten der Achse gleich sind
				apply(a(i),log2(axis)) == apply(b(i),log2(axis))
			}).reduceLeft( _ && _ ) )
				return true
		}
		return false
	}
}
