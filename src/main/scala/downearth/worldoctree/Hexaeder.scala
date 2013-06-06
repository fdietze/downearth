package downearth.worldoctree

import simplex3d.math.{Vec3i,Vec3b,all}
import simplex3d.math.double.functions.{lessThanEqual,greaterThanEqual,normalize,cross,dot,round,length}
import simplex3d.math.double.{Vec3,Vec2}

import collection.immutable.VectorBuilder
import downearth.Config
import downearth.message

// Konstanten zur Verwendung im Hexaeder
object Polyeder {
	// die Lookup-Tabelle für die Vertex-Indizes der sechs Seitenflächen
	val detail = Config.hexaederResolution
	assert( 0 < detail && detail <= 15 )
	
	val mask = detail << 0  | detail << 4  | detail << 8  | detail << 12 |
	           detail << 16 | detail << 20 | detail << 24 | detail << 28

	def apply(vertices: Seq[Vec3]): Polyeder = {
		if(vertices.size == 8){
			val h = new Hexaeder
			var i = 0
			for(v ← vertices){
				h(i) = Vec3(v)
				i += 1
			}
			h
		}
		else if(vertices.size == 10){
			val h = new Polyeder10
			var i = 0
			for(v ← vertices){
				h(i) = v
				i += 1
			}
			h
		}
		else {
			throw new Exception("entweder 8 oder 10 Verdices, etwas anderes wird zur Zeit noch nicht unterstützt")
		}
	}
	
	def apply(v0:Vec3, v1:Vec3, v2:Vec3, v3:Vec3, v4:Vec3, v5:Vec3, v6:Vec3, v7:Vec3):Polyeder =
		apply(Seq(v0,v1,v2,v3,v4,v5,v6,v7))
	
	// für den Hexaeder
	val planelookup = Vector(
		Vector(0,2,4,6),
		Vector(1,3,5,7),
		Vector(1,0,5,4),
		Vector(3,2,7,6),
		Vector(0,1,2,3),
		Vector(4,5,6,7)
	)
}

import Polyeder._

trait Polyeder {
	def apply(p:Int,axis:Int):Double
	def apply(p:Int):Vec3
	def vertices:Seq[Vec3]
	def numVerts:Int

	def noVolume:Boolean
	def volume:Double
	def planemax(axis:Int, direction:Int):Boolean
	def planecoords(axis:Int, direction:Int):Seq[Vec2]
	
	// das alte planetriangles was in zukunft rausfliegen soll wenn der Polyeder existiert
	def planetriangles(axis:Int, direction:Int):Seq[Vec3]
	// alle Dreiecke die verdecken können, bzw verdeckt werder können pro Zellwand.
	def outerTriangles(axis:Int, direction:Int):Seq[Vec3]
	// alle nicht verdeckenden/verdeckbaren Dreiecke
	def innerTriangles:Seq[Vec3]
	// all Dreiecke unabhängig davon, ob sie verdeckt werden oder nicht
	def allTriangles:Seq[Vec3]
	def rotateZ:Polyeder
  def toMessage:downearth.message.Hexaeder
}

abstract class APolyeder extends Polyeder {
	def writeX(i:Int,v:Int):Unit
	def readX(i:Int):Int
	def writeY(i:Int,v:Int):Unit
	def readY(i:Int):Int
	def writeZ(i:Int,v:Int):Unit
	def readZ(i:Int):Int
	
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
	
	def readVertex(p:Int) = {
		Vec3i(readX(p),readY(p),readZ(p))
	}
	
	def writeVertex(p:Int, v:Vec3i) = {
		writeX(p,v.x)
		writeY(p,v.y)
		writeZ(p,v.z)
	}

	def apply(p:Int) = {
		assert(checkrange(p))
		Vec3(readVertex(p)) / detail.toDouble
	}

	def apply(p:Int,axis:Int) = {
		assert(checkrange(p))
		readComponent(p, axis) / detail.toDouble
	}
	
	def apply(p:Vec3b):Vec3 = {
		apply( (if(p.x) 1 else 0) | (if(p.y) 2 else 0) | (if(p.z) 4 else 0) )
	}
	
	def apply(p:Vec3b, axis:Int):Double = {
		apply( (if(p.x) 1 else 0) | (if(p.y) 2 else 0) | (if(p.z) 4 else 0) , axis)
	}

	
	def update(p:Int,v:Vec3) = {
		assert(checkrange(p))
		assert(checkvertex(v))
		val ivec = Vec3i(round(v*detail))
		assert(chechvertexi(ivec))
		
		writeVertex(p, ivec)
	}

	def update(p:Int, axis:Int, v:Double) = {
		assert(checkrange(p))
		assert(checkvalue(v))
		writeComponent(p, axis, (round(v*detail)).toInt)
	}

	//def update(p:Vec3b,v:Vec3){ update(p.x + (p.y << 1) + (p.z << 2),v) }
	//def update(p:Vec3b,axis:Int,v:Int){ update(p.x + (p.y << 1) + (p.z << 2),axis,v) }
	
	def checkrange(p:Int) = 0 <= p && p < numVerts
	def checkvalue(v:Double) = 0 <= v && v <= 1.0
	def checkvertex(v:Vec3) = all(greaterThanEqual(v,Vec3.Zero)) && all(lessThanEqual(v,Vec3.One))
	def chechvertexi(v:Vec3i) = all( greaterThanEqual(v,Vec3i.Zero) ) && all(lessThanEqual(v,Vec3i(detail)))
	
	/*
	// gibt 2d koordinaten einer seitenfläche an.
	// wird zur überprüfung, ob die Seite Verdeckt wird verwendet
	def accessor(k:Vec3i,axis:Int):ConstVec2i = {
			axis match {
			case 0 => k.yz
			case 1 => k.xz
			case 2 => k.xy
		}
	}
	*/
	
	// erstellt eine Kopie von diesem Hexaeder der 90° um die Z-Achse rotiert wurde.
	def rotateZ = {
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
		Polyeder(newverts)
	}
	
	def volume = {
		var vol = 0.0
		val t = allTriangles
		for(i ← Range(0,t.size,3) ) {
			val normal = cross(t(i+2)-t(i+1),t(i+0)-t(i+1)) // Normale nach aussen
			val v3 = t(i+1)+normal
			val h = normalize( cross(t(i+2) - t(i+1), v3 - t(i+1)) )
			val height = dot(h, t(i+1)) - dot(h, t(i+0))
			val area = length(t(i+2)-t(i+1))*height*(0.5)
			vol += area * dot(normalize(normal),t(i+0)) / 3
		}
		vol
	}
}

// TODO immutable Hexaeder, because it is disallowed to modify one inside of the octree without reinserting it (notify the change)
case object FullHexaeder extends Hexaeder{
	override def toString = "[X]"
	override def planemax(axis:Int, direction:Int) = true
	override def rotateZ = this
	override def volume = 1
	override def innerTriangles = Nil
}

case object EmptyHexaeder extends Polyeder {
	def numVerts = 0
	def apply(p:Int, axis:Int) = 0
	def apply(p:Int) = Vec3(0)
	def vertices = Nil
	def noVolume = true
	def planemax(axis:Int, direction:Int) = false
	def planecoords(axis:Int, direction:Int):Seq[Vec2] = Nil
	def rotateZ = this
	def volume = 0
	def planetriangles(axis:Int, direction:Int) = Nil
	def outerTriangles(axis:Int, direction:Int) = Nil
	def innerTriangles = Nil
	def allTriangles = Nil
	override def toString = "[ ]"
  def toMessage = message.Hexaeder(fill=message.Hexaeder.Fill.EMPTY)
}

// Platzhalter für Hexaeder, die in der Generierung noch fehler Haben. Solle 
// nicht mehr auftreten
case object BrokenHexaeder extends Hexaeder(X=0x53535353, Y=0x55335533, Z=0x55553333)

case object UndefHexaeder extends Polyeder {
	def numVerts = 0
	def apply(p:Int, axis:Int) = 0
	def apply(p:Int) = Vec3(0)
	def vertices = Nil
	def innerTriangles = Nil
	def noVolume = true
	def planemax(axis:Int, direction:Int) = false
	def planecoords(axis:Int, direction:Int):Seq[Vec2] = Nil
	def planetriangles(axis:Int, direction:Int) = Nil
	def outerTriangles(axis:Int, direction:Int) = Nil
	def allTriangles = Nil
	def rotateZ = this
	override def toString = "[~]"
	def volume = 0
  def toMessage = ???
}

object Hexaeder {
  import message.Hexaeder.Fill._
  def fromMessage(m:message.Hexaeder) = m.fill match {
    case EMPTY => EmptyHexaeder
    case FULL  => FullHexaeder
    case DATA  =>
      val data = m.data.get
      import data._
      new Hexaeder(x, y, z)
    case f => sys.error("unknown fill type for hexaeder: " + f)
  }
}

class Hexaeder(
	var X:Int = 0xF0F0F0F0 & mask,
	var Y:Int = 0xFF00FF00 & mask,
	var Z:Int = 0xFFFF0000 & mask)
		extends APolyeder {
	
	//   Stores 8 Vertices * 3 Nibbles
	//   +---+---+---+---+---+---+---+---+---+---+---+---+
	// i | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |10 |11 |
	//   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
	//   |x|x|x|x|x|x|x|x|y|y|y|y|y|y|y|y|z|z|z|z|z|z|z|z|
	//   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

  def toMessage = message.Hexaeder(
    fill=message.Hexaeder.Fill.DATA, //TODO: default enum values in scalabuff not working?
    data=Some(message.Hexaeder.Data(X,Y,Z))
  )

	def numVerts = 8
	
	override def toString = "Hexaeder(0x%h, 0x%h, 0x%h)".format(X,Y,Z)

	override def equals(that:Any) = {
		that match {
		case h:Hexaeder =>
			(X == h.X && Y == h.Y && Z == h.Z)
		case _ => 
			false
		}
	}
	
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
		val (axisa,axisb) = downearth.util.otherAxis( axis )
		
		plane(axis, direction).map( p => Vec2(
				readComponent(p, axisa)/detail,
				readComponent(p, axisb)/detail )
			)
	}
	
	def planetriangles(axis:Int, direction:Int) = {
		val p = plane(axis, direction)
		
		val v0 = apply(p(1^direction))
		val v1 = apply(p(0^direction))
		val v2 = apply(p(2^direction))
		val v3 = apply(p(3^direction))
		
		// koordinaten für zwei Dreiecke
		if(dot(v3-v1,cross(v2-v1,v0-v1)) > 0)
			Vector(v0,v1,v3,  v3,v1,v2)
		else
			Vector(v0,v1,v2,  v0,v2,v3)
	}
	
	def outerTriangles(axis:Int, direction:Int) = {
		// koordinaten für zwei Dreiecke
		val triangles = planetriangles(axis,direction)
		
		val t0 =
			triangles(0)(axis) == direction &&
			triangles(1)(axis) == direction &&
			triangles(2)(axis) == direction
		
		val t1 =
			triangles(3)(axis) == direction &&
			triangles(4)(axis) == direction &&
			triangles(5)(axis) == direction
		
		if( t0 && t1 ) 
			triangles // beide Dreiecke sind aussen
		else if ( t0 )
			Vector(triangles(0),triangles(1),triangles(2))
		else if ( t1 )
			Vector(triangles(3),triangles(4),triangles(5))
		else
			Nil
	}
	
	def innerTriangles:Seq[Vec3] = {
		val triangles = new VectorBuilder[Vec3]
		var i = 0
		while(i < 6){
			val axis = (i >> 1)
			val direction = (i & 1).toDouble
			val p = plane(axis,i&1)
			
			val indices =
				if( direction == 1 )
					Vector(p(0),p(1),p(3),p(2))
				else
					Vector(p(1),p(0),p(2),p(3))
		
			val Vector(v0,v1,v2,v3) = indices.map(apply _)
			
			// koordinaten für zwei Dreiecke
			
			if( dot(v3-v1,cross(v2-v1,v0-v1) ) > 0 ){
				if( v0(axis) != direction || v1(axis) !=direction || v3(axis) != direction ){
					triangles += v0
					triangles += v1
					triangles += v3
				}
				
				if( v3(axis) != direction || v1(axis) !=direction || v2(axis) != direction ){
					triangles += v3
					triangles += v1
					triangles += v2
				}
			}
			else{
				if( v0(axis) != direction || v1(axis) !=direction || v2(axis) != direction ){
					triangles += v0
					triangles += v1
					triangles += v2
				}
				
				if( v0(axis) != direction || v2(axis) !=direction || v3(axis) != direction ){
					triangles += v0
					triangles += v2
					triangles += v3
				}
			}
			
			i += 1
		}
		
		triangles.result
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
	
	def allTriangles = {
		val builder = new VectorBuilder[Vec3]
		builder.sizeHint(36)
		for(axis ← 0 to 2; dir ← 0 to 1) {
			val t = planetriangles(axis,dir)
			if(t(0) != t(1) && t(1) != t(2) && t(2) != t(0)) {
				builder += t(0)
				builder += t(1)
				builder += t(2)
			}
			if(t(3) != t(4) && t(4) != t(5) && t(5) != t(3)) {
				builder += t(3)
				builder += t(4)
				builder += t(5)
			}
		}
		builder.result()
	}
}




