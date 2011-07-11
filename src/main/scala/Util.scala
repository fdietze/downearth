package xöpäx

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._
import com.bulletphysics.dynamics.RigidBody
import com.bulletphysics.linearmath.Transform
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.glMultMatrix
  
object MyFont{
	import org.newdawn.slick.TrueTypeFont;
	import java.awt.Font;
	val font = new TrueTypeFont(new Font("Monospace", Font.BOLD, 10),true);
}

object Util {
	def indexInRange(i:Vec3i,nodepos:Vec3i,nodesize:Int) = all(lessThan(i,nodepos+nodesize)) && all(greaterThanEqual(i,nodepos))
	
	def isPowerOfTwo(x:Int) = (((x-1) & x) == 0) && x != 0
	val log2:(Int => Int) = {
		case	1 => 0
		case	2 => 1
		case	4 => 2
		case	8 => 3
		case 16 => 4
		case 32 => 5
		case 64 => 6
		case	x =>(log(x)/0.6931471805599453).toInt
	}

	import scala.collection.IterableLike
	import scala.collection.generic.CanBuildFrom

	class RichCollection[A, Repr](xs: IterableLike[A, Repr]){
	  def distinctBy[B, That](f: A => B)(implicit cbf: CanBuildFrom[Repr, A, That]) = {
	    val builder = cbf(xs.repr)
	    val i = xs.iterator
	    var set = Set[B]()
	    while(i.hasNext) {
	      val o = i.next
	      val b = f(o)
	      if (!set(b)) {
	        set += b
	        builder += o
	      }
	    }
	    builder.result
	  }
	}

	implicit def toRich[A, Repr](xs: IterableLike[A, Repr]) = new RichCollection(xs)

	
	class RichVec3i(private[this] val v: inVec3i) { def until(u: inVec3i) = new PermVec3i(v, u) }
	class PermVec3i(val from: inVec3i, val to: inVec3i) extends Iterable[Vec3i] {
		def iterator: Iterator[Vec3i] = new Iterator[Vec3i] {
			val cur = Vec3i(from)
			def hasNext: Boolean = all(lessThan(cur, to))
			def next(): Vec3i = {
				if (!hasNext) throw new NoSuchElementException

				val res = Vec3i(cur)

				cur.z += 1
				if (cur.z >= to.z) {
					cur.z = from.z
					cur.y += 1
					if (cur.y >= to.y) {
						cur.y = from.y
						cur.x += 1
					}
				}

				return res
			}
		}
	}

	implicit def toRichVec2i(u: Vec2i) = new RichVec2i(u)

	class RichVec2i(private[this] val v: inVec2i) { def until(u: inVec2i) = new PermVec2i(v, u) }
	class PermVec2i(val from: inVec2i, val to: inVec2i) extends Iterable[Vec2i] {
		def iterator: Iterator[Vec2i] = new Iterator[Vec2i] {
			val cur = Vec2i(from)
			def hasNext: Boolean = all(lessThan(cur, to))
			def next(): Vec2i = {
				if (!hasNext) throw new NoSuchElementException

				val res = Vec2i(cur)

				cur.y += 1
				if (cur.y >= to.y) {
					cur.y = from.y
					cur.x += 1
				}

				return res
			}
		}
	}

	implicit def toRichVec3i(u: Vec3i) = new RichVec3i(u)
	
	def time[A](msg:String)(foo: => A) = {
		val start = System.nanoTime
		val f = foo
		val t = (System.nanoTime-start)/1000000.
		println(msg+t)
		f
	}
	// Graham Scan algorithm O(n log n)
	def convexHull2d( points:List[Vec2] ) = {
		def ccw(p1:Vec2, p2:Vec2, p3:Vec2) = (p2.x - p1.x)*(p3.y - p1.y) - (p2.y - p1.y)*(p3.x - p1.x) > 0
		def atan2(y:Float,x:Float) = math.atan2(y,x).toFloat

		val stack = collection.mutable.Stack[Vec2]()
		import stack.{push,pop}
		
		val pt0 = points.minBy(_.x)
		val maybeOnHull = points.filter( _ != pt0 )
		val pt1::rest = maybeOnHull.sortBy( v => atan2(v.y-pt0.y,v.x-pt0.x) )
	
		push(pt0)
		push(pt1)
	
		for(p <- rest){
			while(stack.size >= 2 && !ccw(stack(1),stack(0),p))
				pop
			push(p)
		}
		

		stack
	}
	
	def lineHexaederIntersect(linestart:Vec3,linedir:Vec3,h:Hexaeder) = {
		if( (h eq EmptyHexaeder) || (h eq UndefHexaeder) )
			false
		else{
			val q = Seq( Vec3.UnitX,Vec3.UnitY,Vec3.UnitZ ).minBy( v => abs(dot(v,linedir)) )

			val x = normalize( cross(q,linedir) )
			val y = normalize( cross(x,linedir) )

			val m = transpose( Mat3x2(x,y) )

			// line start projected, hexaeder projected
			val projected = (linestart :: h.vertices.toList) map ( m * _ )
			val convexhull = convexHull2d(projected)
			!(convexhull contains projected.head)
		}
	}
	
	def occludes2d(occluder:Set[Vec2], occludee:Set[Vec2]):Boolean = {
		if( occluder == occludee )
			true
		else {
			val convexHull = convexHull2d((occluder ++ occludee).toList).toSet
			if( convexHull == occluder ) // complete occlusion
				true
			else
				false
			
		}
	}

	def multMatrixOfBody(body:RigidBody){
		val transform = new Transform
		val matrixBuffer = BufferUtils.createFloatBuffer(16)
		val matrixArray = new Array[Float](16)

		body getCenterOfMassTransform transform //write to transform
		transform getOpenGLMatrix matrixArray //write to matrixArray
		matrixBuffer put matrixArray
		matrixBuffer flip

		glMultMatrix(matrixBuffer)
	}
	
	def otherAxis(axis:Int) = (1-((axis+1) >> 1), (2 - (axis >> 1)) )
	// 0 => (1,2)
	// 1 => (0,2)
	// 2 => (0,1)
}






















