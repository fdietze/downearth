package downearth

import simplex3d.math._
import integration.RFloat
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._
import com.bulletphysics.dynamics.RigidBody
import com.bulletphysics.linearmath.Transform
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.ARBShaderObjects
import javax.vecmath.{Vector3f,Quat4f}
import org.lwjgl.BufferUtils
import java.nio.FloatBuffer
import java.io._
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.util.Date
import java.text.SimpleDateFormat
import concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global
import downearth.rendering.Draw
import downearth.worldoctree.{UndefHexaeder, EmptyHexaeder, Polyeder, Hexaeder}
import downearth.generation.ChainHull2D

package object util {
	implicit def v2vf(in:Vec3):Vector3f = new Vector3f(in.x.toFloat,in.y.toFloat,in.z.toFloat)
	implicit def vf2v(in:Vector3f):Vec3 =         Vec3(in.x,in.y,in.z)
	implicit def q2qf(in:Quat4) = new Quat4f(in.a.toFloat,in.b.toFloat,in.c.toFloat,in.d.toFloat)
	implicit def qf2q(in:Quat4f) = Quat4(in.w,in.x,in.y,in.z)
  implicit def λ2Runable(in: () => Any ) = new Runnable {
    def run() {
      in.apply()
    }
  }


	implicit def mat2buffer(in:Mat4):FloatBuffer = {
		val data = DataBuffer[Mat4,RFloat](1)
		data(0) = in
		data.buffer
	}

	implicit def vec3_2buffer(in:Vec3):FloatBuffer = {
		val data = DataBuffer[Vec3,RFloat](1)
		data(0) = in
		data.buffer
	}

	implicit def vec4_2buffer(in:Vec4):FloatBuffer = {
		val data = DataBuffer[Vec4,RFloat](1)
		data(0) = in
		data.buffer
	}
	
	implicit def sequence2FloatBuffer(s:Seq[Float]):FloatBuffer = {
		val buffer = BufferUtils.createFloatBuffer(s.size)
		s.foreach(buffer.put)
		buffer.flip
		buffer
	}

  def glTranslate3dv(v:Vec3) = org.lwjgl.opengl.GL11.glTranslated(v.x, v.y, v.z)
  def glTranslate3iv(v:Vec3i) = org.lwjgl.opengl.GL11.glTranslated(v.x, v.y, v.z)
	def glTranslate2iv(v:Vec2i) = org.lwjgl.opengl.GL11.glTranslated(v.x, v.y, 0)
	def glColor4dv(v:Vec4) = org.lwjgl.opengl.GL11.glColor4d(v.r, v.g, v.b, v.a)
	def glScale3dv(v:Vec3) = org.lwjgl.opengl.GL11.glScaled(v.x, v.y, v.z)
	def glScale1d(s:Double) = org.lwjgl.opengl.GL11.glScaled(s,s,s)

  def lerpVec2(a:Vec2, b:Vec2, t:Double) = a + t * (b - a)
  def lerpVec2i(a:Vec2i, b:Vec2i, t:Double) = a + t * (b - a)

	// Testet ob innerhalb eines Quaders, meistens OctreeNodes, eine Position liegt.
	//def indexInRange(i:Vec3i,nodepos:Vec3i,nodesize:Int) = all(lessThan(i,nodepos+nodesize)) && all(greaterThanEqual(i,nodepos))
	def indexInRange(i:Vec3i, nodepos:Vec3i, nodesize:Int) = {
		i.x >= nodepos.x &&
		i.y >= nodepos.y &&
		i.z >= nodepos.z &&
		i.x < nodepos.x + nodesize &&
		i.y < nodepos.y + nodesize &&
		i.z < nodepos.z + nodesize
	}

	def indexInRange(i:Vec3i, nodepos:Vec3i, nodesize:Vec3i) = {
		i.x >= nodepos.x &&
		i.y >= nodepos.y &&
		i.z >= nodepos.z &&
		i.x < nodepos.x + nodesize.x &&
		i.y < nodepos.y + nodesize.y &&
		i.z < nodepos.z + nodesize.z
	}
	
	def indexInRange(i:Vec2i, pos:Vec2i, size:Vec2i) = {
		i.x >= pos.x &&
		i.y >= pos.y &&
		i.x  < pos.x + size.x &&
		i.y  < pos.y + size.y
	}

	def printLogInfo(obj:Int) {
		
		val iVal = BufferUtils.createIntBuffer(1)
		ARBShaderObjects.glGetObjectParameterARB(obj,ARBShaderObjects.GL_OBJECT_INFO_LOG_LENGTH_ARB, iVal);

		val length = iVal.get()
		if(length > 1) {
			// We have some info we need to output.
			val infoLog = BufferUtils.createByteBuffer(length)
			iVal.flip()
			ARBShaderObjects.glGetInfoLogARB(obj, iVal, infoLog)
			val infoBytes = new Array[Byte](length)
			infoLog.get(infoBytes)
			val out = new String(infoBytes)
			println("Info log:\n" + out)
			System.exit(0)
		}
	}
	
	def isPowerOfTwo(x:Int) = (((x-1) & x) == 0) && x != 0
	val log2:(Int => Int) = {
		case  1 => 0
		case  2 => 1
		case  4 => 2
		case  8 => 3
		case 16 => 4
		case 32 => 5
		case 64 => 6
		case  x =>(log(x)/0.6931471805599453).toInt
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
	class PermVec3i(val from: inVec3i, val target: inVec3i) extends Iterable[Vec3i] {
		def iterator: Iterator[Vec3i] = new Iterator[Vec3i] {
			val cur = Vec3i(from)
			def hasNext: Boolean = all(lessThan(cur, target))
			def next(): Vec3i = {
				if (!hasNext) throw new NoSuchElementException

				val res = Vec3i(cur)

				cur.z += 1
				if (cur.z >= target.z) {
					cur.z = from.z
					cur.y += 1
					if (cur.y >= target.y) {
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
	class PermVec2i(val from: inVec2i, val target: inVec2i) extends Iterable[Vec2i] {
		def iterator: Iterator[Vec2i] = new Iterator[Vec2i] {
			val cur = Vec2i(from)
			def hasNext: Boolean = all(lessThan(cur, target))
			def next(): Vec2i = {
				if (!hasNext) throw new NoSuchElementException

				val res = Vec2i(cur)

				cur.y += 1
				if (cur.y >= target.y) {
					cur.y = from.y
					cur.x += 1
				}

				return res
			}
		}
	}

	implicit def toRichVec3i(u: Vec3i) = new RichVec3i(u)

	class Timer {
		var starttime = 0L
		var passedtime = 0L

		def getTime = System.nanoTime

		def start  { starttime = getTime }
		def stop   { passedtime += getTime - starttime }
		def measure[A](function: => A) = {
			start
			val returnvalue = function
			stop
			returnvalue
		}
		def reset  { passedtime = 0 }
		def read =   passedtime/1000000000.0
	}
	
	def time[A](msg:String)(foo: => A) = {
		val start = System.nanoTime
		val f = foo
		val t = (System.nanoTime-start)/1000000.0
		printf("%s: %6.2f ms\n",msg,t)
		f
	}

	// Graham Scan algorithm O(n log n)
	/*
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
	*/
	
	// testet, ob ein Strahl einen Hexaeder trifft, wobei davon ausgegangen 
	// wird, dass sich der Hexaeder im Ursprung das Koordinatensystems befindet.
	def rayPolyederIntersect(rayStart:Vec3,rayDirection:Vec3,h:Polyeder):Boolean = {
		if( (h eq EmptyHexaeder) || (h eq UndefHexaeder) )
			false
		else{
			val q = Seq( Vec3.UnitX,Vec3.UnitY,Vec3.UnitZ ).minBy( v => abs(dot(v,rayDirection)) )

			val x = normalize( cross(q,rayDirection) )
			val y = normalize( cross(x,rayDirection) )
			val m = transpose( Mat2x3(x,y) )

			// alle Vertices werden in richtung des Strahls projeziert
			val projected = Vector.concat( Seq(rayStart), h.vertices ).map( m * _ )
			val projectedStart = projected.head
			
			val convexHull = ChainHull2D( projected )
			
			// enthält die Konvexe Hülle der Projezierten Vertices noch 
			// den projezierten Startpunkt des Strahls, so hat der strahl den 
			// Hexaeder getroffen
			!(convexHull contains projectedStart)
		}
	}
	
	// falls schon sichergestellt wurde, dass der Strahl den Hexaeder trifft, 
	// wird hir noch herausgefunden, ob der strahl eine Aussenwand des Hexaeders
	// trifft oder nicht.
	def rayCellTest(rayStart:Vec3,rayDirection:Vec3,h:Hexaeder):Boolean = {
		val q = Seq( Vec3.UnitX,Vec3.UnitY,Vec3.UnitZ ).minBy( v => abs(dot(v,rayDirection)) )
		val x:Vec3 = normalize( cross(q,rayDirection) )
		val y:Vec3 = normalize( cross(x,rayDirection) )
		val m = transpose( Mat2x3(x,y) )
		
		for(i ← 0 until 6) {
			val axis = i >> 1
			val direction = i & 1
			val (triangle1, triangle2) = h.planetriangles(axis,direction).splitAt(3)
			
			def triangleMax(v0:Vec3, v1:Vec3, v2:Vec3) = {
				(v0(axis) == direction) && (v1(axis) == direction) && (v2(axis) == direction)
			}
			
			for( triangle ← List(triangle1,triangle2) ) {
				val v0 = triangle(0)
				val v1 = triangle(1)
				val v2 = triangle(2)
				// normale des Dreiecks
				val n = cross(v2-v1,v0-v1)
				// falls der Strahl auf die Sichtbare Seite des ersten Dreiecks trifft.
				if( dot(n,rayDirection) <= 0 ) {
					val projected = Vector(m*rayStart, m*v0, m*v1, m*v2)
					val projectedStart = projected(0)
					
					val convexhull = ChainHull2D( projected )
					
					// falls das dreieck getroffen wurde
					if( !( convexhull contains projectedStart ) ){
						// wird ausgegeben, ob es aussen am rand liegt oder nicht
						return triangleMax(v0,v1,v2)
					}
				}
			}
		}
		
		// dieser code hier wird nur erreicht, wenn der strahl den Hexaeder garnicht getroffen hat
		Draw addText "nicht getroffen"
		return false
	}
	
	def occludes2d(occluder:Seq[Vec2], occludee:Seq[Vec2]):Boolean = {
		if( occluder == occludee )
			true
		else {
			val vertices = Vector.concat(occluder,occludee)
			val convexHull = ChainHull2D(vertices)
			if( convexHull.toSet == occluder.toSet ) // complete occlusion
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
		matrixBuffer.flip()

		glMultMatrix(matrixBuffer)
	}
	
	def otherAxis(axis:Int) = (1-((axis+1) >> 1), (2 - (axis >> 1)) )
	// 0 => (1,2)
	// 1 => (0,2)
	// 2 => (0,1)
	
	def round10(a:Double) = math.round(a*10.0)/10.0
	def round10(v:Vec3):Vec3 = Vec3(round10(v.x), round10(v.y), round10(v.z))
	
	var counter = 0
	
	def screenShot(name:String) {
		counter += 1
		glReadBuffer(GL_FRONT)
		val bpp = 4
		val buffer = BufferUtils.createByteBuffer( Main.width.toInt * Main.height.toInt * bpp )
		glReadPixels(0, 0, Main.width.toInt, Main.height.toInt, GL_RGBA, GL_UNSIGNED_BYTE, buffer )

    Future.apply { // save picture in background
			val format = "PNG"
			val now = new Date
			val df = new SimpleDateFormat("yyyy-MM-dd")
			val fullName = (df format now) + "-" + name + "-%03d." + format.toLowerCase
			val file = {
				var f:File = new File("screenshots" , fullName format counter )
				while(f.exists) {
					counter += 1
					f = new File("screenshots" , fullName format counter )
				}
				f
			}
		
			var i      = 0
			val width  = Main.width.toInt
			val height = Main.height.toInt
      val end    = width * height * bpp

      val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
		
			while(i < end) {
				val r = buffer.get(i) & 0xFF;
				val g = buffer.get(i + 1) & 0xFF;
				val b = buffer.get(i + 2) & 0xFF;
				val p = (i/bpp)
				image.setRGB(
					p % width,
					height - 1 - (p / width),
					(0xFF << 24) | (r << 16) | (g << 8) | b
				)
				
				i += bpp
			}
		
			try {
				ImageIO.write(image, format, file);
				DisplayEventManager.showEventText("File "+ file.getName +" saved.")
			} catch {
			 case e : IOException => e.printStackTrace
			}
		}
	}
}
