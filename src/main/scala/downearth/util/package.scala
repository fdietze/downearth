package downearth

import simplex3d.math.double.functions._
import simplex3d.math.double._
import simplex3d.math._

import com.bulletphysics.dynamics.RigidBody
import com.bulletphysics.linearmath.Transform

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.{Display, ARBShaderObjects}
import org.lwjgl.BufferUtils

import java.nio.{ByteBuffer, IntBuffer, FloatBuffer}
import java.io._
import java.awt.image.BufferedImage
import java.util.Date
import java.text.SimpleDateFormat
import javax.imageio.ImageIO
import javax.vecmath.{Vector3f,Quat4f}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import downearth.rendering.{Update, GlDraw}
import downearth.worldoctree.{UndefHexaeder, EmptyHexaeder, Polyeder, Hexaeder}
import downearth.generation.ChainHull2D
import simplex3d.math.floatx._
import simplex3d.math.{Vec2i, Vec3i}


package object util {
  def assertionsActivated = try {
    Predef.assert(false)
    false
  } catch {
    case e: AssertionError => true
  }

	implicit def v2vf(in:ReadVec3):Vector3f = new Vector3f(in.x.toFloat,in.y.toFloat,in.z.toFloat)
	implicit def vf2v(in:Vector3f):Vec3 =         Vec3(in.x,in.y,in.z)
	implicit def q2qf(in:ReadQuat4) = new Quat4f(in.a.toFloat,in.b.toFloat,in.c.toFloat,in.d.toFloat)
	implicit def qf2q(in:Quat4f) = Quat4(in.w,in.x,in.y,in.z)
  implicit def λ2Runable(in: () => Any ) = new Runnable {
    def run() {
      in.apply()
    }
  }

//	implicit def mat2buffer(in:Mat4):FloatBuffer = {
//		val data = DataBuffer[Mat4,RFloat](1)
//		data(0) = in
//		data.buffer
//	}
//
//	implicit def vec3_2buffer(in:Vec3):FloatBuffer = {
//		val data = DataBuffer[Vec3,RFloat](1)
//		data(0) = in
//		data.buffer
//	}
//
//	implicit def vec4_2buffer(in:Vec4):FloatBuffer = {
//		val data = DataBuffer[Vec4,RFloat](1)
//		data(0) = in
//		data.buffer
//	}
	
	implicit def sequence2FloatBuffer(s:Seq[Float]):FloatBuffer = {
		val buffer = BufferUtils.createFloatBuffer(s.size)
		s.foreach(buffer.put)
		buffer.flip
		buffer
	}

  def glTranslate3dv(v:ReadVec3) = org.lwjgl.opengl.GL11.glTranslated(v.x, v.y, v.z)
  def glTranslate3iv(v:ReadVec3i) = org.lwjgl.opengl.GL11.glTranslated(v.x, v.y, v.z)
	def glTranslate2iv(v:ReadVec2i) = org.lwjgl.opengl.GL11.glTranslated(v.x, v.y, 0)
	def glColor4dv(v:ReadVec4) = org.lwjgl.opengl.GL11.glColor4d(v.r, v.g, v.b, v.a)
	def glScale3dv(v:ReadVec3) = org.lwjgl.opengl.GL11.glScaled(v.x, v.y, v.z)
	def glScale1d(s:Double) = org.lwjgl.opengl.GL11.glScaled(s,s,s)

  def lerpVec2(a:ReadVec2, b:ReadVec2, t:Double) = a + Vec2(t) * (b - a)
  // def lerpVec2i(a:Vec2i, b:Vec2i, t:Double) = a + t * (b - a)

	// Testet ob innerhalb eines Quaders, meistens OctreeNodes, eine Position liegt.
	//def indexInRange(i:Vec3i,nodepos:Vec3i,nodesize:Int) = all(lessThan(i,nodepos+nodesize)) && all(greaterThanEqual(i,nodepos))
	def indexInRange(i:ReadVec3i, nodepos:ReadVec3i, nodesize:Int) = {
		i.x >= nodepos.x &&
		i.y >= nodepos.y &&
		i.z >= nodepos.z &&
		i.x < nodepos.x + nodesize &&
		i.y < nodepos.y + nodesize &&
		i.z < nodepos.z + nodesize
	}

	def indexInRange(i:ReadVec3i, nodepos:ReadVec3i, nodesize:ReadVec3i) = {
		i.x >= nodepos.x &&
		i.y >= nodepos.y &&
		i.z >= nodepos.z &&
		i.x < nodepos.x + nodesize.x &&
		i.y < nodepos.y + nodesize.y &&
		i.z < nodepos.z + nodesize.z
	}
	
	def indexInRange(i:ReadVec2i, pos:ReadVec2i, size:ReadVec2i) = {
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

  def nextPowerOfTwo(x:Int) = {
    var n = x
    n -= 1
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    n += 1
    n
  }

  def mod(a:Int, b:Int) = (a % b + b) % b
  def mod(a:ReadVec3i, b:Int):Vec3i = Vec3i(
    mod(a.x, b),
    mod(a.y, b),
    mod(a.z, b)
  )
  def mod(a:ReadVec3i, b:ReadVec3i):Vec3i = Vec3i(
    mod(a.x, b.x),
    mod(a.y, b.y),
    mod(a.z, b.z)
  )

  def divFloor(a:Int, b:Int) = (a - mod(a,b)) / b
  def divCeil(a:Int, b:Int) = if(a % b == 0) a / b else (a + ( b - mod(a,b) )) / b

  def divFloor(a:ReadVec3i, b:Int):Vec3i = Vec3i(
    divFloor(a.x, b),
    divFloor(a.y, b),
    divFloor(a.z, b)
  )
  def divFloor(a:ReadVec3i, b:Vec3i):Vec3i = Vec3i(
    divFloor(a.x, b.x),
    divFloor(a.y, b.y),
    divFloor(a.z, b.z)
  )

  def divCeil(a:ReadVec3i, b:Int):Vec3i = Vec3i(
    divCeil(a.x, b),
    divCeil(a.y, b),
    divCeil(a.z, b)
  )
  def divCeil(a:ReadVec3i, b:ReadVec3i):Vec3i = Vec3i(
    divCeil(a.x, b.x),
    divCeil(a.y, b.y),
    divCeil(a.z, b.z)
  )

  def halves(n:Int) = (n/2, n-n/2)
  def halves(v:ReadVec3i) = (Vec3i(
    v.x/2,
    v.y/2,
    v.z/2
  ), Vec3i(
    v.x-v.x/2,
    v.y-v.y/2,
    v.z-v.z/2
  ))

	import scala.collection.IterableLike
	import scala.collection.generic.CanBuildFrom

	implicit class RichCollection[A, Repr](xs: IterableLike[A, Repr]){
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

	implicit class RichVec3i(private[this] val v: ReadVec3i) { def until(u: ReadVec3i) = new PermVec3i(v, u) }
	class PermVec3i(val from: ReadVec3i, val target: ReadVec3i) extends Iterable[Vec3i] {
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

	implicit class RichVec2i(private[this] val v: ReadVec2i) { def until(u: ReadVec2i) = new PermVec2i(v, u) }
	class PermVec2i(val from: ReadVec2i, val target: ReadVec2i) extends Iterable[Vec2i] {
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

  class Timer {
    var startTime = 0L
    var totalPassedTime = 0L

    def now = System.nanoTime
    def passed = now - startTime

    def reset()   { totalPassedTime = 0 }
    def start()   { startTime = now }
    def restart() {reset(); start()}
    def stop()    { totalPassedTime += passed }

    def measure[A](code: => A) = {
      start()
      val returnValue = code
      stop()
      returnValue
    }

    def benchmark(n:Int)(code: => Unit):Double = {
      var i = 0
      start()
      while( i < n ) {
        code
        i += 1
      }
      totalPassedTime += passed / n
      
      return passed.toDouble / n
    }

    def readNanos = if(totalPassedTime == 0) passed else totalPassedTime
    def readMillis = readNanos/1000000
    def readSeconds = readNanos/1000000000.0
    def readHuman:String = readHuman(3)
    def readHuman(precision:Int = 8) = {
      val time = readSeconds
      val fraction = time - math.floor(time)
      var s = time.toInt
      val sb = new StringBuilder
      val d = s / 86400; s -= d*86400
      if( d > 0 ) sb ++= "%dd " format d

      val h = s / 3600; s -= h*3600
      if( h > 0 ) sb ++= "%dh " format h

      val m = s / 60; s -= m*60
      if( m > 0 ) sb ++= "%dm " format m

      sb ++= "%."+precision+"fs" format (s+fraction)
      sb.toString
    }
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
	def rayPolyederIntersect(ray:Ray, h:Polyeder):Boolean = {

    if(h == null) {
      println("penis")
      println("penis")
      println("penis")
    }

		if( (h eq EmptyHexaeder) || (h eq UndefHexaeder) )
			false
		else{
			val q = Seq( Vec3.UnitX,Vec3.UnitY,Vec3.UnitZ ).minBy( v => abs(dot(v,ray.dir)) )

			val x = normalize( cross(q,ray.dir) )
			val y = normalize( cross(x,ray.dir) )
			val m = transpose( Mat2x3(x,y) )

			// alle Vertices werden in richtung des Strahls projeziert
			val projected = Vector.concat( Seq(ray.pos), h.vertices ).map( m * _ )
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
	def rayCellTest(ray:Ray, h:Hexaeder):Boolean = {
		val q = Seq( Vec3.UnitX,Vec3.UnitY,Vec3.UnitZ ).minBy( v => abs(dot(v,ray.dir)) )
		val x:Vec3 = normalize( cross(q,ray.dir) )
		val y:Vec3 = normalize( cross(x,ray.dir) )
		val m = transpose( Mat2x3(x,y) )
		
		for(i ← 0 until 6) {
			val axis = i >> 1
			val direction = i & 1
			val (triangle1, triangle2) = h.planetriangles(axis,direction).splitAt(3)
			
			def triangleMax(v0:ReadVec3, v1:ReadVec3, v2:ReadVec3) = {
				(v0(axis) == direction) && (v1(axis) == direction) && (v2(axis) == direction)
			}
			
			for( triangle ← List(triangle1,triangle2) ) {
				val v0 = triangle(0)
				val v1 = triangle(1)
				val v2 = triangle(2)
				// normale des Dreiecks
				val n = cross(v2-v1,v0-v1)
				// falls der Strahl auf die Sichtbare Seite des ersten Dreiecks trifft.
				if( dot(n,ray.dir) <= 0 ) {
					val projected = Vector(m*ray.pos, m*v0, m*v1, m*v2)
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
		
		// dieser code hier wird nur erreicht, wenn der strahl den Hexaeder garnicht getroffen hat, obwohl dis sicher gestellt ist
		GlDraw addText "nicht getroffen"
		return false
	}
	
	def occludes2d(occluder:IndexedSeq[ReadVec2], occludee:IndexedSeq[ReadVec2]):Boolean = {
		if( occluder == occludee )
			true
		else {
			val vertices = occluder ++ occludee
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
	def round10(v:ReadVec3):Vec3 = Vec3(round10(v.x), round10(v.y), round10(v.z))
	
	var counter = 0
	
	def screenShot(name:String) {
		counter += 1
		val bpp = 4
		val buffer = BufferUtils.createByteBuffer( Display.getWidth * Display.getHeight * bpp )
		glReadPixels(0, 0, Display.getWidth, Display.getHeight, GL_RGBA, GL_UNSIGNED_BYTE, buffer )

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
			val width  = Display.getWidth
			val height = Display.getHeight
      val end    = width * height * bpp

      val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
		
			while(i < end) {
				val r = buffer.get(i) & 0xFF
				val g = buffer.get(i + 1) & 0xFF
				val b = buffer.get(i + 2) & 0xFF
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
			 case e : IOException => e.printStackTrace()
			}
		}
	}

  implicit class RichByteBuffer(buffer:ByteBuffer) {
    def width:Int = buffer.limit - buffer.position

    def split(relativeSplitPos:Int) = {
      val oldPosition = buffer.position
      require( 0 <= relativeSplitPos && relativeSplitPos < buffer.limit - buffer.position,
        s"splitpos not in range.\nSplitPos: $relativeSplitPos\n$buffer")

      val data1:ByteBuffer = buffer.duplicate
      val data2:ByteBuffer = buffer.duplicate
      data1.position(buffer.position).limit(buffer.position + relativeSplitPos)
      data2.position(buffer.position + relativeSplitPos).limit(buffer.limit)

      assert(buffer.position == oldPosition)
      (data1,data2)
    }

    def put2(data:ByteBuffer) = {
      val oldPosition = data.position
      buffer.put(data)
      data.position(oldPosition)
      buffer
    }

    def applyUpdates(dependentUpdates:Seq[Update]) = {
      var resultList = List(buffer)
      for(update <- dependentUpdates) {
        val (pre, _) = resultList.split(update.byteOffset)
        val (_  , post ) = resultList.split(update.byteOffset + update.byteOldSize)
        if( update.data.width == 0 )
          resultList = pre ::: post
        else
          resultList = pre ::: update.data :: post
      }
      resultList.merge
    }
  }

  implicit class ByteBufferList(bufferList:List[ByteBuffer]) {
    def split(absoluteSplitPos:Int) = {
      var offset = 0
      val (pre,other) = bufferList.span{ b =>
        if(offset + b.width <= absoluteSplitPos) {
          offset += b.width
          true
        } else false
      }
      assert(offset <= absoluteSplitPos)

      // bufferList: [....][...][........]
      // splitpos:           |
      // pre:        [....]
      // other:            [...][........]
      // offset-           |
      if( other.isEmpty || offset == absoluteSplitPos )
        (pre, other) // split exactly between buffers
      else {
        val (left,right) = other.head.split(absoluteSplitPos - offset)
        (pre :+ left, right :: other.tail)
      }
    }

    def merge = {
      // merge parts of data to one buffer
      val newSize = (0 /: bufferList)(_ + _.width)
      val newBuffer = BufferUtils.createByteBuffer(newSize)
      (newBuffer /: bufferList)(_ put2 _).flip()

      newBuffer
    }
  }
}

