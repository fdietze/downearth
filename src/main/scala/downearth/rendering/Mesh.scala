package downearth.rendering

import org.lwjgl.opengl.ARBBufferObject._
import org.lwjgl.opengl.ARBVertexBufferObject._

import simplex3d.math.double.{Vec2,Vec3,Vec4}
import simplex3d.data._
import simplex3d.data.double._
import simplex3d.math.integration.RFloat

import org.lwjgl.BufferUtils

import java.nio._

import downearth.util._
import downearth.worldoctree.NodeUnderMesh

// Klassen zur verwaltung von VertexArrays. Sie kapseln zum einen die Daten, und
// erlauben einen vereinfachten Zugriff und Manipulation, zum anderen übernehmen
// sie die Kommunikation mit der Grafikkarte.

trait Mesh {
	def bind()
	def genvbo()
	def freevbo()

  def hasVbo:Boolean
	def byteSize:Int

  def isEmpty = byteSize == 0
  def nonEmpty = byteSize > 0

  override def finalize() {
    if(hasVbo) println("Mesh Warning: forgot to free Vbo")
  }
}

class ObjMesh(val data:FloatBuffer, val indices:IntBuffer) extends Mesh {
	var vbo = 0
  var vibo = 0
	def size = indices.limit

  def byteSize: Int = ???

  def hasVbo = (vbo > 0 && vibo > 0)

  require(size > 0)

  val stride = 8*4
  val posOffset      = 0
  val texCordsOffset = 3*4
  val normalOffset   = 5*4

  val posComponents = 3
  val texCoordComponents = 2

	def bind() {
		assert(vbo != 0 && vibo != 0)
		glBindBufferARB(GL_ARRAY_BUFFER_ARB, vbo)
    glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, vibo)
	}

  def unbind() {
    glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0)
    glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, 0) 
  }

	def genvbo() {
		freevbo
		// vbo with size of 0 can't be initialized
		
		vbo = glGenBuffersARB()
		glBindBufferARB(GL_ARRAY_BUFFER_ARB, vbo)
		glBufferDataARB(GL_ARRAY_BUFFER_ARB, data, GL_STATIC_DRAW_ARB)
		glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0)

    vibo = glGenBuffersARB()
    glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, vibo);
    glBufferDataARB(GL_ELEMENT_ARRAY_BUFFER_ARB, indices, GL_STATIC_DRAW_ARB);
    glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, 0)
		
    assert( hasVbo )
	}
	
	def freevbo() {
		if( vbo > 0 ) {
			glDeleteBuffersARB( vbo )
      glDeleteBuffersARB( vibo )
			vbo = 0
      vibo = 0
		}
	}
}

@deprecated("a","b") case class TextureMeshData(
			vertexArray:Array[Vec3],
			normalArray:Array[Vec3],
			texcoordsArray:Array[Vec2]
//			colorArray:Array[Vec4]
		) {
	def size = vertexArray.size
  def stride = 32

  def toByteBuffer = {
    val byteBuffer = BufferUtils.createByteBuffer(vertexArray.size * stride)
    val vertices = DataView[Vec3,RFloat](byteBuffer, 0, 8)
    val normals  = DataView[Vec3,RFloat](byteBuffer, 3, 8)
    val texcoords= DataView[Vec2,RFloat](byteBuffer, 6, 8)

    for(i <- 0 until vertexArray.size){
      vertices(i) = vertexArray(i)
      normals(i) = normalArray(i)
      texcoords(i) = texcoordsArray(i)
    }
    byteBuffer
  }
}

import scala.collection.mutable.ArrayBuilder

case class TextureMeshBuilder(
			vertexBuilder:ArrayBuilder[Vec3] = ArrayBuilder.make[Vec3],
			normalBuilder:ArrayBuilder[Vec3] = ArrayBuilder.make[Vec3],
			texCoordBuilder:ArrayBuilder[Vec2] = ArrayBuilder.make[Vec2]
//			colorBuilder:ArrayBuilder[Vec4] = ArrayBuilder.make[Vec4]
			) {
	def result = TextureMeshData(
					vertexBuilder.result,
					normalBuilder.result,
					texCoordBuilder.result
//					colorBuilder.result
					)
}

// A <: B <: MeshData => Patch[A] <: Patch[B]
object Update {
  val byteStride = 32 //TODO sizeOf
  def apply(offset:Int, oldSize:Int, data:TextureMeshData) = {
    require(offset >= 0)
    require(oldSize >= 0)
    new Update(offset * byteStride, oldSize * byteStride, data.toByteBuffer)
  }
}

case class Update(byteOffset:Int, byteOldSize:Int, data:ByteBuffer) {
  //TODO require( effect != 'NOTHING )
  require( byteOffset >= 0, toString)
  require( byteOldSize >= 0, toString)

  def byteSize = data.width
  def byteSizeDifference = byteSize - byteOldSize
  def effect = if(byteOldSize == 0)
    if(byteSize == 0)
      'NOTHING
    else
      'INSERT
  else // byteOldSize > 0
    if(byteSize == 0)
      'DELETE
    else
      'REPLACE

  override def toString = s"Update(offset=$byteOffset, oldSize=$byteOldSize, dataWidth=$byteSize ${effect.name})"
}

case class UpdateInfo(node:NodeUnderMesh, oldByteOffset:Int, oldByteSize:Int, newByteSize:Int) {
  require( oldByteOffset >= 0 )
  require( oldByteSize >= 0 )
  require( newByteSize >= 0 )
}

// Diese Klasse wird verwendet, um den Octree darzustellen. Er repräsentiert ein
// Mesh und stellt Methoden zur Verfügung, die es erlauben das Mesh über Updates
// zu verändern.
object TextureMesh {
  //TODO: global byteStride and Attributes definition
  val byteStride = 32 //TODO sizeOf

  def apply(data:TextureMeshData) = new TextureMesh(data.toByteBuffer)
	
	def apply(meshes:Array[TextureMesh]) = {
		val byteSize = (0 /: meshes)(_ + _.data.width)
    val byteBuffer = BufferUtils.createByteBuffer(byteSize)
    // TODO we need to call put2 everywhere
    (byteBuffer /: meshes)( _ put2 _.data).flip
		new TextureMesh(byteBuffer)
	}
}

// die Basisklasse TextureMesh kann serialisiert werden, und somit auch auf der
// Festplatte gespeichert werden.
class TextureMesh(val data:ByteBuffer) extends Mesh {

  require(data.position == 0)

  @deprecated("a","b") def vertices  = DataView[Vec3,RFloat](data, 0, 8)
  @deprecated("a","b") def normals   = DataView[Vec3,RFloat](data, 3, 8)
  @deprecated("a","b") def texcoords = DataView[Vec2,RFloat](data, 6, 8)
	@deprecated("a","b") private var msize = vertices.size

  var vbo = 0

  def hasVbo = vbo > 0

  def bind() {
    glBindBufferARB(GL_ARRAY_BUFFER_ARB, vbo)
  }

  //@deprecated("a","b") def size = msize
  def byteSize = data.width
  def vertexCount = byteSize / TextureMesh.byteStride
	
	def genvbo() {
		freevbo()
		// vbo with size of 0 can't be initialized
		if( nonEmpty ) {
			vbo = glGenBuffersARB()
			glBindBufferARB(GL_ARRAY_BUFFER_ARB, vbo)
			glBufferDataARB(GL_ARRAY_BUFFER_ARB, data, GL_STATIC_DRAW_ARB)
			glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0)
		}
	}
	
	def freevbo() {
		if( hasVbo ) {
			glDeleteBuffersARB(vbo)
			vbo = 0
		}
	}

  // fügt mehrere Updates in den Hexaeder ein. Hier ist es Sinnvoll alle
  // Updates erst zusammenzuführen, um sie dann alle in einem Schritt in den
  // Hexaeder einzufügen.
  def applyUpdates(updates:Iterable[Update]): TextureMesh = {

    println(updates.mkString("\n"))
    // First patch inserts all the old data
    require(data.position == 0)
    var newDataParts:List[ByteBuffer] = List(data)

    implicit class RichViewSplit(viewList:List[ByteBuffer]) {
      def viewsplit(splitPos:Int) = {
        var destOffset = 0
        val (pre,other) = viewList.span {
          buffer =>
            if( destOffset + buffer.width <= splitPos ) {
              destOffset += buffer.width
              true
            }
            else
              false
        }

        if( destOffset < splitPos ) {
          println(destOffset,splitPos)
          val (left,right) = other.head.split(splitPos-destOffset) //DODO pass absolute splitpos
          (pre :+ left, right :: other.tail)
        }
        else
          (pre, other)
      }
    }

    // apply patches sequentially
    for(update <- updates if(update.effect != 'NOTHING)) {
      val (pre, other) = newDataParts.viewsplit(update.byteOffset)
      val post = other.viewsplit(update.byteOffset + update.byteOldSize)._2
      newDataParts = pre ::: update.data :: post
      println("patches: " + newDataParts.mkString("\n"))
    }

    // merge parts of data to one buffer
    val newSize = (0 /: newDataParts)(_ + _.width)
    val newBuffer = BufferUtils.createByteBuffer(newSize)
    (newBuffer /: newDataParts)(_ put _).flip()



    val mesh = new TextureMesh(newBuffer)
    mesh.genvbo()
    mesh
  }

  // TODO chunksize in Bytes
  def split(chunksizes:Array[Int]) = {
    var index = 0
    for(chunksize ← chunksizes) yield {
      val byteWidth = chunksize * 8 * 4
      val byteBuffer = BufferUtils.createByteBuffer(byteWidth)

      val chunk = data.duplicate()
      chunk.position(index)
      chunk.limit(index+byteWidth)
      byteBuffer.put(chunk).flip()

      index += chunksize * 8 * 4
      new TextureMesh(byteBuffer)
    }
  }
}

