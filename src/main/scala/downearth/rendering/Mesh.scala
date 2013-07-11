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

// Klassen zur verwaltung von VertexArrays. Sie kapseln zum einen die Daten, und
// erlauben einen vereinfachten Zugriff und Manipulation, zum anderen übernehmen
// sie die Kommunikation mit der Grafikkarte.

trait Mesh {
	def bind()
	def genvbo()
	def freevbo()

  def hasVbo:Boolean
	def size:Int
}

class ObjMesh(val data:FloatBuffer, val indices:IntBuffer) extends Mesh {
	var vbo = 0
  var vibo = 0
	def size = indices.limit
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

case class TextureMeshData(
			vertexArray:Array[Vec3],
			normalArray:Array[Vec3],
			texcoordsArray:Array[Vec2]
//			colorArray:Array[Vec4]
		) {
	def size = vertexArray.size
  def stride = 32

  def toByteBuffer = {
    val byteBuffer = BufferUtils.createByteBuffer(vertexArray.length * stride)
    val vertices = DataView[Vec3,RFloat](byteBuffer, 0, 8)
    val normals  = DataView[Vec3,RFloat](byteBuffer, 3, 8)
    val texcoords= DataView[Vec2,RFloat](byteBuffer, 6, 8)

    for(i <- 0 until vertexArray.length){
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
  val byteStride = 32
  def apply(offset:Int, oldSize:Int, data:TextureMeshData) = new Update(offset * byteStride, oldSize * byteStride, data.toByteBuffer)
}
case class Update(byteOffset:Int, byteOldSize:Int, data:ByteBuffer) {
  def byteSizeDifference = data.width - byteOldSize
  @deprecated("a","b") def vertexCount = data.width / Update.byteStride
  @deprecated("a","b") def oldSize     = byteOldSize / Update.byteStride
  @deprecated("a","b") def sizeDifference = byteSizeDifference / Update.byteStride

}



// Diese Klasse wird verwendet, um den Octree darzustellen. Er repräsentiert ein
// Mesh und stellt Methoden zur Verfügung, die es erlauben das Mesh über Updates
// zu verändern.
object TextureMesh {

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

  @deprecated("a","b") def vertices  = DataView[Vec3,RFloat](data, 0, 8)
  @deprecated("a","b") def normals   = DataView[Vec3,RFloat](data, 3, 8)
  @deprecated("a","b") def texcoords = DataView[Vec2,RFloat](data, 6, 8)
	@deprecated("a","b") private var msize = vertices.size

  var vbo = 0

  def hasVbo = vbo > 0

  def bind() {
    glBindBufferARB(GL_ARRAY_BUFFER_ARB, vbo)
  }

	def size = msize
	
	def genvbo() {
		freevbo()
		// vbo with size of 0 can't be initialized
		if( size > 0 ) {
			vbo = glGenBuffersARB()
			glBindBufferARB(GL_ARRAY_BUFFER_ARB, vbo)
			glBufferDataARB(GL_ARRAY_BUFFER_ARB, data, GL_STATIC_DRAW_ARB)
			glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0)
		}
	}
	
	def freevbo() {
		if( vbo > 0 ) {
			glDeleteBuffersARB(vbo)
			vbo = 0
		}
	}

  // fügt mehrere Updates in den Hexaeder ein. Hier ist es Sinnvoll alle
  // Updates erst zusammenzuführen, um sie dann alle in einem Schritt in den
  // Hexaeder einzufügen.
  def applyUpdates(updates:Iterable[Update]): TextureMesh = {
    val oldvertices = vertices
    /*val oldnormals = normals
    val oldcoords = texcoords*/

    // First patch inserts all the old data
    var patches:List[ByteBuffer] = List(oldvertices.bindingBuffer())

    implicit class RichViewSplit(viewlist:List[ByteBuffer]) {
      def viewsplit(pos:Int) = {
        var destoffset = 0
        val (pre,other) = viewlist.span {
          buffer =>
            if( destoffset + buffer.width <= pos ) {
              destoffset += buffer.width
              true
            }
            else
              false
        }

        if( destoffset < pos ) {
          val (left,right) = other.head.split(pos-destoffset)
          (pre :+ left ,right :: other.tail)
        }
        else
          (pre, other)
      }
    }

    // perpare updates to be applied sequentially
    for(update <- updates) {
      val (pre, other) = patches.viewsplit(update.byteOffset)
      val post = other.viewsplit(update.byteOldSize)._2
      patches = pre ::: update.data :: post
    }


    val newSize = (0 /: patches)(_ + _.width)
    val newBuffer = BufferUtils.createByteBuffer(newSize)
    (newBuffer /: patches)(_ put _).flip()



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

