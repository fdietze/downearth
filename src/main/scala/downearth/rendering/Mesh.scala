package downearth.rendering

import org.lwjgl.opengl.GL15._

import simplex3d.math.double.{Vec2,Vec3,Vec4}
import simplex3d.math.integration.RFloat

import org.lwjgl.BufferUtils

import java.nio._

import downearth.util._
import downearth.worldoctree.NodeUnderMesh
import org.lwjgl.opengl.GL11
import simplex3d.math.floatx.{Vec3f, Vec2f}
import collection.mutable
import mutable.ArrayBuilder

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
		glBindBuffer(GL_ARRAY_BUFFER, vbo)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vibo)
	}

  def unbind() {
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0)
  }

	def genvbo() {
		freevbo
		// vbo with size of 0 can't be initialized
		
		vbo = glGenBuffers()
		glBindBuffer(GL_ARRAY_BUFFER, vbo)
		glBufferData(GL_ARRAY_BUFFER, data, GL_STATIC_DRAW)
		glBindBuffer(GL_ARRAY_BUFFER, 0)

    vibo = glGenBuffers()
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vibo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices, GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0)
		
    assert( hasVbo )
	}
	
	def freevbo() {
		if( vbo > 0 ) {
			glDeleteBuffers( vbo )
      glDeleteBuffers( vibo )
			vbo = 0
      vibo = 0
		}
	}
}

case class Update(byteOffset:Int, byteOldSize:Int, data:ByteBuffer) {
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

  def before(that:Update) = this.byteOffset + this.byteOldSize <= that.byteOffset

  override def toString = s"Update(offset=$byteOffset, oldSize=$byteOldSize, dataWidth=$byteSize ${effect.name})"
}

case class UpdateInfo(node:NodeUnderMesh, oldByteOffset:Int, oldByteSize:Int, newByteSize:Int) {
  require( oldByteOffset >= 0 )
  require( oldByteSize >= 0 )
  require( newByteSize >= 0 )

  override def toString = s"UpdateInfo(oldOffset=$oldByteOffset, newSize=$newByteSize, node=$node)"
}

case class TextureMeshBuilder(
  vertexBuilder:ArrayBuilder[Vec3f] = ArrayBuilder.make[Vec3f],
  normalBuilder:ArrayBuilder[Vec3f] = ArrayBuilder.make[Vec3f],
  texCoordBuilder:ArrayBuilder[Vec2f] = ArrayBuilder.make[Vec2f]
) {
  def result:ByteBuffer = result(direct = false)
  def result(direct:Boolean = false) = {
    val verts = vertexBuilder.result()
    val normals = normalBuilder.result()
    val texCoords = texCoordBuilder.result()
    require( verts.length == normals.length && normals.length == texCoords.length )

    val numVerts = verts.length
    val byteSize = numVerts * TextureMesh.byteStride
    val buffer = (if(direct) ByteBuffer.allocateDirect(byteSize) else ByteBuffer.allocate(byteSize)).order(ByteOrder.nativeOrder)

    for(i <- 0 until numVerts){
      glwrapper.util.putVec3f( buffer, verts(i) )
      glwrapper.util.putVec3f( buffer, normals(i) )
      glwrapper.util.putVec2f( buffer, texCoords(i) )
    }

    buffer.flip()
    buffer
  }
}

// Diese Klasse wird verwendet, um den Octree darzustellen. Er repräsentiert ein
// Mesh und stellt Methoden zur Verfügung, die es erlauben das Mesh über Updates
// zu verändern.
object TextureMesh {
  @inline def vertexOffset   = 0
  @inline def vertexType     = GL11.GL_FLOAT
  @inline def normalOffset   = 12
  @inline def normalType     = GL11.GL_FLOAT
  @inline def texCoordOffset = 24
  @inline def texCoordType   = GL11.GL_FLOAT
  @inline def byteStride     = 32

  def apply(data:TextureMeshBuilder) = new TextureMesh(data.result(direct=true))
	
	def apply(meshes:Array[TextureMesh]) = {
		val byteSize = (0 /: meshes)(_ + _.byteSize)
    val byteBuffer = BufferUtils.createByteBuffer(byteSize)
    // TODO we need to call put2 everywhere
    (byteBuffer /: meshes)( _ put2 _.data).flip
		new TextureMesh(byteBuffer)
	}

  val empty = new TextureMesh(BufferUtils.createByteBuffer(0))
}

// die Basisklasse TextureMesh kann serialisiert werden, und somit auch auf der
// Festplatte gespeichert werden.
class TextureMesh(_data:ByteBuffer) extends Mesh {
  val data = _data.asReadOnlyBuffer()

  require(data.isDirect)
  require(data.position == 0)
  require(data.limit == data.capacity)

  override def toString = s"TextureMesh(dataWidth=${data.width})"

  var vbo = 0

  def hasVbo = vbo > 0

  def bind() {
    glBindBuffer(GL_ARRAY_BUFFER, vbo)
  }

  def byteSize = data.width
  def vertexCount = byteSize / TextureMesh.byteStride
	
	def genvbo() {
		freevbo()
		// vbo with size of 0 can't be initialized
		if( nonEmpty ) {
			vbo = glGenBuffers()
			glBindBuffer(GL_ARRAY_BUFFER, vbo)
			glBufferData(GL_ARRAY_BUFFER, data, GL_STATIC_DRAW)
			glBindBuffer(GL_ARRAY_BUFFER, 0)
		}
	}
	
	def freevbo() {
		if( hasVbo ) {
			glDeleteBuffers(vbo)
			vbo = 0
		}
	}

  def applyUpdate(update:Update) = applyUpdates(List(update))

  // fügt mehrere Updates in den Hexaeder ein. Hier ist es Sinnvoll alle
  // Updates erst zusammenzuführen, um sie dann alle in einem Schritt in den
  // Hexaeder einzufügen.
  def applyUpdates(dependentUpdates:Seq[Update]): TextureMesh = {
    require(data.position == 0)

    val newBuffer = data.applyUpdates(dependentUpdates)

    val mesh = new TextureMesh(newBuffer)
    mesh.genvbo()

    mesh
  }

  def split(chunkByteSizes:Array[Int]) = {
    assert(chunkByteSizes.sum == data.width, s"chunkSizes do not sum up to meshSize:\nmesh: ${data.width}\nchunks: $chunkByteSizes")
    var index = 0
    for(byteWidth <- chunkByteSizes) yield {
      val byteBuffer = BufferUtils.createByteBuffer(byteWidth)

      val chunk = data.duplicate()
      chunk.position(index)
      chunk.limit(index+byteWidth)
      byteBuffer.put(chunk).flip()

      assert(data.position == 0)
      assert(data.limit == data.capacity)

      index += byteWidth
      new TextureMesh(byteBuffer)
    }
  }
}

