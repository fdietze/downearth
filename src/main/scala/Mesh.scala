package xöpäx

import org.lwjgl.opengl.GL11._

import org.lwjgl.opengl.ARBBufferObject._
import org.lwjgl.opengl.ARBVertexBufferObject._

import simplex3d.math.float.{Vec3,Vec2}
import simplex3d.math.Vec3i

import simplex3d.data._
import simplex3d.data.float._

trait Mesh extends Serializable{
	var vertexBufferObject:Int = 0
	def genvbo
	def freevbo
	def draw
	def free = glDeleteBuffersARB(vertexBufferObject)
	def size:Int
}
 
trait MutableMesh[T <: MeshData] extends Mesh{
	def patch(patches:Iterable[Patch[T]])
}

trait MeshData{
	def size:Int
}

trait MeshBuilder[T <: MeshData]{
	def result : T
}

case class TextureMeshData(
			vertexArray:Array[Vec3],
			normalArray:Array[Vec3],
			texcoordsArray:Array[Vec2]) extends MeshData{
	def size = vertexArray.size
}

import scala.collection.mutable.ArrayBuilder

case class TextureMeshBuilder(
			vertexBuilder:ArrayBuilder[Vec3] = ArrayBuilder.make[Vec3],
			normalBuilder:ArrayBuilder[Vec3] = ArrayBuilder.make[Vec3],
			texCoordBuilder:ArrayBuilder[Vec2] = ArrayBuilder.make[Vec2]
			) extends MeshBuilder[TextureMeshData] {
	def result = TextureMeshData(
					vertexBuilder.result,
					normalBuilder.result,
					texCoordBuilder.result)
}

// A <: B <: MeshData => Patch[A] <: Patch[B]
case class Patch[+T <: MeshData](pos:Int,size:Int,data:T){
	//the difference of the size after the patch has been applied
	def sizedifference = data.size - size
}

/*
object EmptyMeshData extends MeshData{
	override def size = 0
}

object EmptyPatch extends Patch[Nothing](0,0,null)
*/

class MutableTextureMesh(data:TextureMeshData) extends TextureMesh(data) with MutableMesh[TextureMeshData]{
	import data._

	private var msize = vertexArray.size
	override def size = msize

	def patch(patches:Iterable[Patch[TextureMeshData]]){
		val oldvertices = vertices
		val oldnormals = normals
		val orldcoords = texcoords
		
		val newsize = size + (patches map (_.sizedifference)).sum
		def errorstring = {
			patches map ( p => "replace " + p.size + " with " + p.data.size + "\n" )
		}
		assert(newsize >= 0,"newsize must be greater than or equal to 0\n"+errorstring)
		
		val t = interleave(
			DataSeq[Vec3, RFloat],
			DataSeq[Vec3, RFloat],
			DataSeq[Vec2, RFloat]
			)( newsize )

		vertices  = t._1
		normals   = t._2
		texcoords = t._3

		case class View(offset:Int,size:Int,data:TextureMeshData){
			def split(splitpos:Int) = {
				assert(splitpos > 0 && splitpos < size )
				(View(offset,splitpos,data),View(offset+splitpos,size-splitpos,data))
			}
		}

		var dataview = List(View(0,oldvertices.size,null))

		implicit def richviewsplit(viewlist:List[View]) = new {
			def viewsplit(pos:Int) = {
				var destoffset = 0
				val (pre,other) = viewlist.span{
					v =>
						if( destoffset + v.size <= pos ){
							destoffset += v.size
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

		for(p <- patches) {
			val (pre, other) = dataview.viewsplit(p.pos)
			val (_ , post) = other.viewsplit(p.size)
			dataview = (pre ::: View(0,p.data.size,p.data) :: post)
		}

		var index = 0;
		for(View(offset,size,data) <- dataview){
			if(data != null){
				for(i <- offset until (offset+size) ){
					vertices(index) = data.vertexArray(i)
					normals(index) = data.normalArray(i/3)
					texcoords(index) = data.texcoordsArray(i)
					index += 1
				}
			}
			else{
				vertices.bindingBufferSubData(index,size).put(oldvertices.bindingBufferSubData(offset,size))
				index += size
			}
		}

		msize = newsize
		genvbo
	}
}

object TextureMesh{
	def apply(data:TextureMeshData) = new TextureMesh(data)
	def apply(meshes:Array[TextureMesh]) = {
		val size = meshes.map(_.size).sum
		val (vertices,normals,texcoords) = interleave(
			DataSeq[Vec3, RFloat],
			DataSeq[Vec3, RFloat],
			DataSeq[Vec2, RFloat]
		)(size)
		
		var currentpos = 0
		var currentsize = 0
		
		for(mesh <- meshes){
			currentsize = mesh.size
			vertices.bindingBufferSubData(currentpos,currentsize) put 
				mesh.vertices.bindingBufferSubData(0,currentsize)
			currentpos += currentsize
		}
	}
}

class TextureMesh(data:TextureMeshData) extends Mesh with Serializable {
	import java.io.{ObjectInputStream, ObjectOutputStream, IOException}
	
	
	@throws(classOf[IOException])
	private[this] def writeObject(out:ObjectOutputStream) {
		out.writeInt(msize)
		out.writeObject(new InterleavedData(vertices,normals,texcoords))
	}
	
	@throws(classOf[IOException]) @throws(classOf[ClassNotFoundException])
	private[this] def readObject(in:ObjectInputStream) {
		msize = in.readInt
		val data = in.readObject.asInstanceOf[InterleavedData]
		vertices = data(0).asInstanceOf[DataView[Vec3,RFloat]]
		normals  = data(1).asInstanceOf[DataView[Vec3,RFloat]]
		texcoords= data(2).asInstanceOf[DataView[Vec2,RFloat]]
	}
	
	import data._
	
	@transient var (vertices,normals,texcoords) = interleave(
		DataSeq[Vec3, RFloat],
		DataSeq[Vec3, RFloat],
		DataSeq[Vec2, RFloat]
	)(vertexArray.size)
	
	@transient private var msize = vertexArray.size
	def size = msize
	
	for(i <- 0 until vertexArray.size){
		vertices(i) = vertexArray(i)
		normals(i) = normalArray(i/3)
		texcoords(i) = texcoordsArray(i)
	}
	
	def draw{
		TextureManager.box.bind
		
		if(vertexBufferObject == 0)
			genvbo
		
		if( size > 0 ) {
			glBindBufferARB(GL_ARRAY_BUFFER_ARB, vertexBufferObject)

			glEnableClientState(GL_VERTEX_ARRAY)
			glEnableClientState(GL_NORMAL_ARRAY)
			glEnableClientState(GL_TEXTURE_COORD_ARRAY)

			glVertexPointer(vertices.components, vertices.rawType, vertices.byteStride, vertices.byteOffset)
			glNormalPointer(normals.rawType, normals.byteStride, normals.byteOffset)
			glTexCoordPointer(texcoords.components, texcoords.rawType, texcoords.byteStride, texcoords.byteOffset)

			glDrawArrays(GL_TRIANGLES, 0, vertices.size)

			glDisableClientState(GL_VERTEX_ARRAY)
			glDisableClientState(GL_NORMAL_ARRAY)
			glDisableClientState(GL_TEXTURE_COORD_ARRAY)

			glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0)
		}
	}
	
	def genvbo {
		freevbo
		// es gibt einen Fehler wenn man versucht ein VBO der länge 0 anzulegen
		if( size > 0 ) {
			vertexBufferObject = glGenBuffersARB()
			glBindBufferARB(GL_ARRAY_BUFFER_ARB, vertexBufferObject)
			glBufferDataARB(GL_ARRAY_BUFFER_ARB, vertices.bindingBuffer, GL_STATIC_COPY_ARB)
			glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0)
		}
		else
			vertexBufferObject = -1
	}
	
	def freevbo {
		if( vertexBufferObject > 0 ) {
			glDeleteBuffersARB(vertexBufferObject)
		}
	}
}

