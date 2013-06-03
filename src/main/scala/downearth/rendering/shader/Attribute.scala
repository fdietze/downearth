package downearth.rendering.shader

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:45
 */

import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL14._
import org.lwjgl.opengl.GL20._
import downearth.util.AddString
import scala.collection.mutable.ArrayBuffer

class BufferBinding(val buffer:ArrayGlBuffer, val size:Int, val glType:Int, val normalized:Boolean, val stride:Int, val offset:Int){
  require( size == 1 || size == 2 || size == 3 || size == 4 || size == GL_BGRA )
}

class Attribute(val program:Program, val name:CharSequence, val location:Int, val size:Int, val glType:Int)  extends AddString {
  if( size != 1 ){
    throw new NotImplementedError("arrays not yet implemented")
  }

  val bufferBinding = new BufferBinding(
    buffer = {
      val b = new ArrayGlBuffer()
      b.create()
      b.bind()
      b
    },
    size = glType match {
      case GL_FLOAT => 1
      case GL_FLOAT_VEC2 => 2
      case GL_FLOAT_VEC3 => 3
      case GL_FLOAT_VEC4 => 4
      case _ => 1 // TODO implement other types
    },
    glType = glType match {
      case GL_FLOAT => GL_FLOAT
      case GL_FLOAT_VEC2 => GL_FLOAT
      case GL_FLOAT_VEC3 => GL_FLOAT
      case GL_FLOAT_VEC4 => GL_FLOAT
      case _ => glType // TODO implement other types
    },
    normalized = false,
    stride = 0,
    offset = 0
  )
  // TODO find a solution for interleaved data


  // TODO is size/type from glVertexAtribPointer and glGetActiveAttrib different?

  def writeData() {
    val bb = bufferBinding
    import bb._
    glVertexAttribPointer(location, bb.size, bb.glType, normalized, stride, offset)
  }

  override def addString(sb:StringBuilder) = {
    sb append s"attribute ${Program.shaderTypeString(glType)} $name ${if(size > 1) (s"[$size]") else ""}"
  }

}
