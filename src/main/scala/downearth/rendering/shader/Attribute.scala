package downearth.rendering.shader

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:45
 */

import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import downearth.util.AddString
import org.lwjgl.opengl.Util

import scala.reflect.runtime.universe._
import org.lwjgl.BufferUtils
import simplex3d.math.floatx.{Vec4f, Vec3f, Vec2f}

class BufferBinding(val buffer:ArrayGlBuffer, val size:Int, val glType:Int, val normalized:Boolean, val stride:Int, val offset:Int){
  require( size == 1 || size == 2 || size == 3 || size == 4 || size == GL_BGRA )
  //require( stride > 0 )
}

abstract class Attribute[T](val size:Int, val glType:Int)  extends AddString {
  val program:Program
  val name:CharSequence
  val location:Int

  glEnableVertexAttribArray(location) // is there ever a reason to disable an attribArray?

  if( size != 1 ) {
    throw new NotImplementedError("arrays not yet implemented")
  }

  val bufferBinding = {
    val b = new ArrayGlBuffer()
    b.create()
    b.bind()
    val bb = new BufferBinding(
      buffer = b,
      size = glType match {
        case GL_FLOAT | GL_INT => 1
        case GL_FLOAT_VEC2 => 2
        case GL_FLOAT_VEC3 => 3
        case GL_FLOAT_VEC4 => 4
        case _ => ??? // TODO implement other types
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
    glBindBuffer(b.target,0)
    Util.checkGLError()
    bb
  }
  // TODO find a solution for interleaved data
  def :=(seq:Seq[T])

  // TODO is size/type from glVertexAtribPointer and glGetActiveAttrib different?

  def writeData() {
    val bb = bufferBinding
    glVertexAttribPointer(location, bb.size, bb.glType, bb.normalized, bb.stride, bb.offset)
  }

  override def addString(sb:StringBuilder) = {
    sb append s"layout(location = $location) attribute ${Program.shaderTypeString(glType)} $name ${if(size > 1) (s"[$size]") else ""}"
  }

}

class AttributeInt(val program:Program, val name:CharSequence, val location:Int) extends Attribute[Int](size = 1, glType = GL_INT) {
  def := (seq:Seq[Int]) {
    val data = BufferUtils.createIntBuffer(seq.size * 1)
    for( v <- seq ) {
      data.put(v)
    }
    data.flip()

    bufferBinding.buffer.bind {
      bufferBinding.buffer.load(data)
    }
  }
}

class AttributeFloat(val program:Program, val name:CharSequence, val location:Int) extends Attribute[Float](size = 1, glType = GL_FLOAT) {
  def := (seq:Seq[Float]) {
    val data = BufferUtils.createFloatBuffer(seq.size * 1)
    for( v <- seq ) {
      data.put(v)
    }
    data.flip()

    bufferBinding.buffer.bind {
      bufferBinding.buffer.load(data)
    }
  }
}

class AttributeVec2f(val program:Program, val name:CharSequence, val location:Int) extends Attribute[Vec2f](size = 1, glType = GL_FLOAT_VEC2 ) {
  def := (seq:Seq[Vec2f]) {
    val data = BufferUtils.createFloatBuffer(seq.size * 2)
    for( v <- seq ) {
      data.put(v.x)
      data.put(v.y)
    }
    data.flip()

    bufferBinding.buffer.bind {
      bufferBinding.buffer.load(data)
    }
  }
}

class AttributeVec3f(val program:Program, val name:CharSequence, val location:Int) extends Attribute[Vec3f](size = 1, glType = GL_FLOAT_VEC3 ) {
  def := (seq:Seq[Vec3f]) {
    val data = BufferUtils.createFloatBuffer(seq.size * 3)
    for( v <- seq ) {
      data.put(v.x)
      data.put(v.y)
      data.put(v.z)
    }

    data.flip()
    bufferBinding.buffer.bind {
      bufferBinding.buffer.load(data)
    }
  }
}

class AttributeVec4f(val program:Program, val name:CharSequence, val location:Int) extends Attribute[Vec4f](size = 1, glType = GL_FLOAT_VEC4 ) {
  def := (seq:Seq[Vec4f]) {
    val data = BufferUtils.createFloatBuffer(seq.size * 4)
    for( v <- seq ) {
      data.put(v.x)
      data.put(v.y)
      data.put(v.z)
      data.put(v.w)
    }

    data.flip()
    bufferBinding.buffer.bind {
      bufferBinding.buffer.load(data)
    }
  }
}
