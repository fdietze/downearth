package downearth.rendering.shader

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:45
 */

import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import downearth.util.AddString
import org.lwjgl.opengl.Util

import scala.reflect.runtime.universe._
import org.lwjgl.BufferUtils
import simplex3d.math.floatx._
import java.nio.ByteBuffer
import downearth.util._
import org.lwjgl.opengl.ARBInstancedArrays._


case class BufferBinding(buffer:ArrayGlBuffer, size:Int, glType:Int, normalized:Boolean, stride:Int, offset:Int) {
  require( size == 1 || size == 2 || size == 3 || size == 4 || size == GL_BGRA )
  require( stride > 0 )

  override def toString = {
    s"BufferBinding($buffer, size: $size, ${Program.shaderTypeString(glType)}, normalized: $normalized, stride: $stride, offset: $offset)"
  }
}

abstract class Attribute[T](val size:Int, val glType:Int)  extends AddString {
  val program:Program
  val binding:Binding
  val name:CharSequence
  val location:Int
  val bufferBinding:BufferBinding

  def enable() { glEnableVertexAttribArray(location) }

  def disable() { glDisableVertexAttribArray(location) }

  def enabled_=(b:Boolean) { if(b) enable() else disable() }

  def enabled = {
    val buffer = sharedIntBuffer(1)
    glGetVertexAttrib(location, GL_VERTEX_ATTRIB_ARRAY_ENABLED, buffer)
    buffer.get(0) == GL_TRUE
  }

  if( size != 1 ) {
    throw new NotImplementedError("arrays not yet implemented")
  }

  // TODO find a solution for interleaved data
  def :=(seq:Seq[T])

  def :=(data:ByteBuffer) {
    bufferBinding.buffer.bind{
      bufferBinding.buffer.putData( data )
    }

    binding.changedAttributes.enqueue(this)
  }

  def read(size:Int) : Seq[T]

  // TODO is size/type from glVertexAtribPointer and glGetActiveAttrib different?

  def writeData() {
    val bb = bufferBinding

    glVertexAttribPointer(location, bb.size, bb.glType, bb.normalized, bb.stride, bb.offset)

    println( name, location, bb );
  }

  override def addString(sb:StringBuilder) = {
    sb append s"layout(location = $location) attribute ${Program.shaderTypeString(glType)} $name ${if(size > 1) (s"[$size]") else ""}"
  }

  def divisor:Int = {
    val data = sharedIntBuffer(1)
    glGetVertexAttrib(location, GL_VERTEX_ATTRIB_ARRAY_DIVISOR_ARB, data)
    data.get(0)
  }

  def divisor_=(i:Int) {
    glVertexAttribDivisorARB(location, i)
  }

}

class AttributeFake[T](val program:Program, val binding:Binding, val name:CharSequence) extends Attribute[T](1, -1) {
  println(this)

  val location:Int = -1
  val bufferBinding:BufferBinding = null

  override def addString(sb:StringBuilder) = {
    sb append "unbound attribute " append name
  }

  override def divisor:Int = -1
  override def divisor_=(i:Int) { }

  override def writeData() {
    assert(false, "should not call this method")
  }

  override def :=(data:ByteBuffer) { }
  override def := (seq:Seq[T]) { }
  override def read(size:Int) = Nil

}

class AttributeInt(val program:Program, val binding:Binding, val name:CharSequence, val location:Int, val bufferBinding:BufferBinding) extends Attribute[Int](size = 1, glType = GL_INT) {

  def := (seq:Seq[Int]) {
    val data = sharedByteBuffer(bufferBinding.stride * seq.size)

    for( (v,i) <- seq.zipWithIndex ) {
      import bufferBinding.{offset,stride}
      data.putInt(offset + i*stride + 0, v)
    }

    bufferBinding.buffer.bind {
      bufferBinding.buffer.putData( data )
    }

    binding.changedAttributes.enqueue(this)
  }

  def read(size:Int) = {
    import bufferBinding.{offset,stride}

    val data = sharedByteBuffer(size * stride)

    bufferBinding.buffer.bind {
      bufferBinding.buffer.getData(data)
    }

    for(i <- 0 until size) yield data.getInt(offset + i*stride + 0)
  }
}

class AttributeFloat(val program:Program, val binding:Binding, val name:CharSequence, val location:Int, val bufferBinding:BufferBinding) extends Attribute[Float](size = 1, glType = GL_FLOAT) {
  def := (seq:Seq[Float]) {
    val data = sharedByteBuffer(bufferBinding.stride * seq.size)

    for( (v,i) <- seq.zipWithIndex ) {
      import bufferBinding.{offset,stride}
      data.putFloat(offset + i*stride + 0, v)
    }

    bufferBinding.buffer.bind{
      bufferBinding.buffer.putData( data )
    }

    binding.changedAttributes.enqueue(this)
  }

  def read(size:Int) = {
    import bufferBinding.{offset,stride}

    val data = sharedByteBuffer(size * stride)

    bufferBinding.buffer.bind {
      bufferBinding.buffer.getData(data)
    }

    for(i <- 0 until size) yield data.getFloat(offset + i*stride + 0)
  }
}

class AttributeVec2f(val program:Program, val binding:Binding, val name:CharSequence, val location:Int, val bufferBinding:BufferBinding) extends Attribute[ReadVec2f](size = 1, glType = GL_FLOAT_VEC2 ) {
  def := (seq:Seq[ReadVec2f]) {
    val data = sharedByteBuffer(bufferBinding.stride * seq.size)

    for( (v,i) <- seq.zipWithIndex ) {
      import bufferBinding.{offset,stride}
      data.putFloat(offset + i*stride + 0, v.x)
      data.putFloat(offset + i*stride + 4, v.y)
    }

    bufferBinding.buffer.bind{
      bufferBinding.buffer.putData( data )
    }

    binding.changedAttributes.enqueue(this)
  }

  def read(size:Int) = {
    import bufferBinding.{offset,stride}

    val data = sharedByteBuffer(size * stride)

    bufferBinding.buffer.bind {
      bufferBinding.buffer.getData(data)
    }

    for(i <- 0 until size) yield ConstVec2f(
      x = data.getFloat(offset + i*stride + 0),
      y = data.getFloat(offset + i*stride + 4)
    )
  }
}

class AttributeVec3f(val program:Program, val binding:Binding, val name:CharSequence, val location:Int, val bufferBinding:BufferBinding) extends Attribute[ReadVec3f](size = 1, glType = GL_FLOAT_VEC3 ) {
  def := (seq:Seq[ReadVec3f]) {
    val data = sharedByteBuffer(bufferBinding.stride * seq.size)

    for( (v,i) <- seq.zipWithIndex ) {
      import bufferBinding.{offset,stride}
      data.putFloat(offset + i*stride + 0, v.x)
      data.putFloat(offset + i*stride + 4, v.y)
      data.putFloat(offset + i*stride + 8, v.z)
    }

    bufferBinding.buffer.bind {
      bufferBinding.buffer.putData( data )
    }

    binding.changedAttributes.enqueue(this)
  }

  def read(size:Int) = {
    import bufferBinding.{offset,stride}

    val data = sharedByteBuffer(size * stride)

    bufferBinding.buffer.bind {
      bufferBinding.buffer.getData(data)
    }

    for(i <- 0 until size) yield ConstVec3f(
      x = data.getFloat(offset + i*stride + 0),
      y = data.getFloat(offset + i*stride + 4),
      z = data.getFloat(offset + i*stride + 8)
    )
  }
}

class AttributeVec4f(val program:Program, val binding:Binding, val name:CharSequence, val location:Int, val bufferBinding:BufferBinding) extends Attribute[ReadVec4f](size = 1, glType = GL_FLOAT_VEC4 ) {
  def := (seq:Seq[ReadVec4f]) {
    val data = sharedByteBuffer(bufferBinding.stride * seq.size)

    for( (v,i) <- seq.zipWithIndex ) {
      import bufferBinding.{offset,stride}
      data.putFloat(offset + i*stride + 0,  v.x)
      data.putFloat(offset + i*stride + 4,  v.y)
      data.putFloat(offset + i*stride + 8,  v.z)
      data.putFloat(offset + i*stride + 12, v.w)
    }

    bufferBinding.buffer.bind{
      bufferBinding.buffer.putData( data )
    }

    binding.changedAttributes.enqueue(this)
  }

  def read(size:Int) = {
    import bufferBinding.{offset,stride}

    val data = sharedByteBuffer(size * stride)

    bufferBinding.buffer.bind {
      bufferBinding.buffer.getData(data)
    }

    for(i <- 0 until size) yield ConstVec4f(
      x = data.getFloat(offset + i*stride + 0),
      y = data.getFloat(offset + i*stride + 4),
      z = data.getFloat(offset + i*stride + 8),
      w = data.getFloat(offset + i*stride + 12)
    )
  }
}

