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
import downearth.util.sizeOf

final class BufferBinding(val buffer:ArrayGlBuffer, val size:Int, val glType:Int, val normalized:Boolean, val stride:Int, val offset:Int) {
  require( size == 1 || size == 2 || size == 3 || size == 4 || size == GL_BGRA )
  var data:ByteBuffer = null

  def load() {
    buffer.load(data)
  }

  def defineCapacity(capacity:Int) {
    if(data == null) {
      data = BufferUtils.createByteBuffer(capacity)
    }
    else if( data.capacity() < capacity  ) {
      val newData = BufferUtils.createByteBuffer(capacity)
      newData.put(data)
      newData.rewind
      data = newData
    }
    else if( data.capacity() > capacity ) {
      val newData = BufferUtils.createByteBuffer(capacity)
      data.limit(capacity)
      newData.put(data)
      newData.rewind
      data = newData
    }
  }

  require( stride > 0 )
}

abstract class Attribute[T](val size:Int, val glType:Int)  extends AddString {
  val program:Program
  val binding:Binding
  val name:CharSequence
  val location:Int
  val bufferBinding:BufferBinding

  glEnableVertexAttribArray(location) // is there ever a reason to disable an attribArray?

  if( size != 1 ) {
    throw new NotImplementedError("arrays not yet implemented")
  }

  // TODO find a solution for interleaved data
  def :=(seq:Seq[T])

  def :=(data:ByteBuffer) {
    bufferBinding.data = data
    binding.changedAttributes.enqueue(this)
  }

  // TODO is size/type from glVertexAtribPointer and glGetActiveAttrib different?

  def writeData() {
    val bb = bufferBinding
    glVertexAttribPointer(location, bb.size, bb.glType, bb.normalized, bb.stride, bb.offset)
    bufferBinding.buffer.bind {
      bufferBinding.load()
    }
  }

  override def addString(sb:StringBuilder) = {
    sb append s"layout(location = $location) attribute ${Program.shaderTypeString(glType)} $name ${if(size > 1) (s"[$size]") else ""}"
  }

}

class AttributeInt(val program:Program, val binding:Binding, val name:CharSequence, val location:Int, val bufferBinding:BufferBinding) extends Attribute[Int](size = 1, glType = GL_INT) {
  def := (seq:Seq[Int]) {
    bufferBinding.defineCapacity(bufferBinding.stride * seq.size)
    val data = bufferBinding.data

    data.clear()
    for( (v,i) <- seq.zipWithIndex ) {
      import bufferBinding.{offset,stride}
      data.putInt(offset + i*stride + 0, v)
    }

    binding.changedAttributes.enqueue(this)
  }
}

class AttributeFloat(val program:Program, val binding:Binding, val name:CharSequence, val location:Int, val bufferBinding:BufferBinding) extends Attribute[Float](size = 1, glType = GL_FLOAT) {
  def := (seq:Seq[Float]) {
    bufferBinding.defineCapacity(bufferBinding.stride * seq.size)
    val data = bufferBinding.data

    data.clear()
    for( (v,i) <- seq.zipWithIndex ) {
      import bufferBinding.{offset,stride}
      data.putFloat(offset + i*stride + 0, v)
    }

    binding.changedAttributes.enqueue(this)
  }
}

class AttributeVec2f(val program:Program, val binding:Binding, val name:CharSequence, val location:Int, val bufferBinding:BufferBinding) extends Attribute[ReadVec2f](size = 1, glType = GL_FLOAT_VEC2 ) {
  def := (seq:Seq[ReadVec2f]) {
    bufferBinding.defineCapacity(bufferBinding.stride * seq.size)
    val data = bufferBinding.data

    data.clear()
    for( (v,i) <- seq.zipWithIndex ) {
      import bufferBinding.{offset,stride}
      data.putFloat(offset + i*stride + 0, v.x)
      data.putFloat(offset + i*stride + 4, v.y)
    }

    binding.changedAttributes.enqueue(this)
  }
}

class AttributeVec3f(val program:Program, val binding:Binding, val name:CharSequence, val location:Int, val bufferBinding:BufferBinding) extends Attribute[ReadVec3f](size = 1, glType = GL_FLOAT_VEC3 ) {
  def := (seq:Seq[ReadVec3f]) {
    bufferBinding.defineCapacity(bufferBinding.stride * seq.size)
    val data = bufferBinding.data

    data.clear()
    for( (v,i) <- seq.zipWithIndex ) {
      import bufferBinding.{offset,stride}
      data.putFloat(offset + i*stride + 0, v.x)
      data.putFloat(offset + i*stride + 4, v.y)
      data.putFloat(offset + i*stride + 8, v.z)
    }

    binding.changedAttributes.enqueue(this)
  }
}

class AttributeVec4f(val program:Program, val binding:Binding, val name:CharSequence, val location:Int, val bufferBinding:BufferBinding) extends Attribute[ReadVec4f](size = 1, glType = GL_FLOAT_VEC4 ) {
  def := (seq:Seq[ReadVec4f]) {
    bufferBinding.defineCapacity(bufferBinding.stride * seq.size)
    val data = bufferBinding.data
    data.clear()
    for( (v,i) <- seq.zipWithIndex ) {
      import bufferBinding.{offset,stride}
      data.putFloat(offset + i*stride + 0,  v.x)
      data.putFloat(offset + i*stride + 4,  v.y)
      data.putFloat(offset + i*stride + 8,  v.z)
      data.putFloat(offset * i*stride + 12, v.w)
    }

    binding.changedAttributes.enqueue(this)
  }
}
