package downearth.rendering.shader

import org.lwjgl.opengl.GL15._
import java.nio.ByteBuffer

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:50
 */

abstract class GlBuffer() {
  var id:Int = 0

  def create() {
    id = glGenBuffers()
  }

  def target:Int
  var usage = GL_STATIC_DRAW

  def bind() {
    glBindBuffer(target, id)
  }

  def delete() {
    glDeleteBuffers(id)
    id = 0
  }

  def load( buffer:ByteBuffer ) {
    glBufferData(target, buffer, usage)
  }
}

class ArrayGlBuffer extends GlBuffer {
  def target = GL_ARRAY_BUFFER
}

class ElementArrayGlBuffer extends GlBuffer {
  def target = GL_ELEMENT_ARRAY_BUFFER
}
