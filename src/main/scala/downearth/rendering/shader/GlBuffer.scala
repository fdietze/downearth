package downearth.rendering.shader

//import org.lwjgl.opengl.GL15._
import simplex3d.backend.lwjgl.ArbEquivalents.GL15._
import java.nio.{IntBuffer, FloatBuffer, ByteBuffer}

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:50
 */

abstract class GlBuffer() {
  var id:Int = 0
  var hasData = false

  def create() {
    id = glGenBuffers()
  }

  def target:Int
  var usage = GL_STATIC_DRAW

  def bind() {
    require(id != 0)
    glBindBuffer(target, id)
  }

  def bind( env: => Unit ) {
    require(id != 0)
    glBindBuffer(target, id)
    env
    glBindBuffer(target, 0)
  }

  def delete() {
    glDeleteBuffers(id)
    id = 0
  }

  def load( buffer:ByteBuffer ) {
    require( buffer.position() == 0 )
    hasData = true
    glBufferData(target, buffer, usage)
  }

  def load( buffer:FloatBuffer ) {
    require( buffer.position() == 0 )
    hasData = true
    glBufferData(target, buffer, usage)
  }

  def load( buffer:IntBuffer ) {
    require( buffer.position() == 0 )
    hasData = true
    glBufferData(target, buffer, usage)
  }
}

class ArrayGlBuffer extends GlBuffer {
  def target = GL_ARRAY_BUFFER
}

class ElementArrayGlBuffer extends GlBuffer {
  def target = GL_ELEMENT_ARRAY_BUFFER
}
