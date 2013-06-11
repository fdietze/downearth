package downearth.rendering

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL14._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL20._

import java.nio.ByteBuffer

/**
 * User: arne
 * Date: 29.04.13
 * Time: 21:55
 */

object Texture {
  glActiveTexture(GL_TEXTURE0 + 0);

  glActiveTexture(GL_TEXTURE1)
}

class Texture(val width:Int, val height:Int, pixels:ByteBuffer) {
  var id = glGenTextures()

  glBindTexture(GL_TEXTURE_2D, id)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE)
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, width, height, 0, GL_BGRA, GL_UNSIGNED_BYTE, pixels)

  glBindTexture(GL_TEXTURE_2D, 0)

  def bind() {
    assert( id != 0 )
    glBindTexture(GL_TEXTURE_2D, id)
  }

  def delete() {
    assert( id != 0 )
    glDeleteTextures(id)
    id = 0
  }

  override def finalize() {
    if( id != 0 ) {
      delete()
    }
  }
}
