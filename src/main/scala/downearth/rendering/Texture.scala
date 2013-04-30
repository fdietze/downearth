package downearth.rendering

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL14._

import java.nio.ByteBuffer

/**
 * User: arne
 * Date: 29.04.13
 * Time: 21:55
 */

class Texture(val width:Int, val height:Int, pixels:ByteBuffer) {
  val id = glGenTextures()

  glBindTexture(GL_TEXTURE_2D, id)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE)
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, width, height, 0, GL_BGRA, GL_UNSIGNED_BYTE, pixels)

  glBindTexture(GL_TEXTURE_2D, 0)

  var active = true

  def bind {
    assert(active)
    glBindTexture(GL_TEXTURE_2D, id)
  }

  def destroy {
    assert(active)
    active = false
    glDeleteTextures(id)
  }
}
