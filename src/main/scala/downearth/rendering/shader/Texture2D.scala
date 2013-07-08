package downearth.rendering.shader

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL14._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL20._

import java.nio.ByteBuffer
import org.lwjgl.opengl.GL30

/**
 * User: arne
 * Date: 29.04.13
 * Time: 21:55
 */

abstract class Texture{
  var id = glGenTextures()

  val width:Int
  val height:Int

  def target:Int

  def bind() {
    assert( id != 0 )
    glBindTexture(target, id)
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

class TextureCube(val width:Int,
              positiveX:ByteBuffer,
              negativeX:ByteBuffer,
              positiveY:ByteBuffer,
              negativeY:ByteBuffer,
              positiveZ:ByteBuffer,
              negativeZ:ByteBuffer) extends Texture {

  val height = width

  def target = GL_TEXTURE_CUBE_MAP

  glBindTexture(target, id)
  glTexParameteri(target, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  glTexParameteri(target, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  glTexParameteri(target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
  glTexParameteri(target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
  glTexParameteri(target, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE)

  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, 0, GL_RGBA8, width, width, 0, GL_BGRA, GL_UNSIGNED_BYTE, positiveX)
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, 0, GL_RGBA8, width, width, 0, GL_BGRA, GL_UNSIGNED_BYTE, negativeX)
  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, 0, GL_RGBA8, width, width, 0, GL_BGRA, GL_UNSIGNED_BYTE, positiveY)
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, 0, GL_RGBA8, width, width, 0, GL_BGRA, GL_UNSIGNED_BYTE, negativeY)
  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, 0, GL_RGBA8, width, width, 0, GL_BGRA, GL_UNSIGNED_BYTE, positiveZ)
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, 0, GL_RGBA8, width, width, 0, GL_BGRA, GL_UNSIGNED_BYTE, negativeZ)

  GL30.glGenerateMipmap(target)

  glBindTexture(target, 0)
}

class Texture2D(val width:Int, val height:Int, pixels:ByteBuffer) extends Texture {

  def target = GL_TEXTURE_2D

  glBindTexture(target, id)
  glTexParameteri(target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
  glTexParameteri(target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
  glTexParameteri(target, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
  glTexParameteri(target, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
  glTexParameteri(target, GL_GENERATE_MIPMAP, GL_TRUE)
  glTexImage2D(target, 0, GL_RGBA8, width, height, 0, GL_BGRA, GL_UNSIGNED_BYTE, pixels)
  GL30.glGenerateMipmap(target)

  glBindTexture(target, 0)


}
