object Texture {

  def create2D(width:Int, height:Int, pixels:ByteBuffer) = {

    val texture = (new Texture2D(width,height)).create()

    texture.bind {
      texture.parameter(GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
      texture.parameter(GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
      texture.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
      texture.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)

      glTexImage2D(texture.target, 0, GL_RGBA8, width, height, 0, GL_BGRA, GL_UNSIGNED_BYTE, pixels)

      texture.generateMipmap()
    }

    texture
  }

  def createCube(width:Int,
                positiveX:ByteBuffer,
                negativeX:ByteBuffer,
                positiveY:ByteBuffer,
                negativeY:ByteBuffer,
                positiveZ:ByteBuffer,
                negativeZ:ByteBuffer) = {
    val texture = (new TextureCube(width)).create()

    texture.bind {
      texture.parameter(GL_TEXTURE_MAG_FILTER, GL_LINEAR)
      texture.parameter(GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
      texture.parameter(GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
      texture.parameter(GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
      texture.parameter(GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE)

      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, 0, GL_RGBA8, width, width, 0, GL_BGRA, GL_UNSIGNED_BYTE, positiveX)
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, 0, GL_RGBA8, width, width, 0, GL_BGRA, GL_UNSIGNED_BYTE, negativeX)
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, 0, GL_RGBA8, width, width, 0, GL_BGRA, GL_UNSIGNED_BYTE, positiveY)
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, 0, GL_RGBA8, width, width, 0, GL_BGRA, GL_UNSIGNED_BYTE, negativeY)
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, 0, GL_RGBA8, width, width, 0, GL_BGRA, GL_UNSIGNED_BYTE, positiveZ)
      glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, 0, GL_RGBA8, width, width, 0, GL_BGRA, GL_UNSIGNED_BYTE, negativeZ)

      texture.generateMipmap()
    }
  }
}

abstract class Texture extends GlObject {
  var id = 0

  def target:Int
  def binding:Int

  def create() = { id = glGenTextures(); this }
  def delete() = { glDeleteTextures(id); id = 0 }

  def bind() {
    glBindTexture(target, id)
  }

  def bind[T](block: => T) = {
    require( id != 0 )
    val outer = GL11.glGetInteger(binding)
    glBindTexture(target, id)
    val v = block
    glBindTexture(target, outer)
    v
  }

  def generateMipmap() {
    GL30.glGenerateMipmap(target)
  }

  def parameter(name:Int,param:Int) {
    glTexParameteri(target, name, param)
  }
}

class Texture2D extends Texture {
	def target   = GL_TEXTURE_2D
  def binding  = GL_TEXTURE_BINDING_2D
}

class TextureCube extends Texture {
	def target   = GL_TEXTURE_CUBE_MAP
  def binding  = GL_TEXTURE_BINDING_CUBE_MAP
}

