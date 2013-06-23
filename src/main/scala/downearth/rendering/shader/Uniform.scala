package downearth.rendering.shader


import org.lwjgl.opengl._
import GL11._
import GL13._
import GL20._
import ARBInstancedArrays._
import org.lwjgl.BufferUtils

import simplex3d.math.floatx._

import downearth.rendering.{TextureCube, Texture2D}
import downearth.util._

class UniformConfig(
  val program:Program,
  val binding:Binding,
  val location:Int,
  val name:CharSequence,
  val glType:Int,
  val size:Int
)

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:43
 */
abstract class Uniform[T](config:UniformConfig) extends AddString {
  val program:Program   = config.program
  val binding:Binding   = config.binding
  val location:Int      = config.location
  val name:CharSequence = config.name
  val glType:Int        = config.glType
  val size:Int          = config.size

  def :=(v:T)

  def get:T

  /// stores the saved data
  def writeData()

  override def addString(sb:StringBuilder) =
    sb append s"layout(location = $location) uniform ${Program.shaderTypeString(glType)} $name ${if(size > 1) (s"[$size]") else ""}"

}

class UniformFake[T](program:Program, binding:Binding, _name:String) extends Uniform[T](
  new UniformConfig(program, binding, 0, _name, 0, 1)) {

  println(this)

  def := (v:T) { }
  def get = null.asInstanceOf[T]

  def writeData() {
    assert(false, "should not call this method")
  }

  override def addString( sb:StringBuilder) =
    sb append "(broken) uniform " append name
}

class UniformVec4f(config:UniformConfig) extends Uniform[ReadVec4f](config) {
  private[this] val data = Vec4f(0)

  def :=(v:ReadVec4f) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = sharedFloatBuffer(4)
    glGetUniform(program.id, location, data)
    ConstVec4f(data get 0, data get 1, data get 2, data get 3)
  }

  def writeData() {
    glUniform4f(location, data.x.toFloat, data.y.toFloat, data.z.toFloat, data.w.toFloat)
  }
}

class UniformVec3f(config:UniformConfig) extends Uniform[ReadVec3f](config) {

  private[this] val data = Vec3f(0)

  def :=(v:ReadVec3f) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = sharedFloatBuffer(3)
    glGetUniform(program.id, location, data)
    ConstVec3f(data get 0, data get 1, data get 2)
  }

  def writeData() {
    glUniform3f(location, data.x, data.y, data.z)
  }
}

class UniformVec2f(config:UniformConfig) extends Uniform[ReadVec2f](config) {
  private[this] val data = Vec2f(0)

  def :=(v:ReadVec2f) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = sharedFloatBuffer(2)
    glGetUniform(program.id, location, data)
    ConstVec2f(data get 0, data get 1)
  }

  def writeData() {
    glUniform2f(location, data.x, data.y)
  }
}

class UniformFloat(config:UniformConfig) extends Uniform[Float](config) {
  private[this] var data:Float = 0

  def :=(v:Float) {
    data = v
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    val data = sharedFloatBuffer(1)
    glGetUniform(program.id, location, data)
    data get 0
  }

  def writeData() {
    glUniform1f(location, data)
  }
}

class UniformSampler2D(val position:Int, config:UniformConfig) extends Uniform[Texture2D](config) {
  private[this] var texture: Texture2D = null

  def :=(v:Texture2D) {
    texture = v
    binding.changedUniforms.enqueue(this)
  }

  def get = ???

  def writeData() {
    glUniform1i(location, position)
    glActiveTexture(GL_TEXTURE0 + position)
    texture.bind()
  }
}

class UniformSamplerCube(val position:Int, config:UniformConfig) extends Uniform[TextureCube](config) {
  private[this] var texture: TextureCube = null

  def :=(v:TextureCube) {
    texture = v
    binding.changedUniforms.enqueue(this)
  }

  def get = ???

  def writeData() {
    glUniform1i(location, position)
    glActiveTexture(GL_TEXTURE0 + position)
    texture.bind()
  }
}

class UniformMat4f(config:UniformConfig) extends Uniform[ReadMat4f](config) {
  val buffer = BufferUtils.createFloatBuffer(16)

  def :=(m:ReadMat4f) {
    var i = 0
    while( i < 16 ) {
      val x = i >> 2
      val y = i & 3
      buffer.put( m(c=y,r=x) )
      i += 1
    }
    buffer.flip()
    binding.changedUniforms.enqueue(this)
  }

  def get = {
    // untested
    val data = sharedFloatBuffer(16)
    glGetUniform(program.id, location, data)
    ConstMat4f(data get  0, data get  1, data get  2, data get   3,
               data get  4, data get  5, data get  6, data get   7,
               data get  8, data get  9, data get 10, data get  11,
               data get 12, data get 13, data get 14, data get  15)
  }

  def writeData() {
    glUniformMatrix4(location, true, buffer)
  }
}
