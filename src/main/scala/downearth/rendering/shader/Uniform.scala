package downearth.rendering.shader


import org.lwjgl.opengl._
import GL11._
import GL13._
import GL20._
import ARBInstancedArrays._
import org.lwjgl.BufferUtils

import simplex3d.math.floatx._

import downearth.rendering.Texture
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

  /// stores the saved data
  def writeData()

  override def addString(sb:StringBuilder) =
    sb append s"layout(location = $location) uniform ${Program.shaderTypeString(glType)} $name ${if(size > 1) (s"[$size]") else ""}"

}

class Vec4Uniform(config:UniformConfig) extends Uniform[ReadVec4f](config) {
  private[this] val data = Vec4f(0)

  def :=(v:ReadVec4f) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def writeData() {
    glUniform4f(location, data.x.toFloat, data.y.toFloat, data.z.toFloat, data.w.toFloat)
  }


}



class UniformVec4fInstanced(config:UniformConfig) extends Uniform[Seq[ReadVec4f]](config) {

  val buffer = new ArrayGlBuffer

  def := (seq:Seq[ReadVec4f]) {
    buffer.bind {
      val data = BufferUtils.createFloatBuffer(4*seq.size)
      seq.foreach( v => data.put(v.x).put(v.y).put(v.z).put(v.w) )
      data.flip
      buffer.load(data)
    }
  }

  def writeData() {
    glEnableVertexAttribArray(location)
    glVertexAttribPointer(location, 4, GL_FLOAT, false, sizeOf[Vec4f], 0)
    glVertexAttribDivisorARB(location, 1)
  }
}

class UniformVec3f(config:UniformConfig) extends Uniform[ReadVec3f](config) {

  private[this] val data = Vec3f(0)

  def :=(v:ReadVec3f) {
    data := v
    binding.changedUniforms.enqueue(this)
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

  def writeData() {
    glUniform1f(location, data)
  }
}

class UniformSampler2D(val position:Int, config:UniformConfig) extends Uniform[Texture](config) {
  private[this] var texture: Texture = null

  def :=(v:Texture) {
    texture = v
    binding.changedUniforms.enqueue(this)
  }

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

  def writeData() {
    glUniformMatrix4(location, true, buffer)
  }
}
