package downearth.rendering.shader


import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL20._
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.Util

import simplex3d.math.double._
import simplex3d.data.DataView
import simplex3d.math.integration.RFloat

import downearth.rendering.Texture
import downearth.util._



/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:43
 */
abstract class Uniform[T](map:Map[String,Any]) extends AddString {
  val program:Program   = map("program").asInstanceOf[Program]
  val binding:Binding   = map("binding").asInstanceOf[Binding]
  val location:Int      = map("location").asInstanceOf[Int]
  val name:CharSequence = map("name").asInstanceOf[String]
  val glType:Int        = map("type").asInstanceOf[Int]
  val size:Int          = map("size").asInstanceOf[Int]

//  protected var _value:T = null
//  def value_=(v:T) {
//    _value = v
//
//  }

  def :=(v:T)

  /// stores the saved data
  def writeData()

  override def addString(sb:StringBuilder) =
    sb append s"layout(location = $location) uniform ${Program.shaderTypeString(glType)} $name ${if(size > 1) (s"[$size]") else ""}"

}

class Vec4Uniform(map:Map[String,Any]) extends Uniform[ReadVec4](map) {
  private[this] val data = Vec4(0)

  def :=(v:ReadVec4) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def writeData() {
    // require( glGetInteger(GL_CURRENT_PROGRAM) == program.id )
    // require( location == glGetUniformLocation(program.id, name), s"location should be $location, but it is ${glGetUniformLocation(program.id, name)}")
    glUniform4f(location, data.x.toFloat, data.y.toFloat, data.z.toFloat, data.w.toFloat)
  }

}

//class Vec4UniformInstanced(map:Map[String,Any]) extends Uniform(map) {
//  var data: () => DataView[Vec4,RFloat] = null
//
//  def writeData() {
//    val d = data()
//
//    glEnableVertexAttribArray(location)
//    glVertexAttribPointer(location, 4, GL_FLOAT, false, sizeOf[Vec4], sizeOf[Float] * 4)
//    glVertexAttribDivisor(location, 1)
//  }
//
//}

class Vec3Uniform(map:Map[String,Any]) extends Uniform[ReadVec3](map) {

  private[this] val data = Vec3(0)

  def :=(v:ReadVec3) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def writeData() {
    glUniform3f(location, data.x.toFloat, data.y.toFloat, data.z.toFloat)
  }
}

class Vec2Uniform(map:Map[String,Any]) extends Uniform[ReadVec2](map) {
  private[this] val data = Vec2(0)

  def :=(v:ReadVec2) {
    data := v
    binding.changedUniforms.enqueue(this)
  }

  def writeData() {
    glUniform2f(location, data.x.toFloat, data.y.toFloat)
  }
}

class FloatUniform(map:Map[String,Any]) extends Uniform[Float](map) {
  private[this] var data:Float = 0

  def :=(v:Float) {
    data = v
    binding.changedUniforms.enqueue(this)
  }

  def writeData() {
    glUniform1f(location, data)
  }
}

class TextureUniform(val position:Int, map:Map[String,Any]) extends Uniform[Texture](map) {
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

class Mat4Uniform(map:Map[String,Any]) extends Uniform[ReadMat4](map) {
  val buffer = BufferUtils.createFloatBuffer(16)

  def :=(m:ReadMat4) {
    var i = 0
    while( i < 16 ) {
      val x = i >> 2
      val y = i & 3
      buffer.put( m(c=y,r=x).toFloat )
      i += 1
    }
    buffer.flip()
    binding.changedUniforms.enqueue(this)
  }

  def writeData() {
    glUniformMatrix4(location, true, buffer)
  }
}
