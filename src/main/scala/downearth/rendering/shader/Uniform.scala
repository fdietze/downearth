package downearth.rendering.shader

import simplex3d.math.double._
import org.lwjgl.opengl.GL20._
import downearth.rendering.Texture
import org.lwjgl.opengl.GL13._
import org.lwjgl.BufferUtils
import downearth.util.AddString

/**
 * User: arne
 * Date: 02.06.13
 * Time: 20:43
 */
abstract class Uniform(map:Map[String,Any]) extends AddString {
  val program:Program   = map("program").asInstanceOf[Program]
  val location:Int      = map("location").asInstanceOf[Int]
  val name:CharSequence = map("name").asInstanceOf[String]
  val glType:Int        = map("type").asInstanceOf[Int]
  val size:Int          = map("size").asInstanceOf[Int]

  def writeData()

  override def addString(sb:StringBuilder) =
    sb append s"uniform ${Program.shaderTypeString(glType)} $name ${if(size > 1) (s"[$size]") else ""}"

}

class Vec4Uniform(map:Map[String,Any]) extends Uniform(map) {
  var vec: () => ReadVec4 = null
  def writeData() {
    val value = vec()
    glUniform4f(location, value.x.toFloat, value.y.toFloat, value.z.toFloat, value.w.toFloat)
  }
}

class Vec3Uniform(map:Map[String,Any]) extends Uniform(map) {
  var vec: () => ReadVec3 = null

  def writeData() {
    val value = vec()
    glUniform3f(location, value.x.toFloat, value.y.toFloat, value.z.toFloat)
  }
}

class Vec2Uniform(map:Map[String,Any]) extends Uniform(map) {
  var vec: () => ReadVec2 = null

  def writeData() {
    val value = vec()
    glUniform2f(location, value.x.toFloat, value.y.toFloat)
  }
}

class FloatUniform(map:Map[String,Any]) extends Uniform(map) {
  var f: () => Float = null

  def writeData() {
    val value = f()
    glUniform1f(location, value)
  }
}

class TextureUniform(val position:Int, map:Map[String,Any]) extends Uniform(map) {
  var texture: () => Texture = null

  def writeData() {
    glActiveTexture(position)
    texture().bind()
    glUniform1i(location,position)
  }
}

class Mat4Uniform(map:Map[String,Any]) extends Uniform(map) {
  var mat: () => ReadMat4 = null
  val buffer = BufferUtils.createFloatBuffer(16)

  def writeData() {
    val m = mat()
    var i = 0
    while( i < 16 ) {
      val x = i >> 2
      val y = i & 3
      buffer.put( m(c=y,r=x).toFloat )
      i += 1
    }
    buffer.flip()
    glUniformMatrix4(location, false, buffer)
  }
}
