package downearth.rendering

/**
 * User: arne
 * Date: 24.05.13
 * Time: 20:32
 */

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL14._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL21._

import scala.reflect.runtime.universe._

import java.io.{InputStreamReader, InputStream}
import scala.reflect.ClassTag

class ShaderCompileError(message:String) extends Exception(message)
class ShaderLinkError(message:String) extends Exception(message)

class Buffer(val target:Int, val id:Int = glGenBuffers()) {
  def bind() {
    glBindBuffer(target, id)
  }

  def delete() {
    glDeleteBuffers(id)
  }
}

abstract class Shader {
  var id = 0

  def create() {
    id = glCreateShader(id)
  }

  def delete() {
    glDeleteShader(id)
    id = 0
  }

  def shaderType:Int

  def compile() {
    assert(id != 0)
    glCompileShader(id)
    checkCompileStatus()
  }

  def source = ???

  def source_=(src: CharSequence) {
    glShaderSource(id, src)
  }

  def checkCompileStatus() {
    val status = glGetShaderi(id, GL_COMPILE_STATUS)
    if (status == GL_FALSE)
    {
      val infoLogLength = glGetShaderi(id, GL_INFO_LOG_LENGTH)
      val strInfoLog = glGetShaderInfoLog(id, infoLogLength)

      val strShaderType = shaderType match {
        case GL_VERTEX_SHADER   => "vertex"
        //      case GL_GEOMETRY_SHADER => "geometry"
        case GL_FRAGMENT_SHADER => "fragment"
      }

      throw new ShaderCompileError(s"$strShaderType shader:\n$strInfoLog\n")
    }
  }

  override def finalize() {
    if(id != 0)
      delete()
  }
}

class VertexShader extends Shader {
  def shaderType = GL_VERTEX_SHADER
}

class FragmentShader extends Shader {
  def shaderType = GL_FRAGMENT_SHADER
}

class Program {
  var id = 0

  def create() {
    id = glCreateProgram
  }

  def attach(shader:Shader) {
    glAttachShader(id, shader.id)
  }

  def link() {
    glLinkProgram(id)
    checkLinkStatus()
  }

  def use() {
    glUseProgram(id)
  }

  def delete() {
    glDeleteProgram(id)
    id = 0
  }

  def detach(shader:Shader) {
    glDetachShader(id, shader.id)
  }

  def checkLinkStatus() {
    val status = glGetProgrami(id, GL_LINK_STATUS)
    if (status == GL_FALSE)
    {
      val infoLogLength = glGetProgrami(id, GL_INFO_LOG_LENGTH)
      val strInfoLog = glGetProgramInfoLog(id, infoLogLength)
      throw new ShaderLinkError(s"Linker failure: $strInfoLog\n")
    }
  }

  override def finalize() {
    if( id != 0 ) {
      delete()
    }
  }
}

object Shader {
  def apply[T <: Shader](strShaderFile:InputStream )(implicit t:ClassTag[T]):T = {
    val reader = new InputStreamReader( strShaderFile )
    val buffsize = 100000
    val data = new Array[Char](buffsize)
    val result = reader.read(data)

    assert(result < buffsize, "buffersize too small for shader")

    val shader = t.runtimeClass.getConstructor().newInstance().asInstanceOf[T]

    shader.create()
    shader.source = data
    shader.compile()
    shader
  }
}

object Program {
  def apply(vertexShaders: VertexShader*)(fragmentShaders: FragmentShader*) = {
    val program = new Program()

    val shaderList = vertexShaders ++ fragmentShaders

    for(shader <- shaderList)
      program attach shader

    program.link()

    for(shader <- shaderList)
      program detach shader

    program
  }
}

