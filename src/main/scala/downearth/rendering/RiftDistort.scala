package downearth.rendering

import glwrapper._
import simplex3d.math.floatx.Vec4f
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL14._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL31._
import org.lwjgl.opengl.Display
import java.nio.ByteBuffer
import org.lwjgl.opengl.GL14._
import downearth.Config
import downearth.gui.{Listener, WidgetResized, MainWidget}
import simplex3d.math.Vec2i
import org.lwjgl.opengl.Util

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 7/11/13
 * Time: 10:20 PM
 * To change this template use File | Settings | File Templates.
 */


object RiftDistort extends Listener {
  listenTo(MainWidget)
  addReaction {
    case WidgetResized(widget) => setFrameBufferSize(widget.size)
  }

  val program = Program("rift")("RiftDistort.vsh")("RiftDistortSideBySide.fsh")

  val binding = program.getBinding

  println(binding)

  val uSamplerColor = binding.uniformSampler2D("uSamplerColor")
  val uDistort      = binding.uniformBool("uDistort");
  val uCorrectChromaticAberation = binding.uniformBool("uCorrectChromaticAberation");

  val vao = VertexArrayObject.create

  val position = binding.attributeVec4f("position")
  position := Seq( Vec4f(-1,-1,0,1), Vec4f(3,-1,0,1), Vec4f(-1,3,0,1) )

  vao.bind {
    binding.enableAttributes()
    binding.setAttributePointers()
  }

  val colorbuffer = (new Texture2D).create()
  val renderbuffer = (new Texture2D).create()

  glActiveTexture( GL_TEXTURE17 )
  colorbuffer.bind()
  colorbuffer.parameter( GL_TEXTURE_BASE_LEVEL, 0 )
  colorbuffer.parameter( GL_TEXTURE_MAX_LEVEL, 0 )
  colorbuffer.parameter( GL_TEXTURE_MIN_FILTER, GL_LINEAR )
  colorbuffer.parameter( GL_TEXTURE_MAG_FILTER, GL_LINEAR )
  glActiveTexture( GL_TEXTURE0 )

  new RenderBuffer

  uSamplerColor := colorbuffer

  val framebuffer = {

    setFrameBufferSize( Vec2i(Display.getWidth, Display.getHeight) )

    val fb = (new FrameBuffer).create()
    fb.bind{
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, colorbuffer.target, colorbuffer.id, 0)
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, renderbuffer.target, renderbuffer.id, 0)

      assert( fb.checkStatus == "complete" )
    }

    fb
  }

  def setFrameBufferSize(size:Vec2i) {
    colorbuffer.bind {
      glTexImage2D(colorbuffer.target, 0, GL_RGBA8, size.x, size.y, 0, GL_RGBA, GL_UNSIGNED_BYTE, null.asInstanceOf[ByteBuffer])
      colorbuffer.parameter( GL_TEXTURE_BASE_LEVEL, 0)
    }

    renderbuffer.bind {
      glTexImage2D(renderbuffer.target, 0, GL_DEPTH_COMPONENT24, size.x, size.y, 0, GL_DEPTH_COMPONENT, GL_FLOAT, null.asInstanceOf[ByteBuffer])
      glPixelStorei(GL_UNPACK_ALIGNMENT, 4)
      renderbuffer.parameter( GL_TEXTURE_BASE_LEVEL, 0)
    }
  }

  def draw() {
    uDistort := Config.uDistort
    uCorrectChromaticAberation := Config.uCorrectChromaticAberation

    program.use {
      vao.bind {
        binding.writeChangedUniforms()
        Util.checkGLError()
        glUniform1i( uSamplerColor.location, 17 )
        Util.checkGLError()
        glDrawArrays(GL_TRIANGLES,0,3)
        Util.checkGLError()
      }
    }
  }
}
