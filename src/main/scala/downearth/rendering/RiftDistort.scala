package downearth.rendering

import glwrapper._
import simplex3d.math.floatx.Vec4f
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL14._
import org.lwjgl.opengl.GL31._
import org.lwjgl.opengl.Display
import java.nio.ByteBuffer
import org.lwjgl.opengl.GL14._
import downearth.Config

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 7/11/13
 * Time: 10:20 PM
 * To change this template use File | Settings | File Templates.
 */


object RiftDistort {
  val program = Program.auto("RiftDistortSideBySide")

  val binding = program.getBinding

  println(binding)

  val uSamplerColor = binding.uniformSampler2D("uSamplerColor")

  val vao = VertexArrayObject.create

  val position = binding.attributeVec4f("position")
  position := Seq( Vec4f(-1,-1,0,1), Vec4f(3,-1,0,1), Vec4f(-1,3,0,1) )

  vao.bind {
    binding.enableAttributes()
    binding.setAttributePointers()
  }

  val colorbuffer = (new Texture2D).create()
  val renderbuffer = (new Texture2D).create()

  uSamplerColor := colorbuffer

  val framebuffer = {
    setFrameBufferSize()
    val fb = (new FrameBuffer).create()
    fb.bind{
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, colorbuffer.target, colorbuffer.id, 0)
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, renderbuffer.target, renderbuffer.id, 0)

      assert( fb.checkStatus == "complete" )
    }

    fb
  }

  def setFrameBufferSize() {
    colorbuffer.bind {
      glTexImage2D(colorbuffer.target, 0, GL_RGBA8, Display.getWidth, Display.getHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, null.asInstanceOf[ByteBuffer])
      colorbuffer.parameter( GL_TEXTURE_BASE_LEVEL, 0)
    }

    renderbuffer.bind {
      glTexImage2D(renderbuffer.target, 0, GL_DEPTH_COMPONENT24, Display.getWidth, Display.getHeight, 0, GL_DEPTH_COMPONENT, GL_FLOAT, null.asInstanceOf[ByteBuffer])
      glPixelStorei(GL_UNPACK_ALIGNMENT, 4)
      renderbuffer.parameter( GL_TEXTURE_BASE_LEVEL, 0)
    }
  }

  val startTime = System.currentTimeMillis()
  def timeFloat = (System.currentTimeMillis()-startTime) / 1000f

  def draw() {
    program.use {
      vao.bind {
        binding.writeChangedUniforms()
        glDrawArrays(GL_TRIANGLES,0,3)
      }
    }
  }
}
