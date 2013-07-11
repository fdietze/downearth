package downearth.rendering

import glwrapper._
import simplex3d.math.floatx.Vec4f

import org.lwjgl.opengl.{Display, GL11}
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL31._
import java.nio.ByteBuffer
import org.lwjgl.opengl.GL14._
import downearth.Config

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 7/11/13
 * Time: 3:15 PM
 * To change this template use File | Settings | File Templates.
 */


object Postprocess {
  val program = Program.auto("postprocess")

  val binding = program.getBinding

  val image = binding.uniformSampler2DRect("image")
  val time  = binding.uniformFloat("time")
  val a = binding.uniformFloat("a")
  val b = binding.uniformFloat("b")
  val c = binding.uniformFloat("c")
  val position = binding.attributeVec4f("position")

  position := Seq( Vec4f(-1,-1,0,1), Vec4f(3,-1,0,1), Vec4f(-1,3,0,1) )

  val vao = VertexArrayObject.create

  vao.bind {
    binding.enableAttributes()
    binding.setAttributePointers()
  }

  val colorbuffer = (new TextureRectangle).create()
  val renderbuffer = (new TextureRectangle).create()

  image := colorbuffer

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
      glTexImage2D(GL_TEXTURE_RECTANGLE, 0, GL_RGBA8, Display.getWidth, Display.getHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, null.asInstanceOf[ByteBuffer])
    }

    renderbuffer.bind {
      glTexImage2D(GL_TEXTURE_RECTANGLE, 0, GL_DEPTH_COMPONENT24, Display.getWidth, Display.getHeight, 0, GL_DEPTH_COMPONENT, GL_FLOAT, null.asInstanceOf[ByteBuffer])
      glPixelStorei(GL_UNPACK_ALIGNMENT, 4)
    }
  }

  val startTime = System.currentTimeMillis()
  def timeFloat = (System.currentTimeMillis()-startTime) / 1000f

  def draw() {
    time := timeFloat

    a := Config.a.toFloat
    b := Config.b.toFloat
    c := Config.c.toFloat

    program.use {
      vao.bind {
        binding.writeChangedUniforms()

        glDrawArrays(GL_TRIANGLES,0,3)
      }
    }
  }
}
