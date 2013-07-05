package downearth.rendering

import org.lwjgl.opengl.GL11._
import org.lwjgl.BufferUtils

import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.math.floatx._

import simplex3d.data._
import simplex3d.data.double._

import downearth.{Config, Player}
import downearth.rendering.shader._
import downearth.util.sizeOf

object Skybox {
  val shader = Program.auto("skybox")
  val vao = VertexArrayObject.create
  val skyboxBinding = shader.getBinding

  println(skyboxBinding)

  val shader_position = skyboxBinding.attributeVec4f("position")
  val shader_matrix = skyboxBinding.uniformMat4f("matrix")
  val shader_cubemap = skyboxBinding.uniformSamplerCube("cubemap")
  shader_cubemap := TextureManager.skybox2

  val vertices = Seq(
    Vec4f(1,1,1,1), Vec4f(-1,1,1,1), Vec4f(-1,1,-1,1), Vec4f(1,1,-1,1),
    Vec4f(-1,1,1,1), Vec4f(-1,-1,1,1), Vec4f(-1,-1,-1,1), Vec4f(-1,1,-1,1),
    Vec4f(-1,-1,1,1), Vec4f(1,-1,1,1), Vec4f(1,-1,-1,1), Vec4f(-1,-1,-1,1),
    Vec4f(1,-1,1,1), Vec4f(1,1,1,1), Vec4f(1,1,-1,1), Vec4f(1,-1,-1,1),
    //top
    Vec4f(1,-1,1,1), Vec4f(-1,-1,1,1), Vec4f(-1,1,1,1), Vec4f(1,1,1,1),
    //bottom
    Vec4f(1,1,-1,1), Vec4f(-1,1,-1,1), Vec4f(-1,-1,-1,1), Vec4f(1,-1,-1,1)
  )

  shader_position := vertices

  val texcoords = Seq(
    Vec2f(1.00f,0.0f),Vec2f(0.75f,0.0f),Vec2f(0.75f,0.5f),Vec2f(1.00f,0.5f),
    Vec2f(0.75f,0.0f),Vec2f(0.50f,0.0f),Vec2f(0.50f,0.5f),Vec2f(0.75f,0.5f),
    Vec2f(0.50f,0.0f),Vec2f(0.25f,0.0f),Vec2f(0.25f,0.5f),Vec2f(0.50f,0.5f),
    Vec2f(0.25f,0.0f),Vec2f(0.00f,0.0f),Vec2f(0.00f,0.5f),Vec2f(0.25f,0.5f),
    Vec2f(0.00f,1.0f),Vec2f(0.25f,1.0f),Vec2f(0.25f,0.5f),Vec2f(0.00f,0.5f),
    Vec2f(0.25f,1.0f),Vec2f(0.50f,1.0f),Vec2f(0.50f,0.5f),Vec2f(0.25f,0.5f)
  )

  val buffer = {
    val dataBuffer = BufferUtils.createByteBuffer(6*4*sizeOf[Vec4f]*2)

    for( (Vec4f(x,y,z,w), Vec2f(u,v)) <- vertices zip texcoords ) {
      import dataBuffer.{ putFloat => put }
      put(x);put(y);put(z);put(w);put(u);put(v);put(0);put(0)
    }

    dataBuffer.flip()

    val buffer = (new ArrayBuffer).create
    buffer.bind {
      buffer.putData(dataBuffer)
    }

    buffer
  }
	
	def skybox = Mat4f(inverse(Mat4x3f rotate(Quat4f(Player.camera.directionQuat))))

	val m_skyboxBuffer = BufferUtils.createFloatBuffer(16)

	def skyboxBuffer = {
    m_skyboxBuffer.clear()
    downearth.util.putMat4f(m_skyboxBuffer, Mat4f(skybox))
    m_skyboxBuffer.flip()
    m_skyboxBuffer
	}
	
	def render {
    if( Config.skybox ) {
      if(Config.test) {

        shader_matrix := Mat4f(Player.camera.projection)*skybox

        glDisable( GL_DEPTH_TEST )

        shader.use {
          vao.bind {
            skyboxBinding.bindChanges()
            glDrawArrays(GL_QUADS, 0, 4*6)
          }
        }
      }
      else {
        glMatrixMode( GL_MODELVIEW )
        glLoadMatrix( skyboxBuffer )

        glDisable( GL_DEPTH_TEST )
        glDisable( GL_LIGHTING )
        glEnable( GL_TEXTURE_2D )

        TextureManager.skybox.bind

        buffer.bind {
          glEnableClientState(GL_VERTEX_ARRAY)
          glEnableClientState(GL_TEXTURE_COORD_ARRAY)

          glVertexPointer(4, GL_FLOAT, sizeOf[Vec4f]*2, 0)
          glTexCoordPointer(2, GL_FLOAT, sizeOf[Vec4f]*2, sizeOf[Vec4f])

          glDrawArrays(GL_QUADS, 0, 4*6)

          glDisableClientState(GL_TEXTURE_COORD_ARRAY)
          glDisableClientState(GL_VERTEX_ARRAY)
        }
      }
    }
	}
}

