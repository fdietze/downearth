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
  val shader = {
    val vertShader = Shader[VertexShader]( getClass.getResourceAsStream("skybox.vsh") )
    val fragShader = Shader[FragmentShader]( getClass.getResourceAsStream("skybox.fsh") )
    Program("skybox")(vertShader)(fragShader)
  }

  val skyboxBinding = shader.getBinding
  println(skyboxBinding)
  val a_position = skyboxBinding.attributeVec3f("a_position")

  val u_mvp = skyboxBinding.uniformMat4f("u_mvp")
  val cubemap = skyboxBinding.uniformSamplerCube("cubemap")
  cubemap := TextureManager.skybox2

  val vertices = Seq(
    Vec3f(1,1,1), Vec3f(-1,1,1), Vec3f(-1,1,-1), Vec3f(1,1,-1),
    Vec3f(-1,1,1), Vec3f(-1,-1,1), Vec3f(-1,-1,-1), Vec3f(-1,1,-1),
    Vec3f(-1,-1,1), Vec3f(1,-1,1), Vec3f(1,-1,-1), Vec3f(-1,-1,-1),
    Vec3f(1,-1,1), Vec3f(1,1,1), Vec3f(1,1,-1), Vec3f(1,-1,-1),
    //top
    Vec3f(1,-1,1), Vec3f(-1,-1,1), Vec3f(-1,1,1), Vec3f(1,1,1),
    //bottom
    Vec3f(1,1,-1), Vec3f(-1,1,-1), Vec3f(-1,-1,-1), Vec3f(1,-1,-1)
  )

  a_position := vertices

  val buffer = {
    val dataBuffer = BufferUtils.createByteBuffer(6*4*sizeOf[Vec4f]*2)

    val texcoords = Seq(
      Vec2f(1.00f,0.0f),Vec2f(0.75f,0.0f),Vec2f(0.75f,0.5f),Vec2f(1.00f,0.5f),
      Vec2f(0.75f,0.0f),Vec2f(0.50f,0.0f),Vec2f(0.50f,0.5f),Vec2f(0.75f,0.5f),
      Vec2f(0.50f,0.0f),Vec2f(0.25f,0.0f),Vec2f(0.25f,0.5f),Vec2f(0.50f,0.5f),
      Vec2f(0.25f,0.0f),Vec2f(0.00f,0.0f),Vec2f(0.00f,0.5f),Vec2f(0.25f,0.5f),
      Vec2f(0.00f,1.0f),Vec2f(0.25f,1.0f),Vec2f(0.25f,0.5f),Vec2f(0.00f,0.5f),
      Vec2f(0.25f,1.0f),Vec2f(0.50f,1.0f),Vec2f(0.50f,0.5f),Vec2f(0.25f,0.5f)
    )

    for( (Vec3f(x,y,z), Vec2f(u,v)) <- vertices zip texcoords ) {
      import dataBuffer.{ putFloat => put }
      put(x);put(y);put(z);put(1);put(u);put(v);put(0);put(0)
    }

    dataBuffer.flip()

    val buffer = new ArrayBuffer
    buffer.create()
    buffer.bind{
      buffer.putData(dataBuffer)
    }

    buffer
  }
	
	def skybox = Mat4f(inverse(Mat4x3f rotate(Quat4f(Player.camera.directionQuat))))

	val m_skyboxBuffer = DataBuffer[Mat4,RFloat](1)
	def skyboxBuffer = {
		m_skyboxBuffer(0) = Mat4(skybox)
		m_skyboxBuffer.buffer
	}
	
	def render {
    if( Config.skybox ) {
      if(false) {
        glDisable( GL_DEPTH_TEST )
        shader.use {
          u_mvp := skybox
          skyboxBinding.bindChanges()
          glDrawArrays(GL_QUADS, 0, 4*6)
        }
      }
      else {
        glMatrixMode( GL_MODELVIEW )
        glLoadMatrix( skyboxBuffer )

        glDisable( GL_DEPTH_TEST )
        glDisable( GL_LIGHTING )
        glEnable( GL_TEXTURE_2D )

        TextureManager.skybox.bind

        glColor4f(1,1,1,1)

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

