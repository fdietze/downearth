package openworld

import simplex3d.math.float._
import org.lwjgl.opengl.GL11._

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import simplex3d.data._
import simplex3d.data.float._

object Skybox{
	val vertices = Array( 
		Vec3(1,1,1), Vec3(-1,1,1), Vec3(-1,1,-1), Vec3(1,1,-1),
		Vec3(-1,1,1), Vec3(-1,-1,1), Vec3(-1,-1,-1), Vec3(-1,1,-1),
		Vec3(-1,-1,1), Vec3(1,-1,1), Vec3(1,-1,-1), Vec3(-1,-1,-1),
		Vec3(1,-1,1), Vec3(1,1,1), Vec3(1,1,-1), Vec3(1,-1,-1),
		//top
		Vec3(1,-1,1), Vec3(-1,-1,1), Vec3(-1,1,1), Vec3(1,1,1),
		//bottom
		Vec3(1,1,-1), Vec3(-1,1,-1), Vec3(-1,-1,-1), Vec3(1,-1,-1)
	)
	
	val texcoords = Array(
		Vec2(1.00f,0.0f),Vec2(0.75f,0.00f),Vec2(0.75f,0.5f),Vec2(1.00f,0.5f),
		Vec2(0.75f,0.0f),Vec2(0.50f,0.00f),Vec2(0.50f,0.5f),Vec2(0.75f,0.5f),
		Vec2(0.50f,0.0f),Vec2(0.25f,0.00f),Vec2(0.25f,0.5f),Vec2(0.50f,0.5f),
		Vec2(0.25f,0.0f),Vec2(0.00f,0.00f),Vec2(0.00f,0.5f),Vec2(0.25f,0.5f),
		
		Vec2(0.00f,1.0f),Vec2(0.25f,1.0f),Vec2(0.25f,0.5f),Vec2(0.00f,0.5f),
		Vec2(0.25f,1.0f),Vec2(0.50f,1.0f),Vec2(0.50f,0.5f),Vec2(0.25f,0.5f)
	)
	
	def skybox = Mat4(inverse(Mat3x4 rotate(Player.camera.directionQuat)))

	val m_skyboxBuffer = DataBuffer[Mat4,RFloat](1)
	def skyboxBuffer = {
		m_skyboxBuffer(0) = skybox
		m_skyboxBuffer.buffer
	}
	
	def render{
		if(Config.skybox) {
			glMatrixMode(GL_MODELVIEW)
			glLoadMatrix( skyboxBuffer )

			glDisable( GL_DEPTH_TEST )
			glDisable( GL_LIGHTING )
			glEnable( GL_TEXTURE_2D )

			TextureManager.skybox.bind
			//TextureManager.box.bind
			glBegin( GL_QUADS )
			for( (Vec3(x,y,z), Vec2(u,v)) <- vertices zip texcoords ){
				glTexCoord2f(u,v)
				glVertex3f(x,y,z)
			}
			glEnd
		}
	}
}

