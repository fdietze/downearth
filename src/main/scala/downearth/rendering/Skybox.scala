package downearth.rendering

import simplex3d.math.double._
import org.lwjgl.opengl.GL11._

import simplex3d.math._
import integration.RFloat
import simplex3d.math.double._
import simplex3d.math.double.functions._

import simplex3d.data._
import simplex3d.data.double._
import java.nio.FloatBuffer
import downearth.{Config, Player}

object Skybox {
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
		Vec2(1.00,0.0),Vec2(0.75,0.0),Vec2(0.75,0.5),Vec2(1.00,0.5),
		Vec2(0.75,0.0),Vec2(0.50,0.0),Vec2(0.50,0.5),Vec2(0.75,0.5),
		Vec2(0.50,0.0),Vec2(0.25,0.0),Vec2(0.25,0.5),Vec2(0.50,0.5),
		Vec2(0.25,0.0),Vec2(0.00,0.0),Vec2(0.00,0.5),Vec2(0.25,0.5),
		
		Vec2(0.00,1.0),Vec2(0.25,1.0),Vec2(0.25,0.5),Vec2(0.00,0.5),
		Vec2(0.25,1.0),Vec2(0.50,1.0),Vec2(0.50,0.5),Vec2(0.25,0.5)
	)
	
	def skybox = Mat4(inverse(Mat4x3 rotate(Player.camera.directionQuat)))

	val m_skyboxBuffer = DataBuffer[Mat4,RFloat](1)
	def skyboxBuffer = {
		m_skyboxBuffer(0) = skybox
		m_skyboxBuffer.buffer
	}
	
	def render {
		if(Config.skybox) {
			glMatrixMode(GL_MODELVIEW)
			glLoadMatrix( skyboxBuffer )

			glDisable( GL_DEPTH_TEST )
			glDisable( GL_LIGHTING )
			glEnable( GL_TEXTURE_2D )

			TextureManager.skybox.bind
			
			glColor4f(1,1,1,1)
			glBegin( GL_QUADS )
			for( (Vec3(x,y,z), Vec2(u,v)) <- vertices zip texcoords ){
				glTexCoord2d(u,v)
				glVertex3d(x,y,z)
			}
			glEnd()
		}
	}
}

