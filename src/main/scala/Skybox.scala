package xöpäx

import simplex3d.math.float._
import org.lwjgl.opengl.GL11._

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
	
	def render{
		glEnable(GL_TEXTURE_2D)
		TextureManager.skybox.bind
		//TextureManager.box.bind
		glDisable(GL_LIGHTING)
		glBegin( GL_QUADS )
		for( (Vec3(x,y,z), Vec2(u,v)) <- vertices zip texcoords ){
			glTexCoord2f(u,v)
			glVertex3f(x,y,z)
		}
		glEnd
	}
}

