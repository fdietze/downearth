package xöpäx

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import simplex3d.data._
import simplex3d.data.float._

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._
import GLU._

import javax.vecmath.Vector3f
import com.bulletphysics.collision.shapes._

class ExampleBall(startpos:Vec3, val radius:Float){
	val body = BulletPhysics.addShape(1,startpos,new SphereShape(radius))
	
	def resetBallPos {
		val v = new Vector3f
		body setLinearVelocity v
		body getCenterOfMassPosition v
		v.negate
		v.x += startpos.x
		v.y += startpos.y
		v.z += startpos.z
		body translate v
	}
	
	//glu phere
	val sphere:Sphere = new Sphere
	sphere.setNormals(GLU_SMOOTH)
	sphere.setDrawStyle(GLU_FILL)
	
	def draw{
		glColor4f(1,1,1,1)
		
		glPushMatrix
	
		Util.multMatrixOfBody(body)
		
		sphere.draw(radius, 18, 9)
		glPopMatrix
	}
}

object Balls{
	//val exampleballs = (-1 to 1) map (i => new ExampleBall( Vec3(6+i,-4+i,3) , 0.25f))
	
	def draw{
		//for(b <- exampleballs) b.draw
	}
	
	def resetBallPos{
		//for(b <- exampleballs) b.resetBallPos
	}
}

