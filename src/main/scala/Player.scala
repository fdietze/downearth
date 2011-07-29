package xöpäx

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import simplex3d.data._
import simplex3d.data.float._

import org.lwjgl.opengl.GL11._

import xöpäx.Util.multMatrixOfBody
import javax.vecmath.Vector3f
import com.bulletphysics.linearmath.Transform
import com.bulletphysics.collision.shapes.SphereShape

object ExampleBall {
	import org.lwjgl.util.glu._
	import GLU._

	val radius = 0.25f
	val startpos = Vec3(6,-4,3)
	val body = BulletPhysics.addShape(1,startpos,new SphereShape(radius))
	val glMatrix = DataBuffer[Mat4,RFloat](1)
	
	val startTransform = new Transform
	startTransform.setIdentity
	
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
	
	val sphere:Sphere = new Sphere
	sphere.setNormals(GLU_SMOOTH)
	sphere.setDrawStyle(GLU_FILL)
	
	def draw{
		glColor4f(1,1,1,1)
		
	
		glPushMatrix
		//TODO load matrix and multMatrix
		val motionState = body.getMotionState
		val transform = new Transform
		motionState.getWorldTransform(transform)
		val matrixAsArray = new Array[Float](16)
		transform.getOpenGLMatrix(matrixAsArray)
		val matrixAsBuffer = org.lwjgl.BufferUtils.createFloatBuffer(16)
		matrixAsBuffer put matrixAsArray
		matrixAsBuffer.flip
		
		glMultMatrix(matrixAsBuffer)
		
		sphere.draw(radius, 18, 9)
		glPopMatrix
	}
}


object Player{
	def position = FreeCamera.position
	def direction = FreeCamera.directionVec
	
	def foo{
		val mousedest = World.raytracer(position,direction,false,100)
		mousedest match {
			case Some(pos) =>
				println(World.octree getPolygons pos)
			case _ =>
		}
	}
	
	def build{
		val mousedest = World.raytracer(position,direction,true,100)
		mousedest match {
			case Some(pos) => 
				println("build at "+pos)
				World(pos) = FullHexaeder
			case _ =>
		}
	}
	
	def remove{
		val mousedest = World.raytracer(position,direction,false,100)
		mousedest match {
			case Some(pos) =>
				World(pos) = EmptyHexaeder
			case _ =>
		}
	}
}

/*
object Camera{

}

class Camera{

}
*/

object FreeCamera{
	val UP = Vec3.UnitZ
	
	val position = Vec3(3,1,0) //Vec3(22,-17,90)
	val direction = quaternion(lookAt(Vec3(-0.26f,0.05f,-0.14f),UP))
	def directionVec = direction.rotateVector(-UP)
	
	val WIDTH = 1024
	val HEIGHT = WIDTH*3/4
	
	val data = DataBuffer[Mat4,RFloat](1)
	
	def move(delta:Vec3){
		position += makeRelative(delta)
		
		//Worldgen.addText(Vec2i(25,50),Collision.sphereworldintersect(position,0.1f).toString)
	}
	
	def turn(delta:Vec3) {
		direction *= Quat4 rotateX(delta.x) rotateY(delta.y) rotateZ(delta.z)
	}
	
	def makeRelative(v:Vec3) = direction.rotateVector(v)
	
	val frustum = {
		val v = WIDTH.toFloat / HEIGHT.toFloat	
		
		val n = 0.05f //near
		val f = 1000   //far
		val r = v*n   //right
		val t = n     //top
		//l = -r; b = -t
		
		Mat4(n/r,0,0,0, 0,n/t,0,0, 0,0,(f+n)/(n-f),-1, 0,0,2*f*n/(n-f),0)
	}
	
	def applyfrustum{
		glEnable(GL_DEPTH_TEST)
		glEnable(GL_LIGHTING)
		
		// frustum matrix
		data(0) = frustum
		
		glMatrixMode(GL_PROJECTION)
		//glLoadIdentity
		//glFrustum(-r,r,-t,t,n,f)
		glLoadMatrix( data.buffer )
		glMatrixMode(GL_MODELVIEW)
		
		glDisable(GL_BLEND)
	}
	
	def applyortho{
		glDisable(GL_DEPTH_TEST)
		glDisable(GL_LIGHTING)
		glMatrixMode(GL_PROJECTION)
		glLoadIdentity
		glOrtho(0,WIDTH,HEIGHT,0,-1,1)
		glMatrixMode(GL_MODELVIEW)
	}
	
	def apply() = {
		data(0) = Mat4(inverse(Mat3x4 rotate(direction) translate(position)))
		glLoadMatrix( data.buffer )
	}
}
