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


object Camera{
	
	val WIDTH = 1024
	val HEIGHT = WIDTH*3/4
	
	val UP = Vec3.UnitZ
	
}

abstract class Camera{
	def renderScene
}

class Camera3D(var position:Vec3,var directionQuat:Quat4) extends Camera with ControlInterface{
	def this (positionVec:Vec3,directionVec:Vec3) = this(positionVec,quaternion(lookAt(-directionVec,Camera.UP)))
	
	import Camera._
	def camera = this
	
	def direction = directionQuat.rotateVector(-UP)
	
	// rotates the camera to have a correct upwards vector
	def lerpUp(factor:Float){
		val dest = quaternion(lookAt(-direction,Camera.UP))
		directionQuat = slerp(directionQuat,dest,factor)
	}
	
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
		// temporary storage for Matrices
		val data = DataBuffer[Mat4,RFloat](1)
		data(0) = frustum
		glMatrixMode(GL_PROJECTION)
		//glLoadIdentity
		//glFrustum(-r,r,-t,t,n,f)
		glLoadMatrix( data.buffer )
		glMatrixMode(GL_MODELVIEW)
		
		glDisable(GL_BLEND)
	}
	
	def apply = {
		val data = DataBuffer[Mat4,RFloat](1)
		data(0) = Mat4(inverse(Mat3x4 rotate(directionQuat) translate(position)))
		glLoadMatrix( data.buffer )
	}
	
	def renderScene {
		applyfrustum
		apply
		
		World.draw
		if(Config.debugDraw)
			BulletPhysics.debugDrawWorld
	}
	
	def makeRelative(v:Vec3) = directionQuat.rotateVector(v)
	
	def move(delta:Vec3){
		position += makeRelative(delta)
	}
	
	def rotate(rot:Vec3){
		directionQuat *= Quat4 rotateX(rot.x) rotateY(rot.y) rotateZ(rot.z)
	}
	
}

// 2D Kamera für die GUI
object GUI extends Camera{
	import Camera.{WIDTH,HEIGHT}
	
	def applyortho{
		glDisable(GL_DEPTH_TEST)
		glDisable(GL_LIGHTING)
		glMatrixMode(GL_PROJECTION)
		glLoadIdentity
		glOrtho(0,WIDTH,HEIGHT,0,-1,1)
		glMatrixMode(GL_MODELVIEW)
		glLoadIdentity
	}
	
	def renderScene {
		applyortho
		Main.showfps
		Draw.drawTexts
		glTranslatef(Camera.WIDTH/2,Camera.HEIGHT/2,0)
		Draw.crossHair
	}
}

object FreeCamera extends Camera3D(
		positionVec = Vec3(3,1,0),
		directionVec = Vec3(0.26f,-0.05f,0.14f)) {
	import Camera._
}
