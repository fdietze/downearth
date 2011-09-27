package openworld

import simplex3d.math.float._
import simplex3d.math.float.functions._

import simplex3d.data._
import simplex3d.data.float._

import org.lwjgl.opengl.GL11._

import Config._
import Util._

abstract class Camera{
	def renderScene
}

// Eine Kamera, die frei im Raum bewegt werden kann, und selbst dafür sorgt, dass alles was sie sieht gerendert wird
class Camera3D(var position:Vec3,var directionQuat:Quat4) extends Camera {
	def this (positionVec:Vec3,directionVec:Vec3) = this(positionVec,quaternion(lookAt(-directionVec,worldUpVector)))
	
	def camera = this
	
	def direction = directionQuat.rotateVector(-worldUpVector)
	
	// rotiert die Kamera, damit der worldUpVector auch für die Kamera oben ist
	def lerpUp(factor:Float){
		val up = inverse(directionQuat) rotateVector worldUpVector
		val α = atan(up.y, up.x) - Pi/2
		
		directionQuat *= Quat4 rotateZ(α*pow(factor,1.5f))
	}
	
	val frustum = {
		val v = screenWidth.toFloat / screenHeight.toFloat	
		
		val n = 0.05f //near
		val f = 1000   //far
		val r = v*n   //right
		val t = n     //top
		//l = -r; b = -t
		
		Mat4(n/r,0,0,0, 0,n/t,0,0, 0,0,(f+n)/(n-f),-1, 0,0,2*f*n/(n-f),0)
	}
	
	// setzt die Projektionsmatrix
	def applyfrustum{
		glMatrixMode(GL_PROJECTION)
		glLoadMatrix( frustum )
		glMatrixMode(GL_MODELVIEW)
		glDisable(GL_BLEND)
	}
	
	// setzt die Rotation und Position der Kamera
	def apply = {
		val data = DataBuffer[Mat4,RFloat](1)
		glLoadMatrix( Mat4(inverse(Mat3x4 rotate(directionQuat) translate(position))) )
	}
	
	def lighting{
		//Add ambient light
		glLightModel(GL_LIGHT_MODEL_AMBIENT, Seq(0.2f, 0.2f, 0.2f, 1.0f))
		//Add positioned light
		glLight(GL_LIGHT0, GL_POSITION, Vec4(position, 1f))
		//Add directed light
		//glLight(GL_LIGHT1, GL_DIFFUSE, Seq(0.5f, 0.2f, 0.2f, 1.0f));
		//glLight(GL_LIGHT1, GL_POSITION, Seq(-1.0f, 0.5f, 0.5f, 0.0f));
	}
	
	def renderScene {
		applyfrustum
		
		if(Config.skybox){
			glLoadMatrix( Mat4(inverse(Mat3x4 rotate(directionQuat))) )
			glDisable( GL_DEPTH_TEST )
			glDisable( GL_LIGHTING )
			Skybox.render
		}
		
		apply
		
		lighting
		
		glEnable(GL_DEPTH_TEST)
		glEnable(GL_LIGHTING)
		
		Main.activateShader{
			World.draw
		}
		
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
	
	def applyortho{
		glDisable(GL_DEPTH_TEST)
		glDisable(GL_LIGHTING)
		glMatrixMode(GL_PROJECTION)
		glLoadIdentity
		glOrtho(0,screenWidth,screenHeight,0,-100,100)
		glMatrixMode(GL_MODELVIEW)
		glLoadIdentity
	}
	
	def renderScene {

		applyortho
		Main.showfps
		Draw.drawTexts
		glPushMatrix
		glTranslatef(screenWidth/2,screenHeight/2,0)
		Draw.crossHair
		glPopMatrix
		
		val α = math.atan2( Player.direction.y, Player.direction.x ).toFloat.toDegrees - 90
		glTranslatef(screenWidth - 64, 64, 0)
		glScalef(32,32,32)
		glRotatef(30,1,0,0)
		glRotatef(α,0,1,0)
		glRotatef(90,1,0,0)
		glTranslatef(-0.5f,-0.5f,-0.5f)
		Draw.renderHexaeder( DefaultHexaeder.current )
		
	}
}
