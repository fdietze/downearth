package openworld

import simplex3d.math._
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
	
	def frustum = {
		val v = screenWidth.toFloat / screenHeight.toFloat	
		
		val n = 0.05f //near
		val f = 1000   //far
		val r = v*n   //right
		val t = n     //top
		//l = -r; b = -t
		
		Mat4(n/r,0,0,0, 0,n/t,0,0, 0,0,(f+n)/(n-f),-1, 0,0,2*f*n/(n-f),0)
	}
	def frustumBuffer = {
		val buffer = DataBuffer[Mat4,RFloat](1)
		buffer(0) = frustum
		buffer.buffer
	}
	
	val m_modelviewBuffer = DataBuffer[Mat4,RFloat](1)
	def modelview = Mat4(inverse(Mat3x4 rotate(directionQuat) translate(position)))
	def modelviewBuffer = {
		m_modelviewBuffer(0) = modelview
		m_modelviewBuffer.buffer
	}
	
	val m_skyboxBuffer = DataBuffer[Mat4,RFloat](1)
	def skyboxBuffer = {
		m_skyboxBuffer(0) = skybox
		m_skyboxBuffer.buffer
	}
	def skybox = Mat4(inverse(Mat3x4 rotate(directionQuat)))
	
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
		glViewport(0,0,screenWidth,screenHeight)
		glMatrixMode(GL_PROJECTION)
		glLoadMatrix( frustumBuffer )
		glMatrixMode(GL_MODELVIEW)
		glDisable(GL_BLEND)
		
		if(Config.skybox) {
			glLoadMatrix( skyboxBuffer )
			glDisable( GL_DEPTH_TEST )
			glDisable( GL_LIGHTING )
			Skybox.render
		}
		
		glLoadMatrix( modelviewBuffer )
		
		lighting
		
		glEnable(GL_DEPTH_TEST)
		glEnable(GL_LIGHTING)
		
		val frustumtest:FrustumTest =
		if( Config.frustumCulling )
			new FrustumTestImpl(frustum,modelview)
		else {
			new FrustumTest {
				def testNode( info:NodeInfo ) = true 
				val falsecount = 0
			}
		}
		
		Main.activateShader{
			World.draw(frustumtest)
		}
		
		Draw.addText("frustum culled nodes: "+frustumtest.falsecount)
		
		BuildInterface.highlightHexaeder(position,direction)
		
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

// die GUI wird sebst als Kamera implementiert weil sie ihre eigene 2D Szene hat
object GUI extends Camera{
	
	def applyortho{
		glDisable(GL_DEPTH_TEST)
		glDisable(GL_LIGHTING)
		glMatrixMode(GL_PROJECTION)
		glLoadIdentity
		glOrtho(0,screenWidth,screenHeight,0,-100,100)
		glMatrixMode(GL_MODELVIEW)
		glLoadIdentity
		glViewport(0,0,screenWidth,screenHeight)
	}
	
	def renderScene {
		applyortho
		Main.showfps
		Draw.drawTexts
		glPushMatrix
		glTranslatef(screenWidth/2,screenHeight/2,0)
		glDisable(GL_LIGHTING)
		glDisable(GL_TEXTURE_2D)
		glColor3f(1,1,1)
		Draw.crossHair
		glPopMatrix
		
		/*
		// rendert den aktuell ausgewählten Hexaeder zum Bauen
		val α = math.atan2( Player.direction.y, Player.direction.x ).toFloat.toDegrees - 90
		glTranslatef(screenWidth - 64, 64, 0)
		glScalef(32,32,32)
		glRotatef(30,1,0,0)
		glRotatef(α,0,1,0)
		glRotatef(90,1,0,0)
		glTranslatef(-0.5f,-0.5f,-0.5f)
		Draw.renderHexaeder( BuildInterface.current )
		*/
		
	}
}

trait FrustumTest {
	def testNode( info:NodeInfo ):Boolean
	def falsecount:Int
}

// Frustum Culling
// Idea by Mark Morley, http://web.archive.org/web/20030601123911/http://www.markmorley.com/opengl/frustumculling.html
class FrustumTestImpl(projection:Mat4, modelview:Mat4) extends FrustumTest {

	val planes = Array(Vec4(0),Vec4(0),Vec4(0),Vec4(0),Vec4(0),Vec4(0))
	val rows = transpose(projection * modelview)
	planes(0) = normalize(rows(3) - rows(0)) //right plane
	planes(1) = normalize(rows(3) + rows(0)) //left plane
	planes(2) = normalize(rows(3) + rows(1)) //bottom plane
	planes(3) = normalize(rows(3) - rows(1)) //top plane
	planes(4) = normalize(rows(3) - rows(2)) //far plane
	planes(5) = normalize(rows(3) + rows(2)) //near plane

	var falsecount = 0

	def testNode( info:NodeInfo ):Boolean = {
		val inside = testCube(info.pos + info.size / 2, info.size / 2)
		if( !inside )
			falsecount += 1
		return inside
	}
	
	private def testCube( pos:Vec3, radius:Float ):Boolean = {
		// TODO: Give the information, if a cube is completely in the frustum
		var p = 0
		import pos.{x,y,z}
		while( p < 6 )
		{
			if( planes(p).x * (x - radius) + planes(p).y * (y - radius) + planes(p).z * (z - radius) + planes(p).w <= 0 )
			if( planes(p).x * (x + radius) + planes(p).y * (y - radius) + planes(p).z * (z - radius) + planes(p).w <= 0 )
			if( planes(p).x * (x - radius) + planes(p).y * (y + radius) + planes(p).z * (z - radius) + planes(p).w <= 0 )
			if( planes(p).x * (x + radius) + planes(p).y * (y + radius) + planes(p).z * (z - radius) + planes(p).w <= 0 )
			if( planes(p).x * (x - radius) + planes(p).y * (y - radius) + planes(p).z * (z + radius) + planes(p).w <= 0 )
			if( planes(p).x * (x + radius) + planes(p).y * (y - radius) + planes(p).z * (z + radius) + planes(p).w <= 0 )
			if( planes(p).x * (x - radius) + planes(p).y * (y + radius) + planes(p).z * (z + radius) + planes(p).w <= 0 )
			if( planes(p).x * (x + radius) + planes(p).y * (y + radius) + planes(p).z * (z + radius) + planes(p).w <= 0 )
				return false
			p += 1
		}
		return true
	}
}
