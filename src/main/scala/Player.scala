package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import simplex3d.data._
import simplex3d.data.float._

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._
import GLU._

import com.bulletphysics.linearmath.Transform
import com.bulletphysics.collision.shapes._

import Util._
import Config.startpos
import javax.vecmath.{Vector3f, Quat4f}

object Player {

	val camDistFromCenter = Vec3(0,0,0.8f)
	var isGhost = false

	//val camera = new Camera3D(startpos,Vec3(1,0,0))
	
	private val m_camera = new Camera3D(startpos,Vec3(1,0,0))
	def camera = {
		m_camera.position = position
		m_camera
	}

	def position:Vec3 = {
		if( isGhost )
			m_camera.position
		else
			body.getCenterOfMassTransform(new Transform).origin + camDistFromCenter
	}

	def position_= (newpos:Vec3) {
		m_camera.position := newpos
		m_camera.position += camDistFromCenter

		val v = new Vector3f
		body.setLinearVelocity(v)
		body getCenterOfMassPosition v
		v.negate
		v.x += newpos.x
		v.y += newpos.y
		v.z += newpos.z
		body translate v
	}

	def direction:Vec3 = camera.direction

	def resetPos {
		position = startpos
	}

	// CapsuleShape(0.3f,1.2f)
	val body = BulletPhysics.addShape(1,startpos.clone,new CapsuleShape(0.3f,1.2f) )
	body setAngularFactor 0
	
	def move(dir:Vec3){
		if( isGhost ){
			m_camera move dir*4
		}
		else {
			val flatdir = m_camera makeRelative dir
			flatdir *= 4
			flatdir.z = 0
			body.applyCentralImpulse( flatdir )
			val v = new Vector3f
			body getCenterOfMassPosition v
			Draw.addText("Player Position: " + round10(v)  )
			body getLinearVelocity v
			Draw.addText("Player Velocity: " + round10(v) )
		}
	}
	
	var dir = Vec2(Pi/2)
	
	def rotate(rot:Vec3){
		// TODO hier entstehen noch starke rotationsartefakte
		m_camera.rotate(rot)
		m_camera.lerpUp(1-m_camera.direction.z.abs)

	}
	
	def jump{
		if( !isGhost )
			body.applyCentralImpulse(new Vector3f(0,0,5))
	}

	def toggleGhost{
		if( isGhost ){
			position = camera.position - camDistFromCenter
			BulletPhysics.addBody(body)
			isGhost = false
		}
		else {
			BulletPhysics.removeBody(body)
			isGhost = true
		}
	}
}

object BuildInterface{
	val full = FullHexaeder
	val half = new PartialHexaeder(Z = 0x44440000)
	val quarter = new PartialHexaeder(Z = 0x44440000, Y = 0x44004400)
	val eighth = new PartialHexaeder(0x40404040,0x44004400,0x44440000)
	val rampA = new PartialHexaeder(Z = 0x00440000)
	val rampB = new PartialHexaeder(Z = 0x44880000)
	val rampC = new PartialHexaeder(Z = 0x00880000)
	
	// wird benötigt um den Korrekten Hexaeder hervorzuheben
	// true: Momentan am Bauen
	// false: Momentan am Buddeln
	var buildStatus = false

	def makeRotations(h:Hexaeder) = {
		val h1 = h.rotateZ
		val h2 = h1.rotateZ
		val h3 = h2.rotateZ
		Vector(h,h1,h2,h3)
	}

	val all = Vector(full,half,quarter,eighth,rampA,rampB,rampC) map makeRotations
	private var m_id = 0
	def id_= ( new_id:Int ) { m_id = clamp(new_id,0,all.size-1) }
	def id = m_id

	def rotation = {
		val dir = Player.direction
		(math.atan2(dir.y, dir.x) * 2 / math.Pi + 5.5).toInt % 4
	}
	def current = all(id)(rotation)
	def rotate(num:Int){
		if(num != 0)
			buildStatus = true
		id = (id+num+all.size) % all.size
	}
	
	def build(position:Vec3,direction:Vec3) {
		val mousedest = World.raytracer(position, direction, buildStatus, 100)
		mousedest match {
			case Some(pos) => 
				World(pos) = if(buildStatus) current else EmptyHexaeder
			case _ =>
		}
	}
	
	def highlightHexaeder(position:Vec3, direction:Vec3) {
		val selection = World.raytracer(position,direction,buildStatus,100)
		selection match {
		case Some(v) =>
			Draw.addText("Selected Voxel: " + Vec3i(v) )
			// malt die Markierung der angewählten Zelle
			glPushMatrix
			glTranslatef(v.x,v.y,v.z)
			val hexaeder = 
			if(buildStatus)
				current
			else
				World(v)
			
			glDisable(GL_LIGHTING)
			glDisable(GL_TEXTURE_2D)
			glDisable(GL_DEPTH_TEST)
			glEnable(GL_BLEND)
			glColor4f(1,1,1,0.25f)
			Draw.renderHexaeder(hexaeder)
			glDisable(GL_BLEND)
			glEnable(GL_DEPTH_TEST)
			Draw.renderHexaeder(hexaeder)
			glPopMatrix
			glEnable(GL_LIGHTING)
		case None =>
		}
	}
}

