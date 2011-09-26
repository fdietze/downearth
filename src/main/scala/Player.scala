package xöpäx

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

object DefaultHexaeder{
	val full = FullHexaeder
	val half = new PartialHexaeder(Z = 0x44440000){ override def toString = "half" }
	val quarter = new PartialHexaeder(Z = 0x44440000, Y = 0x88448844){ override def toString = "quarter" }
	val eighth = new PartialHexaeder(0x40404040,0x44004400,0x44440000){ override def toString = "eighth" }
	val rampA = new PartialHexaeder(Z = 0x00440000){ override def toString = "rampA" }
	val rampB = new PartialHexaeder(Z = 0x44880000){ override def toString = "rampB" }
	val rampC = new PartialHexaeder(Z = 0x00880000){ override def toString = "rampC" }
	
	val all = Seq(full,half,quarter,eighth,rampA,rampB,rampC)
	private var m_id = 0
	def id_=( new_id:Int ){ m_id = clamp(new_id,0,all.size-1) }
	def id = m_id
	
	def current = all(id)
	def rotate(num:Int){
		id = (id+num+all.size) % all.size
	}
}

object Controller{

	def build {
		val mousedest = World.raytracer(Player.position, Player.direction, true, 100)
		mousedest match {
			case Some(pos) => 
				World(pos) = DefaultHexaeder.current
			case _ =>
		}
	}
	
	def remove {
		val mousedest = World.raytracer(Player.position, Player.direction, false, 100)
		mousedest match {
			case Some(pos) =>
				World(pos) = EmptyHexaeder
			case _ =>
		}
	}

	def move(dir:Vec3) {
		Player move dir
	}
	
	def rotate(rot:Vec3) {
		Player rotate rot
	}
	
	def jump {
		Player.jump
	}
}

