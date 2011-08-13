package xöpäx

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import simplex3d.data._
import simplex3d.data.float._

import org.lwjgl.opengl.GL11._
import org.lwjgl.util.glu._
import GLU._

import javax.vecmath.{Quat4f,Vector3f}
import com.bulletphysics.linearmath.Transform
import com.bulletphysics.collision.shapes._

import Util._

object Player extends ControlInterface{
	import Config.startpos
	
	//val camera = new Camera3D(startpos,Vec3(1,0,0))
	
	private val m_camera = new Camera3D(startpos,Vec3(1,0,0))
	def camera = {
		m_camera.position = position
		m_camera
	}
	
	def position:Vec3 = body.getCenterOfMassTransform(new Transform).origin + Vec3(0,0,0.8f)
	def direction:Vec3 = camera.direction
	
	def resetPos {
		val v = new Vector3f
		body setLinearVelocity v
		body getCenterOfMassPosition v
		v.negate
		v.x += startpos.x
		v.y += startpos.y
		v.z += startpos.z
		body translate v
	}
	
	// CapsuleShape(0.3f,1.2f)
	val body = BulletPhysics.addShape(1,startpos.clone,new CapsuleShape(0.3f,1.2f) )
	
	
	body setAngularFactor 0
	
	def move(dir:Vec3){
		val flatdir = Vec3(((m_camera makeRelative dir)*4).xy,0)
		body.applyCentralImpulse( flatdir )
		
		val v = new Vector3f
		body getCenterOfMassPosition v
		Draw.addText("Player Position: " + round10(v)  )
		body getLinearVelocity v
		Draw.addText("Player Velocity: " + round10(v) )
	}
	
	var dir = Vec2(Pi/2)
	
	def rotate(rot:Vec3){
		m_camera.rotate(rot)
		m_camera.lerpUp(1-m_camera.direction.z.abs)
	}
	
	def jump{
		val v = new Vector3f
		body getLinearVelocity v
		if( abs(v.z) < 0.5 )
			body.applyCentralImpulse(new Vector3f(0,0,5))
		// if there is a contact point
		// jum in direction of Body normal
	}
}

trait ControlInterface{
	def position:Vec3
	def direction:Vec3
	def camera:Camera3D
	def move(dir:Vec3)
	def rotate(rot:Vec3)
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
	def rotate{
		id = (id+1) % all.size
	}
}

object Controller{
	val objects = List[ControlInterface](Player,FreeCamera)
	var id = 0
	var current = objects(id)
	
	def rotateObjects{
		id = (id+1)%objects.size
		current = objects(id)
		World.octree.jumpTo(current.position)
	}
	
	
	def build {
		val mousedest = World.raytracer(current.position,current.direction,true,100)
		mousedest match {
			case Some(pos) => 
				World(pos) = DefaultHexaeder.current
			case _ =>
		}
	}
	
	def remove{
		val mousedest = World.raytracer(current.position,current.direction,false,100)
		mousedest match {
			case Some(pos) =>
				World(pos) = EmptyHexaeder
			case _ =>
		}
	}
	
	def move(dir:Vec3){
		current move dir
	}
	
	def rotate(rot:Vec3){
		current rotate rot
	}
	
	def jump{
		if(current == Player){
			Player.jump
		}
	}
}

