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
		body.applyCentralImpulse((m_camera makeRelative dir)*5)
	}
	
	var dir = Vec2(Pi/2)
	
	def rotate(rot:Vec3){
		m_camera.rotate(rot)
		
		m_camera.lerpUp(1-m_camera.direction.z.abs)
		/*
		dir += rot.xy
		dir.x = clamp( dir.x, 0, Pi )
		dir.y = ( dir.y + 2*Pi ) % (2*Pi)
		m_camera.directionQuat = Quat4 rotateX dir.x rotateZ dir.y
		*/
	}
	
	def jump{
		body.applyCentralImpulse(new Vector3f(0,0,10))
	}
}

trait ControlInterface{
	def position:Vec3
	def direction:Vec3
	def camera:Camera3D
	def move(dir:Vec3)
	def rotate(rot:Vec3)
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
	
	def build{
		val mousedest = World.raytracer(current.position,current.direction,true,100)
		mousedest match {
			case Some(pos) => 
				World(pos) = FullHexaeder
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

