package downearth

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.data._
import simplex3d.data.double._

import com.bulletphysics.linearmath.Transform
import com.bulletphysics.collision.shapes._

import downearth.util._
import downearth.Config._

import gui.MainWidget
import javax.vecmath.{Vector3f, Quat4f}
import downearth.rendering.Draw
import downearth.worldoctree._
import scala.Some
import downearth.world.World
import org.lwjgl.input.Mouse

object Player {
	/////////////////////////////////
	// Physics, rotation and position	
	/////////////////////////////////
	
	val camDistFromCenter = Vec3(0,0,0.8)
	
	private val m_camera = new Camera3D(startpos,Vec3(1,0,0))
	def camera = {
		m_camera.position = position
		m_camera
	}
	
	val (body, ghostObject) = BulletPhysics.addCharacter(startpos)//BulletPhysics.addShape(1,startpos.clone,new CapsuleShape(0.3f,1.2f) )	
	
	def position:Vec3 = {
		if( isGhost )
			m_camera.position
		else {
			ghostObject.getWorldTransform(new Transform).origin + camDistFromCenter
		} // body.getCenterOfMassTransform(new Transform).origin + camDistFromCenter
	}
	
	def position_= (newPos:Vec3) {

		/*val v = new Vector3f
		body.setLinearVelocity(v)
		body getCenterOfMassPosition v
		v.negate
		v += newpos
		body translate v*/

		/*val transform = new Transform
		transform.origin.set(newPos)
		ghostObject.setWorldTransform(transform)*/
		
		//body.reset
		body.warp(newPos)
		//println(ghostObject.getWorldTransform(new Transform).origin)
	}

	def direction:Vec3 = {
    if( Mouse.isGrabbed ) {
      camera.direction
    }
    else {
      val rx = (Mouse.getX * 2.0 - Main.width   ) / Main.height
      val ry = (Mouse.getY * 2.0 - Main.height  ) / Main.height
      camera.directionQuat.rotateVector( normalize(Vec3(rx,ry,-1)) )
    }
  }
	
/*	def velocity:Vec3 = {
		val v = new Vector3f
		body getLinearVelocity v
		v
	}*/

	def resetPos {
		DisplayEventManager.showEventText("reset")
		position = startpos
	}

	//body setAngularFactor 0
	
	def move(dir:Vec3) {
		if( isGhost )
			m_camera move dir*4
		else {
			val flatdir = m_camera rotateVector dir
			flatdir *= 2
			flatdir.z = 0
			//body.applyCentralImpulse( flatdir )*/
			body.setWalkDirection(flatdir)
			//body.playerStep(BulletPhysics.dynamicsWorld, 1/30f)
		}
	}
	
	var dir = Vec2(Pi/2)
	
	def rotate(rot:Vec3) {
		// TODO hier entstehen noch starke rotationsartefakte
		m_camera.rotate(rot)
		m_camera.lerpUp(1 - direction.z.abs )
	}
	
	def jump{
		if( !isGhost ) {
			//body.applyCentralImpulse(new Vector3f(0,0,5))
			DisplayEventManager.showEventText("jump")
			body.jump
		}
	}
	
	var isGhost = Config.startAsGhost
	
	def toggleGhost {
		/*
		if( isGhost ) {
			position = camera.position - camDistFromCenter
			BulletPhysics.addBody(body)
			isGhost = false
		}
		else {
			BulletPhysics.removeBody(body)
			isGhost = true
		}*/
	}
	//////////////////////////////////
	// Tools, Inventory, Menu Controls
	//////////////////////////////////
	val inventory = new Inventory
	var activeTool:PlayerTool = inventory.tools(0)
	def selectTool(tool:Int) = activeTool = inventory.tools(tool)
	def selectTool(tool:PlayerTool) = {
		if( inventory.tools contains tool )
			activeTool = tool
	}
	def selectNextTool {
		selectTool(
			(inventory.tools.indexOf(activeTool) + 1) % inventory.tools.size
		)
	}
	
	def primaryAction   = activeTool.primary
	def secondaryAction = selectNextTool
	
}

class Inventory {
	val materials = new collection.mutable.HashMap[Int,Double] {
		override def default(key:Int) = 0.0
	}
	val tools = IndexedSeq(Shovel, ConstructionTool)
}


trait PlayerTool {
	def primary
	def top:Boolean
	def range = 100
	def selectPolyeder(pos:Vec3i):Polyeder
	
	def draw {
		selectPos match {
			case Some(pos) =>
				Draw.highlight( pos, selectPolyeder(pos) )
			case _ =>
		}
	}
	
	def selectPos = {
		if( MainWidget.mouseOver )
			World.octree.raytracer(Player.position, Player.direction, top, range)
		else
			None
	}
}

object Shovel extends PlayerTool {
	// removes a block
	def top = false
	
	def selectPolyeder(pos:Vec3i) = World.octree(pos).h
	
	override def primary{
		selectPos match {
			case Some(pos) =>
				val block = World.octree(pos)
				//TODO: the evaluation of the materialfunction should be in the Leaf itself
				val material = if( block.material == -1 ) materialfunction(pos + 0.5).id else block.material
				Player.inventory.materials(material) += block.h.volume
				World(pos) = Leaf(EmptyHexaeder)
			case _ =>
		}
	}
	
	override def toString = getClass.getName.split('.').last
}

object ConstructionTool extends PlayerTool {
	// adds a block
	def selectPolyeder(pos:Vec3i) = current
	
	val full = FullHexaeder
	val half = new Hexaeder(Z = 0x44440000)
	val quarter = new Hexaeder(Z = 0x44440000, Y = 0x44004400)
	val eighth = new Hexaeder(0x40404040,0x44004400,0x44440000)
	val rampA = new Hexaeder(Z = 0x00440000)
	val rampB = new Hexaeder(Z = 0x44880000)
	val rampC = new Hexaeder(Z = 0x00880000)
	val test = new Polyeder10
	
	def makeRotations(h:Polyeder) = {
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

	
	override def top = true
	
	var selectedMaterial = 3

	override def primary = {
		selectPos match {
			case Some(pos) =>
				if( Player.inventory.materials(selectedMaterial) >= current.volume ) {
					val block = World.octree(pos)
					//TODO: the evaluation of the materialfunction should be in the Leaf itself
					val material = if( block.material == -1 ) materialfunction(pos + 0.5).id else block.material
					Player.inventory.materials(material) += block.h.volume
					Player.inventory.materials(selectedMaterial) -= current.volume
					World(pos) = Leaf(current, selectedMaterial)
				}
				else {
					DisplayEventManager.showEventText("Not enough Material " + selectedMaterial + ".")
				}
			case _ =>
		}
	}
	
	override def draw {
		selectPos match {
			case Some(pos) =>
				Draw.highlight( pos, selectPolyeder(pos), false )
			case _ =>
		}
	}
}

