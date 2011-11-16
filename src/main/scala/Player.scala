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
import Config._
import javax.vecmath.{Vector3f, Quat4f}

object Player {
	/////////////////////////////////
	// Physics, rotation and position	
	/////////////////////////////////
	
	val camDistFromCenter = Vec3(0,0,0.8f)
	
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
		v += newpos
		body translate v
	}

	def direction:Vec3 = camera.direction
	
	def velocity:Vec3 = {
		val v = new Vector3f
		body getLinearVelocity v
		v
	}

	def resetPos {
		position = startpos
	}

	val body = BulletPhysics.addShape(1,startpos.clone,new CapsuleShape(0.3f,1.2f) )
	body setAngularFactor 0
	
	def move(dir:Vec3) {
		if( isGhost )
			m_camera move dir*4
		else {
			val flatdir = m_camera rotateVector dir
			flatdir *= 4
			flatdir.z = 0
			body.applyCentralImpulse( flatdir )
		}
	}
	
	var dir = Vec2(Pi/2)
	
	def rotate(rot:Vec3) {
		// TODO hier entstehen noch starke rotationsartefakte
		m_camera.rotate(rot)
		m_camera.lerpUp(1-m_camera.direction.z.abs)

	}
	
	def jump{
		if( !isGhost )
			body.applyCentralImpulse(new Vector3f(0,0,5))
	}
	
	var isGhost = false
	
	if( Config.startAsGhost )
		toggleGhost
	
	def toggleGhost {
		if( isGhost ) {
			position = camera.position - camDistFromCenter
			BulletPhysics.addBody(body)
			isGhost = false
		}
		else {
			BulletPhysics.removeBody(body)
			isGhost = true
		}
	}
	//////////////////////////////////
	// Tools, Inventory, Menu Controls
	//////////////////////////////////
	val inventory = new Inventory
	var activetool:PlayerTool = inventory.tools(0)
	def selecttool(tool:Int) = activetool = inventory.tools(tool)
	def selectnexttool {
		selecttool(
			(inventory.tools.indexOf(activetool) + 1) % inventory.tools.size
		)
	}
	

	def updownbutton(direction:Int) = activetool.updownbutton(direction:Int)
	def primarybutton = activetool.primarybutton
	def secondarybutton = selectnexttool
	def draw {
		// draw other stuff
		activetool.draw
	}
}

class Inventory {
	val materials = new collection.mutable.HashMap[Int,Double] {
		override def default(key:Int) = 0.0
	}
	val tools = IndexedSeq(Scoop,ConstructionTool)
}


trait PlayerTool {
	def primarybutton {}
	def updownbutton(direction:Int) {}
	def draw {}
	
	def selectedpos(top:Boolean) = World.raytracer(Player.position, Player.direction, top, buildrange)
}

object Scoop extends PlayerTool {
	override def primarybutton = removeblock
	override def draw = highlightblock
	
	def removeblock {
		selectedpos(top=false) match {
			case Some(pos) =>
				val block = World(pos)
				//TODO: the evaluation of the materialfunction should be in the Leaf itself
				val material = if( block.material == -1 ) materialfunction(pos + 0.5f).id else block.material
				Player.inventory.materials(material) += block.h.volume
				World(pos) = Leaf(EmptyHexaeder)
			case _ =>
		}
	}
	
	def highlightblock {
		selectedpos(top=false) match {
			case Some(pos) =>
				Draw.highlight(pos, World(pos).h)
			case _ =>
		}
	}
}

object ConstructionTool extends PlayerTool {
	override def primarybutton = addblock
	override def draw = highlightblock
	
	var selectedMaterial = 3 //TODO: read from inventory
	var selectedBlock = FullHexaeder //TODO: read from cunstruction list
	
	def addblock {
		selectedpos(top=true) match {
			case Some(pos) =>
				if( Player.inventory.materials(selectedMaterial) >= selectedBlock.volume ) {
					val block = World(pos)
					//TODO: the evaluation of the materialfunction should be in the Leaf itself
					val material = if( block.material == -1 ) materialfunction(pos + 0.5f).id else block.material
					Player.inventory.materials(material) += block.h.volume
					Player.inventory.materials(selectedMaterial) -= selectedBlock.volume
					World(pos) = Leaf(selectedBlock, selectedMaterial)
				}
				else {
					DisplayEventManager.showEventText("Not enough Material " + selectedMaterial + ".")
				}
			case _ =>
		}
	}
	
	def highlightblock {
		selectedpos(top=true) match {
			case Some(pos) =>
				Draw.highlight(pos, selectedBlock)
			case _ =>
		}
	}
}



object BuildInterface {
	val full = FullHexaeder
	val half = new Hexaeder(Z = 0x44440000)
	val quarter = new Hexaeder(Z = 0x44440000, Y = 0x44004400)
	val eighth = new Hexaeder(0x40404040,0x44004400,0x44440000)
	val rampA = new Hexaeder(Z = 0x00440000)
	val rampB = new Hexaeder(Z = 0x44880000)
	val rampC = new Hexaeder(Z = 0x00880000)
	val test = new Polyeder10
	
	// wird benötigt um den Korrekten Hexaeder hervorzuheben
	// true: Momentan am Bauen
	// false: Momentan am Buddeln
	var inventoryMass = 0f
	var buildStatus = false

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
	def rotate(num:Int){
		if(num != 0)
			buildStatus = true
		id = (id+num+all.size) % all.size
	}
	
	def build(position:Vec3,direction:Vec3) {
		val mousedest = World.raytracer(position, direction, buildStatus, buildrange)
		mousedest match {
			case Some(pos) =>
				inventoryMass += World(pos).h.volume
				val replacement = if(buildStatus) current else EmptyHexaeder
				inventoryMass -= replacement.volume
				World(pos) = Leaf(replacement, 3)
			case _ =>
		}
	}
	
	def highlightHexaeder(position:Vec3, direction:Vec3) {
	}
}

