package downearth

import simplex3d.math.double._
import simplex3d.math.double.functions._

import com.bulletphysics.linearmath.Transform

import downearth.util._
import downearth.Config._
import downearth.tools._

import org.lwjgl.input.Mouse
import javax.vecmath.Vector3f


object Player extends Ray {
  /////////////////////////////////
  // Physics, rotation and position  
  /////////////////////////////////
  
  val camDistFromCenter = Vec3(0,0,0.8)
  
  private val m_camera = new Camera3D(startpos,Vec3(1,0,0))
  def camera = {
    if( !isGhost )
      m_camera.position := pos
    m_camera
  }
  
  val (body, ghostObject) = BulletPhysics.addCharacter(startpos)//BulletPhysics.addShape(1,startpos.clone,new CapsuleShape(0.3f,1.2f) )  
  
  def pos:ReadVec3 = {
    if( isGhost )
      m_camera.position
    else {
      ghostObject.getWorldTransform(new Transform).origin + camDistFromCenter
    }
  }

  def pos_= (newPos: ReadVec3) {
    if( isGhost )
      m_camera.position := newPos
    else {
      val tmp = new Vector3f(newPos.x.toFloat, newPos.y.toFloat, newPos.z.toFloat)
      body.warp(tmp)
    }
  }

  def dir:Vec3 = {
    if( Mouse.isGrabbed ) {
      camera.direction
    }
    else {
      val rx = (Mouse.getX * 2.0 - Main.width   ) / Main.height
      val ry = (Mouse.getY * 2.0 - Main.height  ) / Main.height
      dir(rx,ry)
    }
  }

  def dir(rx:Double,ry:Double):Vec3 = {
    camera.directionQuat.rotateVector( normalize(Vec3(rx,ry,-1)) )
  }

  def resetPos {
    DisplayEventManager.showEventText("reset")
    pos = startpos
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
  
  def rotate(rot:Vec3) {

    m_camera.rotate(rot)
    // TODO this method to make z an absolute needs still some improvements
    m_camera.lerpUp( 1 - pow( dir.z, 2 ) )
  }

  def rotate(rot:Quat4) {
    m_camera.directionQuat *= rot
  }
  
  def jump() {
    if( !isGhost ) {
      //body.applyCentralImpulse(new Vector3f(0,0,5))
      DisplayEventManager.showEventText("jump")
      body.jump
    }
  }
  
  var isGhost = Config.startAsGhost
  
  def toggleGhost() {
    if( isGhost ) {
      pos = camera.position - camDistFromCenter
      // BulletPhysics.addBody(body)
      isGhost = false
    }
    else {
      // BulletPhysics.removeBody(body)
      isGhost = true
    }
  }

  //////////////////////////////////
  // Tools, Inventory, Menu Controls
  //////////////////////////////////

  class Foo {
    val materials = new collection.mutable.HashMap[Int,Double] {
      override def default(key:Int) = 0.0
    }

    val tools:Seq[PlayerTool] = Seq(Shovel, ConstructionTool, TestBuildTool)
  }

  val inventory = new Foo

  var activeTool:PlayerTool = inventory.tools(0)
  def selectTool(tool:Int) = activeTool = inventory.tools(tool)
  def selectTool(tool:PlayerTool) = {
    if( inventory.tools contains tool )
      activeTool = tool
    else {
      throw new Exception("player tool not in tools list")
    }
  }
  def selectNextTool {
    selectTool(
      (inventory.tools.indexOf(activeTool) + 1) % inventory.tools.size
    )
  }
  
  def primaryAction()   = activeTool.action()
  def secondaryAction() = selectNextTool
}









