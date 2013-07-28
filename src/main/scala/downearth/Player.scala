package downearth

import simplex3d.math.double._
import simplex3d.math.double.functions._

import com.bulletphysics.linearmath.Transform

import downearth.util._
import downearth.Config._
import downearth.tools._

import org.lwjgl.input.Mouse
import javax.vecmath.Vector3f
import org.lwjgl.opengl.Display
import downearth.worldoctree.{CubeLike, Sphere, CuboidLike, Cube}
import simplex3d.math.Vec3i


class Player(gameState:GameState) extends Ray { //TODO: why extend and not contain ray?
  import gameState._
  /////////////////////////////////
  // Physics, rotation and position  
  /////////////////////////////////
  
  val camDistFromCenter = Vec3(0,0,0.8)
  
  private val m_camera = new Camera3D(startpos,Vec3(1,0,0))

  def camera = {
    m_camera.position := pos
    m_camera
  }
  
  val (body, ghostObject) = physics.addCharacter(startpos)
  val positionAsGhost = Vec3(startpos)
  
  def pos:ReadVec3 = {
    if( isGhost )
      positionAsGhost
    else {
      ghostObject.getWorldTransform(new Transform).origin + camDistFromCenter
    }
  }

  def window = Cube(Vec3i(pos) - playerRadius / 2, playerRadius)
  def sightWindow = Cube(Vec3i(pos) - playerSightRadius.toInt / 2, playerSightRadius.toInt)
  def sightSphere = Sphere(Vec3i(pos), playerSightRadius.toInt)

  def canSee(area:CubeLike):Boolean = area overlaps sightSphere


  def pos_= (newPos: ReadVec3) {
    if( isGhost )
      positionAsGhost := newPos
    else {
      val tmp = newPos - camDistFromCenter
      val param = new Vector3f(tmp.x.toFloat, tmp.y.toFloat, tmp.z.toFloat)
      body.warp(param)
    }
  }

  def dir:Vec3 = {
    if( Mouse.isGrabbed ) {
      camera.direction
    }
    else {
      val rx = (Mouse.getX * 2.0 - Display.getWidth   ) / Display.getHeight
      val ry = (Mouse.getY * 2.0 - Display.getHeight  ) / Display.getHeight
      dir(rx,ry)
    }
  }

  def dir(rx:Double,ry:Double):Vec3 = {
    camera.directionQuat.rotateVector( normalize(Vec3(rx,ry,-1)) )
  }

  def resetPos() {
    DisplayEventManager.showEventText("reset")
    pos = startpos
  }

  //body setAngularFactor 0
  
  def move(dir:Vec3) {
    if( isGhost ) {
      positionAsGhost += m_camera rotateVector dir*4
    }
    else {
      val flatdir = m_camera rotateVector dir
      flatdir *= 2
      flatdir.z = 0
      body.setWalkDirection(flatdir)
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
      body.jump()
    }
  }
  
  var isGhost = Config.startAsGhost
  
  def toggleGhost() {
    if( isGhost ) {
      // BulletPhysics.addBody(body)
      val p = pos
      isGhost = false
      pos = p
    }
    else {
      val p = pos
      isGhost = true
      pos = p
    }
  }

  //////////////////////////////////
  // Tools, Inventory, Menu Controls
  //////////////////////////////////

  val inventory = new {
    import gameState.tools._
    val materials = new collection.mutable.HashMap[Int,Double] {
      override def default(key:Int) = 0.0
    }

    val tools:IndexedSeq[PlayerTool] = Array(shovel, constructionTool, testBuildTool)
  }

  var activeTool:PlayerTool = inventory.tools(0)
  def selectTool(tool:Int) = activeTool = inventory.tools(tool)
  def selectTool(tool:PlayerTool) = {
    if( inventory.tools contains tool )
      activeTool = tool
    else {
      throw new Exception("player tool not in tools list")
    }
  }
  def selectNextTool() {
    selectTool(
      (inventory.tools.indexOf(activeTool) + 1) % inventory.tools.size
    )
  }
  
  def primaryAction()   = activeTool.action()
  def secondaryAction() = selectNextTool()
}









