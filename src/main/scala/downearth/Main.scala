package downearth

import org.lwjgl.opengl._
import org.lwjgl.input._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import downearth.rendering._
import downearth.generation.WorldNodeGenerator
import downearth.Config._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.ARBDebugOutput._
import org.lwjgl.opengl.AMDDebugOutput._
import downearth.world.World
import downearth.util._
import downearth.gui._
//import downearth.server.LocalServer
import downearth.worldoctree.InnerNodeUnderMesh


object Main extends Logger {

  def main(args: Array[String]) {

    log.println( "Assertions " + (if( assertionsActivated ) "active" else "inactive" ))
    log.println( "started" )

    val ca = new ContextAttribs(3,1).withDebug(true)
    val alpha = 8
    val depth = 16
    val stencil = 0
    val pf = new PixelFormat(alpha, depth, stencil)

    Display.setDisplayMode( new DisplayMode(Config.windowResolutionWidth,Config.windowResolutionHeight) )
    Display.setResizable(true)
    Display.create(pf, ca)

    log.println( "display created" )

    val wed = new WidgetEventDispatcher(MainWidget)

    val gameLoop = new GameLoop
    wed.listenTo(gameLoop)

    gameLoop.run()

    Config.loader.save()
  }
}

class GameLoop extends Publisher with Logger { gameLoop =>

  var snapshotRequest = 0L
  var snapshotCurrent = -1L

  var finished = false

  def time = System.currentTimeMillis
  val starttime = time
  def uptime = time - starttime

  var lastFrame = uptime
  var currentFps = 0
  var timestamp = starttime
  var frameCounter = 0


  def run() {
    init()

    while(!finished) {

      BulletPhysics.update()
      // input()

      handleInput()

      World.octree.makeUpdates()

      if( Config.streamWorld ) {
        World.octree stream Player.pos
      }

      Renderer.draw()
      frame()

      Display.update()
      Display.sync(fpsLimit)
    }

    destroy()
  }

  def init() {
    val caps = GLContext.getCapabilities

    // TODO: Remove all ARB calls from code
    // grep ARB src/main/scala/downearth -r

    if ( caps.GL_ARB_debug_output ) {
      println("GL_ARB_debug_output")
      glDebugMessageCallbackARB(new ARBDebugOutputCallback())
    }
    else if ( caps.GL_AMD_debug_output ) {
      println("GL_AMD_debug_output")
      glDebugMessageCallbackAMD(new AMDDebugOutputCallback())
    }
    else {
      println("no debuge output")
    }

    if ( caps.GL_ARB_draw_instanced ) {
      println("can do instancing")
    }
    else {
      println("can't do instancing")
    }

    if( caps.GL_ARB_uniform_buffer_object ) {
      println("uniform buffer object supported")
    }
    else {
      println("uniform buffer object NOT supported")
    }

    glClearColor(0.4f, 0.6f, 0.9f, 1f)

    World

    //Mouse setGrabbed true
	}

  def destroy() {

    WorldNodeGenerator.actorSystem.shutdown()
    //LocalServer.actorSystem.shutdown()

    TextureManager.delete()
    ObjManager.delete()

    println("all destroyed")
	}
	
	// Berechnet die Aktuelle Framerate
	def frame() {
		if(time - timestamp > 1000){
			currentFps = frameCounter
			timestamp = time
			frameCounter = 0
      Display.setTitle("%d fps" format currentFps)
		}
		else
			frameCounter += 1
	
		lastFrame = uptime
	}

  var windowMode:DisplayMode = null

  def handleInput() {
    import Mouse._
    import Keyboard._

    if( Display.isCloseRequested )
      finished = true

    val mouseDelta = Vec2i(getDX, getDY)
    // Move and rotate player
    val delta = Vec3(0)
    val delta_angle = Vec3(0)

    if( isKeyDown(keyForward) )
      delta.z -= 1
    if( isKeyDown(keyBackward) )
      delta.z += 1
    if( isKeyDown(keyLeft) )
      delta.x -= 1
    if( isKeyDown(keyRight) )
      delta.x += 1

    val mouseGrabIndependant: Int => Unit = {
      case `keyQuit` =>
        finished = true
      case `keyScreenshot` =>
        screenShot( "screenshot" )
      case `keyMouseGrab` =>
        Mouse setGrabbed !Mouse.isGrabbed
      case `keyPlayerReset` =>
        Player.resetPos
      case `keyStreaming` =>
        streamWorld = !streamWorld
      case `keyWireframe` =>
        wireframe = !wireframe
      case `keyFrustumCulling` =>
        frustumCulling = !frustumCulling
      case `keyTurbo` =>
        turbo = ! turbo
        log.println(s"Turbo is ${if(turbo) "on" else "off"}." )
      case `keyPausePhysics` =>
        BulletPhysics.pause = !BulletPhysics.pause
      case `keyDebugDraw` =>
        debugDraw += 1
      case `keyToggleGhostPlayer` =>
        Player.toggleGhost()
      case `keyToggleInventory` =>
        MainWidget.inventory.visible = !MainWidget.inventory.visible
      case `keyJump` =>
        Player.jump()
      case `keyIncOctreeDepth` =>
        World.octree.incDepth()
      case `keyGenerateNextUngenerated` =>
        val next = World.octree.getNextUngenerated
        println("next Ungenerated:" + next)
        for( area <- next )
          World.octree.generateArea(area)
        println("not finished:")
        World.octree.query(){
          case (area, n:InnerNodeUnderMesh) =>
            if( !n.finishedGeneration ) {
              println(n)
              true
            } else false
          case _ =>
            true
        }
      case `keyToggleFullScreen` =>
        if( Display.isFullscreen ) {
          Display.setDisplayModeAndFullscreen(windowMode)
          MainWidget.resize( Vec2i(windowMode.getWidth, windowMode.getHeight) )
        }
        else {
          windowMode = Display.getDisplayMode
          val mode = Display.getDesktopDisplayMode
          assert(mode.isFullscreenCapable)
          Display.setDisplayModeAndFullscreen(mode)
          MainWidget.resize( Vec2i(mode.getWidth, mode.getHeight) )
        }
      case _ =>
    }

    if( Mouse.isGrabbed ) {

      // rotate with mouse
      delta_angle.y = -mouseDelta.x/300.0
      delta_angle.x = mouseDelta.y/300.0

      // Turbo mode
      if( turbo && Mouse.isButtonDown(0) )
        Player.primaryAction()

      // Keyboard Events
      while ( Keyboard.next ) {
        if (getEventKeyState) {
          mouseGrabIndependant(getEventKey)
        }
        // implement some mouse grab dependant keys here
      }

      // Mouse events
      while( Mouse.next ) {
        ( getEventButton, getEventButtonState ) match {
          case (0 , true) => // left down
          case (0 , false) => // left up
            Player.primaryAction()
          case (1 , true) => // right down
          case (1 , false) => // right up
            //Player.secondarybutton
            Mouse setGrabbed false
            Mouse setCursorPosition( Display.getWidth / 2, Display.getHeight / 2)
          case (-1, false) => // wheel
          // Player.updownbutton( Mouse.getDWheel / 120 )
          case _ =>
        }
      }
    }
    else { // if Mouse is not grabbed

      // Keyboard Events
      while ( Keyboard.next ) {
        if( getEventKey != KEY_NONE ) {
          if( getEventKeyState )
            publish( KeyPress(getEventKey) )
          else
            publish( KeyRelease(getEventKey) )
        }

        if ( getEventKeyState ) {
          mouseGrabIndependant(getEventKey)
          // implement some mouse grab dependant keys here
        }

        val c = Keyboard.getEventCharacter
        if( c.intValue != 0 ) {
          if( c.isControl ) {
            c match {
              case '\t' =>
                print("<tab>")
              case '\b' =>
                print("<back>")
              case '\n' =>
                print("<enter>")
              case '\r' =>
                print("<return>")
              case _ =>
                print(s"<${c.intValue}>")
            }
          }
          else
            print( Keyboard.getEventCharacter )
        }
      }

      if( Display.wasResized ) {
        MainWidget.resize(Vec2i(Display.getWidth, Display.getHeight))
      }

      // Mouse events
      while( Mouse.next ) {
        ( getEventButton, getEventButtonState ) match {
          case (n , true) if n >= 0 =>
            val event =  MouseDown(Vec2i(getEventX,Display.getHeight - getEventY), n)
            publish( event )
          case (n , false) if n >= 0 =>
            val event =  MouseUp(Vec2i(getEventX,Display.getHeight - getEventY), n)
            publish( event )
            if(n == 1)
              Mouse setGrabbed true
          case (-1, _) =>
            val dx =  Mouse.getEventDX
            val dy = -Mouse.getEventDY
            val x  =  Mouse.getEventX
            val y  = Display.getHeight - Mouse.getEventY
            val dW = Mouse.getDWheel
            //TODO assert( (dx != 0 || dy != 0) ^ (dW != 0) )
            if( dx != 0 || dy != 0 ) {
              publish( MouseMove(Vec2i(x-dx,y-dy),Vec2i(x,y)) )
            }
          case _ =>
        }
      }
    }

    val factor = if(turbo) cameraTurboSpeed else cameraSpeed
    Player.move(factor*(delta/max(1,length(delta)))*timeStep)
    Player.rotate(2.0*delta_angle)
  }
}
