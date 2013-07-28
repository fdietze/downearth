package downearth

import org.lwjgl.opengl._
import org.lwjgl.input._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
import akka.pattern.ask
import downearth.rendering._
import downearth.rendering.{GlDraw, ObjManager, TextureManager, Renderer}
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.ARBDebugOutput._
import org.lwjgl.opengl.AMDDebugOutput._
import downearth.util._
import downearth.gui._
import downearth.world.DynamicWorld
import downearth.tools.{TestBuildTool, Shovel, ConstructionTool}
import downearth.resources.MaterialManager
import akka.util.Timeout
import scala.concurrent.{ExecutionContext, Await}
import scala.concurrent.duration._
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import Config._

//import downearth.server.LocalServer
import downearth.worldoctree.{MeshNode, PowerOfTwoCube, InnerNodeUnderMesh}
import akka.actor._
import downearth.generation.{WorldGenerator, Master}
import AkkaMessages._


object Main extends Logger {
  var actorSystem:ActorSystem = null
  var game:ActorRef = null

  def main(args: Array[String]) {
    log.println( "started" )
    log.println( "Assertions " + (if( assertionsActivated ) "active" else "inactive" ))

    log.println( "Creating Actor System" )

    actorSystem = ActorSystem.create("gamecontext")
    game = actorSystem.actorOf( Props[Game].withDispatcher("akka.actor.single-thread-dispatcher"), "game" )

    game ! NextFrame
  }

}

object AkkaMessages {
  case object NextFrame
  case class LastFrame(timeStamp:Long)
  case class FinishedJob(area:PowerOfTwoCube, node:MeshNode)
  case class Predicted(area:PowerOfTwoCube)
}


class DebugLog extends Actor {
  def receive = {
    case Predicted(area) =>
      GlDraw addPredictedCuboid area
  }
}

class GameState(val master:ActorRef) { gameState =>
  val octree = WorldGenerator.genWorld(gameState)
  val physics = new BulletPhysics(gameState)
  val dynamicWorld = DynamicWorld.testScene
  lazy val renderer = new Renderer(gameState)

  lazy val mainWidget = new MainWidget(gameState)

  lazy val materialManager = new MaterialManager

  val tools = new {
    val constructionTool = new ConstructionTool(gameState)
    val shovel           = new Shovel(gameState)
    val testBuildTool    = new TestBuildTool(gameState)
  }

  val player = new Player(gameState)

  def openGLinit() {
    renderer
    mainWidget
    materialManager
  }
}

class FrameFactory extends Actor {
  def now = System.nanoTime
  val timeBetweenFrames = 1000000000 / downearth.Config.fpsLimit

  def receive = {
    case LastFrame(lastFrame) =>
      val timeSinceLastFrame = now - lastFrame
      if(timeSinceLastFrame < timeBetweenFrames ) {
        val timeToWait = timeBetweenFrames - timeSinceLastFrame
        Thread.sleep(timeToWait/1000000)
      }
      sender ! NextFrame
  }
}

class GameMailbox(settings: ActorSystem.Settings, config: com.typesafe.config.Config)
  extends UnboundedPriorityMailbox(
    // Create a new PriorityGenerator, lower prio means more important
    PriorityGenerator {
      case PoisonPill      => 0
      case NextFrame       => 2
      case job:FinishedJob => 3

      case otherwise       => 1
    })

//TODO on exception, shutdown actorsystem
class Game extends Actor with Publisher with Logger { gameLoop =>
  val master = context.actorOf( Props[Master], "master" )
  val debugLog = context.actorOf( Props[DebugLog], "debuglog" )
  val frameFactory = context.actorOf( Props[FrameFactory], "framefactory" )

  val gameState = new GameState(master)
  import gameState._


  override def preStart() {
    createOpenGLContext()
    checkOpenGLCapabilities()
    gameState.openGLinit()
    createWidgetSystem()
    Thread.currentThread().setPriority(Thread.MAX_PRIORITY)
  }

  def receive = {
    case NextFrame =>
        frame()
        frameFactory ! LastFrame(lastFrame)

    case FinishedJob(area, node) =>
      updateTimer.restart()

      octree.insert( area, node )
      physics.worldChange(area)

      lastUpdateDuration = updateTimer.readNanos

    case unknown =>
      println("Game: unknown message: " + unknown)
  }

  override def postStop() {
    Config.loader.save()
    TextureManager.delete()
    ObjManager.delete()
    context.system.shutdown()
  }

  def createWidgetSystem() {
    val wed = new WidgetEventDispatcher(mainWidget)
    wed.listenTo(gameLoop)
  }

  def createOpenGLContext() {
    val ca = new ContextAttribs(3,0).withDebug(true)
    val alpha = 8
    val depth = 16
    val stencil = 0
    val pf = new PixelFormat(alpha, depth, stencil)
    Display.setDisplayMode( new DisplayMode(Config.windowResolutionWidth,Config.windowResolutionHeight) )
    Display.setResizable(true)
    Display.setVSyncEnabled(false)
    Display.create(pf, ca)
    log.println( "display created" )
  }


  var snapshotRequest = 0L
  var snapshotCurrent = -1L

  val timeBetweenFrames = 1000000000 / downearth.Config.fpsLimit

  def now = System.nanoTime
  val starttime = now
  //def uptime = now - starttime

  var lastFrame = now
  var currentFps = 0
  var lastFrameCounterReset = starttime
  var frameCounter = 0

  // for measuring last frame
  val frameTimer = new Timer
  var lastFrameDuration:Long = 0

  // for measuring last octree update
  val updateTimer = new Timer
  var lastUpdateDuration:Long = 0

  def frame() {
    frameRateCalculations()
    frameTimer.restart()

    physics.update()

    handleInput()

    if( Config.streamWorld )
      octree stream player.pos

    renderer.draw()
    Display.update()

    lastFrameDuration = frameTimer.readNanos
  }


  def checkOpenGLCapabilities() { //TODO split checkOpenGLCapabilities
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

    //Mouse setGrabbed true
	}

	// Berechnet die Aktuelle Framerate
	def frameRateCalculations() {
		if(now - lastFrameCounterReset > 1000000000){
			currentFps = frameCounter
			lastFrameCounterReset = now
			frameCounter = 0
      val possibleUpdatesPerFrame = if(lastUpdateDuration > 0 && lastFrameDuration <= timeBetweenFrames)
        (timeBetweenFrames - lastFrameDuration) / lastUpdateDuration
      else 0

      Display.setTitle(s"$currentFps/${Config.fpsLimit} fps, frame: ${lastFrameDuration/1000000}/${timeBetweenFrames/1000000}ms, update: ${lastUpdateDuration/1000000}ms (${possibleUpdatesPerFrame}/frame)")
		}
		else
			frameCounter += 1
	
		lastFrame = now
	}

  var windowMode:DisplayMode = null

  def quit() {
    self ! PoisonPill
  }

  def handleInput() {
    import Mouse._
    import Keyboard._

    if( Display.isCloseRequested )
      quit()

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
        quit()
      case `keyScreenshot` =>
        screenShot( "screenshot" )
      case `keyMouseGrab` =>
        Mouse setGrabbed !Mouse.isGrabbed
      case `keyPlayerReset` =>
        player.resetPos
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
        physics.pause = !physics.pause
      case `keyDebugDraw` =>
        debugDraw += 1
      case `keyToggleGhostPlayer` =>
        player.toggleGhost()
      case `keyToggleInventory` =>
        mainWidget.inventory.visible = !mainWidget.inventory.visible
      case `keyJump` =>
        player.jump()
      case `keyIncOctreeDepth` =>
        octree.incDepth()
      case `keyGenerateNextUngenerated` =>
        val next = octree.getNextUngenerated
        println("next Ungenerated:" + next)
        for( area <- next )
          octree.generateArea(area)
        println("not finished:")
        octree.query(){
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
          mainWidget.resize( Vec2i(windowMode.getWidth, windowMode.getHeight) )
        }
        else {
          windowMode = Display.getDisplayMode
          val mode = Display.getDesktopDisplayMode
          assert(mode.isFullscreenCapable)
          Display.setDisplayModeAndFullscreen(mode)
          mainWidget.resize( Vec2i(mode.getWidth, mode.getHeight) )
        }
      case _ =>
    }

    if( Mouse.isGrabbed ) {

      // rotate with mouse
      delta_angle.y = -mouseDelta.x/300.0
      delta_angle.x = mouseDelta.y/300.0

      // Turbo mode
      if( turbo && Mouse.isButtonDown(0) )
        player.primaryAction()

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
            player.primaryAction()
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
        if( c.toInt != 0 ) {
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
                print(s"<${c.toInt}>")
            }
          }
          else
            print( Keyboard.getEventCharacter )
        }
      }

      if( Display.wasResized ) {
        mainWidget.resize(Vec2i(Display.getWidth, Display.getHeight))
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
    player.move(factor*(delta/max(1,length(delta)))*timeStep)
    player.rotate(2.0*delta_angle)
  }
}
