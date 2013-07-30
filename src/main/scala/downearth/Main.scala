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
    game = actorSystem.actorOf( Props[GameLoop].withDispatcher("akka.actor.single-thread-dispatcher"), "gameloop" )

    game ! NextFrame

    // TODO: shut down actorsystem on crash
    // http://doc.akka.io/docs/akka/snapshot/general/supervision.html#The_Top-Level_Supervisors
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

class GameState(val gameLoop:ActorRef, val master:ActorRef, val debugLog:ActorRef) { gameState =>
  val octree = WorldGenerator.generateInitialWorld(gameState)
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
  val frameState = new FrameState(gameState)
  val input = new Input(gameState)
  var windowMode:DisplayMode = null


  def openGLinit() {
    renderer
    mainWidget
    materialManager
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


class FrameGenerator extends Actor {
  def now = System.nanoTime
  def timeBetweenFrames = 1000000000 / downearth.Config.fpsLimit

  def receive = {
    case LastFrame(lastFrame) =>
      val timeSinceLastFrame = now - lastFrame
      if(timeSinceLastFrame < timeBetweenFrames ) {
        val timeToWait = timeBetweenFrames - timeSinceLastFrame
        val waitMillis = timeToWait/1000000
        val waitNanos = timeToWait - (waitMillis*1000000)
        Thread.sleep(waitMillis, waitNanos.toInt)
      }
      sender ! NextFrame
  }
}

//TODO on exception, shutdown actorsystem
class GameLoop extends Actor with Logger { gameLoop =>
  val master = context.actorOf( Props[Master], "master" )
  val debugLog = context.actorOf( Props[DebugLog], "debuglog" )
  val frameFactory = context.actorOf( Props[FrameGenerator], "framegenerator" )

  val gameState = new GameState(self, master, debugLog)
  import gameState._


  override def preStart() {
    Thread.currentThread().setPriority(Thread.MAX_PRIORITY)

    createOpenGLContext()
    checkOpenGLCapabilities()
    gameState.openGLinit()
    createWidgetSystem()

    octree.generateInitialAreaAroundPlayer()

    //Mouse setGrabbed true
  }

  def receive = {
    case NextFrame =>
      frameState.render()
      frameFactory ! LastFrame(frameState.lastFrame)

    case FinishedJob(area, node) =>
      import frameState._
      updateTimer.restart()

      octree.insert( area, node )
      physics.worldChange(area)
      updateCounter += 1
      generationQueueSize -= 1

      lastUpdateDuration = updateTimer.readNanos
  }


  override def unhandled(message: Any) {
    log.println("GameLoop: unknown message: " + message)
  }

  override def postStop() {
    Config.loader.save()
    TextureManager.delete()
    ObjManager.delete()
    context.system.shutdown()
  }

  def createWidgetSystem() {
    val wed = new WidgetEventDispatcher(mainWidget)
    wed.listenTo(input)
  }

  def createOpenGLContext() {
    val ca = new ContextAttribs(3,0).withDebug(false)
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

  //var snapshotRequest = 0L
  //var snapshotCurrent = -1L


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
      println("no debug output")
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
	}
}
