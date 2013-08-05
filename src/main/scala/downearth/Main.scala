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
import akka.util.Timeout
import scala.concurrent.{ExecutionContext, Await}
import scala.concurrent.duration._
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import Config._
import akka.routing.RoundRobinRouter

//import downearth.server.LocalServer
import downearth.worldoctree.{MeshNode, PowerOfTwoCube, InnerNodeUnderMesh}
import akka.actor._
import downearth.generation.{WorldGenerator, Worker}
import AkkaMessages._
import akka.actor.ActorDSL._

object Main extends Logger {
  //var actorSystem:ActorSystem = null
  //var gameLoop:ActorRef = null

  def main(args: Array[String]) {
    log.println( "Engine started" )
    log.println( "Assertions: " + (if( assertionsActivated ) "active" else "inactive" ))

    implicit val actorSystem = ActorSystem.create("gamecontext")

    val supervisor = actor(new Act {
      val gameLoop = context.actorOf( Props[GameLoop].withDispatcher("akka.actor.single-thread-dispatcher"), "gameloop" )
      val dyingChild = context.watch(gameLoop)
      become {
        case Terminated(terminatedActor) =>
          actorSystem.shutdown()
      }
    })

  }

}

object AkkaMessages {
  case object NextFrame
  case class LastFrame(timeStamp:Long)

  case class GeneratingJob(area:PowerOfTwoCube, playerPos:ReadVec3)
  case class FinishedGeneratingJob(area:PowerOfTwoCube, node:MeshNode)
  case class Predicted(area:PowerOfTwoCube)
}

class DebugLog extends Actor {
  def receive = {
    case Predicted(area) =>
      GlDraw addPredictedCuboid area
  }
}

class GameState(val gameLoop:ActorRef, val workers:ActorRef, val debugLog:ActorRef) { gameState =>
  val octree = WorldGenerator.generateInitialWorld(gameState)
  val physics = new BulletPhysics(gameState)
  val dynamicWorld = DynamicWorld.testScene

  lazy val renderer = new Renderer(gameState)
  lazy val mainWidget = new MainWidget(gameState)
  //TODO: val resources = new Resources

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
  }
}

class GameMailbox(settings: ActorSystem.Settings, config: com.typesafe.config.Config)
  extends UnboundedPriorityMailbox(
    // Create a new PriorityGenerator, lower prio means more important
    PriorityGenerator {
      case PoisonPill      => 0
      case NextFrame       => 2
      case job:FinishedGeneratingJob => 3

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

class GameLoop extends Actor with Logger { gameLoop =>
  val frameFactory = context.actorOf( Props[FrameGenerator], "framegenerator" )
  val debugLog = context.actorOf( Props[DebugLog], "debuglog" )
  val workers = context.actorOf( Props(classOf[Worker], self, debugLog)
    .withRouter(RoundRobinRouter(Config.numWorkingThreads))
    .withDispatcher("akka.actor.worker-dispatcher")
    , "worker-router")

  val gameState = new GameState(self, workers, debugLog)
  import gameState._


  override def preStart() {
    Thread.currentThread().setPriority(Thread.MAX_PRIORITY)

    createOpenGLContext()
    checkOpenGLCapabilities()
    gameState.openGLinit()
    createWidgetSystem()

    octree.generateInitialAreaAroundPlayer()

    self ! NextFrame

    //Mouse setGrabbed true
  }

  def receive = {
    case NextFrame =>
      frameState.render()
      frameFactory ! LastFrame(frameState.lastFrame)

    case FinishedGeneratingJob(area, node) =>
      import frameState._
      octree.insert( area, node )
      physics.worldChange(area)
      updateCounter += 1
      generationQueueSize -= 1
  }


  override def unhandled(message: Any) {
    log.println("GameLoop: unknown message: " + message)
  }

  override def postStop() {
    Config.loader.save()
    TextureManager.delete()
    ObjManager.delete()
  }

  def createWidgetSystem() {
    val wed = new WidgetEventDispatcher(mainWidget)
    wed.listenTo(input)
  }

  def createOpenGLContext() {
    val ca = new ContextAttribs(3,0).withDebug(Config.lwjglDebug)
    val alpha = 8
    val depth = 16
    val stencil = 0
    val pf = new PixelFormat(alpha, depth, stencil)
    Display.setDisplayMode( new DisplayMode(Config.windowResolutionWidth,Config.windowResolutionHeight) )
    Display.setResizable(true)
    Display.setVSyncEnabled(true)
    Display.create(pf, ca)
    log.println( "display created" )
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
