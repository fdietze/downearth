package downearth

import scala.collection.mutable.SynchronizedQueue

import org.lwjgl.opengl._
import org.lwjgl.input._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import downearth.gui.{MainWidget}
import downearth.rendering.Renderer
import downearth.generation.WorldNodeGenerator
import downearth.util._
import downearth.Config._
import downearth.gui.lwjgl.LwjglMain
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL11.glGetInteger
import org.lwjgl.opengl.ARBDebugOutput._
import org.lwjgl.opengl.AMDDebugOutput._
import downearth.world.World

object Main extends LwjglMain

abstract class GameLoop {

  var snapshotRequest = 0L
  var snapshotCurrent = -1L

	var finished = false
	
	def time = System.currentTimeMillis
	val starttime = time
	def uptime = time - starttime

	var lastFrame = uptime
	var timeStep = 0.0
	var currentFps = 0
	var timestamp = starttime
	var frameCounter = 0

  def swapBuffers()
  def extraLoopOperation()
	
	def run() {
		init
		while(!finished) {

      BulletPhysics.update()
      // input()

      extraLoopOperation()

      World.octree.makeUpdates()
      if(Config.streamWorld) {
        World.octree stream Player.position
      }

			Renderer.draw()
			frame()

      swapBuffers()
      Display.sync(fpsLimit)
		}

		destroy()
	}

	def init {


    val caps = GLContext.getCapabilities
    val maxSamples =
      if (caps.OpenGL30 || (caps.GL_EXT_framebuffer_multisample && caps.GL_EXT_framebuffer_blit))
        glGetInteger(GL_MAX_SAMPLES)
      else
        1

    if ( caps.GL_ARB_debug_output ) {
      println("GL_ARB_debug_output")
      glDebugMessageCallbackARB(new ARBDebugOutputCallback())
    }
    else if ( caps.GL_AMD_debug_output ) {
      println("GL_ARB_debug_output")
      glDebugMessageCallbackAMD(new AMDDebugOutputCallback())
    }

    glClearColor(91/255f,203/255f,255/255f,1f)

		if(useshaders)
			Renderer.initshaders

		World
		
		Mouse setGrabbed true
	}

	def destroy() {

    WorldNodeGenerator.actorSystem.shutdown()

    println("all destroyed")

		// WorldSerializer.save(World.octree)
		// sys.exit(0)
	}
	
	// Berechnet die Aktuelle Framerate
	def frame() {
		if(time - timestamp > 1000){
			currentFps = frameCounter
			timestamp = time
			frameCounter = 0
		}
		else
			frameCounter += 1
	
		timeStep = (uptime - lastFrame)/1000.0
		lastFrame = uptime
	}
	
	var mousePos = Vec2i(0,0)
	var lastMousePos = mousePos.clone
	// Behandelt alle Benutzereingaben über Maus und Tastatur
}
