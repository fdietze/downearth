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
import downearth.gui.javafx.{JavaFxMain, HudController}
import downearth.gui.lwjgl.LwjglMain

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

			Renderer.draw()
			frame()

      swapBuffers()
      Display.sync(fpsLimit)
		}

		destroy()
	}

	def init {
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
