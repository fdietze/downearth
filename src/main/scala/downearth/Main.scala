package downearth

import org.lwjgl.opengl._
import org.lwjgl.input._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import Config._

import gui.MainWidget
import org.lwjgl.util.stream.{StreamUtil, StreamHandler}
import openworld.Util._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL11.glGetInteger
import org.lwjgl.opengl.AMDDebugOutput._
import org.lwjgl.opengl.ARBDebugOutput._
import java.util.concurrent.atomic.AtomicLong
import org.lwjgl.util.stream.StreamUtil.RenderStreamFactory
import scala.compat.Platform
import scala.collection.mutable.SynchronizedQueue

class GameLoop(val readHandler: StreamHandler, guiController:HudController) {

  /* adopted from Gears.java */

  val pendingRunnables = new SynchronizedQueue[() => Unit]

  if ((Pbuffer.getCapabilities & Pbuffer.PBUFFER_SUPPORTED) == 0)
    throw new UnsupportedOperationException("Support for pbuffers is required.")

  val pbuffer = new Pbuffer(1, 1, new PixelFormat(), null, null, new ContextAttribs())
  pbuffer.makeCurrent()

  val drawable = pbuffer // wofür auch immer

  val caps = GLContext.getCapabilities
  val maxSamples =
  if (caps.OpenGL30 || (caps.GL_EXT_framebuffer_multisample && caps.GL_EXT_framebuffer_blit))
    glGetInteger(GL_MAX_SAMPLES)
  else
    1

  if ( caps.GL_ARB_debug_output )
	  glDebugMessageCallbackARB(new ARBDebugOutputCallback())
  else if ( caps.GL_AMD_debug_output )
    glDebugMessageCallbackAMD(new AMDDebugOutputCallback())

  private var m_transfersToBuffer = 3
  def transfersToBuffer = m_transfersToBuffer
  def transfersToBuffer_=(transfersToBuffer: Int) {
    if (m_transfersToBuffer != transfersToBuffer) {
      m_transfersToBuffer = transfersToBuffer
    }
    resetStreams
  }

  private var m_samples = 1
  def samples = m_samples
  def samples_=(samples:Int) {
    if (m_samples != samples) {
      m_samples = samples
      resetStreams
    }
  }

  private var m_renderStreamFactory = StreamUtil.getRenderStreamImplementation
  var renderStream = m_renderStreamFactory.create(readHandler, 1, transfersToBuffer)
  def renderStreamFactory = m_renderStreamFactory
  def renderStreamFactory_=(renderStreamFactory: RenderStreamFactory){
    pendingRunnables.enqueue(() => {
      if (renderStream ne null)
        renderStream.destroy
      m_renderStreamFactory = renderStreamFactory
      renderStream = renderStreamFactory.create(renderStream.getHandler, samples, transfersToBuffer)
    })
  }

  private def resetStreams {
    pendingRunnables.enqueue(() => {
      renderStream.destroy
      renderStream = renderStreamFactory.create(renderStream.getHandler, samples, transfersToBuffer)
      updateSnapshot()
    })
  }

  def updateSnapshot() {
    snapshotRequest.incrementAndGet
  }

  private def drainPendingActionsQueue() {
    while ( !pendingRunnables.isEmpty ) {
      val runable = pendingRunnables.dequeue()
      runable()
    }
  }

  val snapshotRequest = new AtomicLong
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
	
	def run() {
		init
		while(!finished) {
      drainPendingActionsQueue()
      renderStream.bind()

			BulletPhysics.update()
//		input()

      // these updates need to be done from the javaFx thread
      guiController.runLater {
        guiController.playerPosition.setText( "Player Position: " + round10(Player.position) )
        guiController.drawcalls.setText( "drawcalls: " + Renderer.drawcalls + ", empty: " + Renderer.emptydrawcalls + "" )
        //guiController.frustumCulledNodes.setText( "frustum culled nodes: " + World.frustumculls )
      }

			Renderer.draw()
			frame()

      renderStream.swapBuffers()
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
    renderStream.destroy()
    pbuffer.destroy()

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
	def input() {
		import Mouse._
		import Keyboard._

		if(Display.isCloseRequested)
			finished = true

		lastMousePos := mousePos
		mousePos := Vec2i(getX, JavaFxMain.height.toInt-Mouse.getY)


		val mouseDelta = Vec2i(getDX, getDY)
		// Move and rotate player
		val delta = Vec3(0)
		val delta_angle = Vec3(0)

		if(isKeyDown(keyForward))
			delta.z -= 1
		if(isKeyDown(keyBackward))
			delta.z += 1
		if(isKeyDown(keyLeft))
			delta.x -= 1
		if(isKeyDown(keyRight))
			delta.x += 1

		
		if( Mouse.isGrabbed ) {
		
			// rotate with mouse
			delta_angle.y = -mouseDelta.x/300.0
			delta_angle.x = mouseDelta.y/300.0
			
			// Turbo mode
			if( turbo && Mouse.isButtonDown(0) )
				Player.primaryAction
			
			
			// Keyboard Events
			while ( Keyboard.next ) {
				if (getEventKeyState) {
					getEventKey match {
					case `keyMouseGrab` =>
						Mouse setGrabbed false
					case `keyPlayerReset` =>
						Player.resetPos
					case `keyStreaming` =>
						streamWorld = !streamWorld
					case `keyWireframe` =>
						wireframe = !wireframe
					case `keyFrustumCulling` =>
						frustumCulling = !frustumCulling
					case `keyScreenshot` =>
						screenShot( "screenshot" )
					case `keyTurbo` =>
						turbo = ! turbo
						DisplayEventManager.showEventText("Turbo is "+(if(turbo) "on" else "off")+"." )
					case `keyQuit` =>
						finished = true
					case `keyPausePhysics` =>
						BulletPhysics.pause = !BulletPhysics.pause
					case `keyDebugDraw` =>
						debugDraw = !debugDraw
					case `keyToggleGhostPlayer` =>
						Player.toggleGhost
					case `keyJump` =>
						Player.jump
					case _ =>
					}
				}
			}
			
			// Mouse events
			while( Mouse.next ) {
				( getEventButton, getEventButtonState ) match {
					case (0 , true) => // left down
					case (0 , false) => // left up
						Player.primaryAction
					case (1 , true) => // right down
					case (1 , false) => // right up
						//Player.secondarybutton
						Mouse setGrabbed false
						Mouse setCursorPosition(JavaFxMain.width.toInt / 2, JavaFxMain.height.toInt / 2)
					case (-1, false) => // wheel
						// Player.updownbutton( Mouse.getDWheel / 120 )
					case _ =>
				}
			}
		}
		else { // if Mouse is not grabbed

			if( mouseDelta != Vec2i(0) ) { // if Mouse is moved
				MainWidget.invokeMouseMoved(lastMousePos, mousePos)
			}
		
			// Keyboard Events
			while ( Keyboard.next ) {
				if (getEventKeyState) {
					getEventKey match {
					case `keyMouseGrab` =>
						Mouse setGrabbed true
					case `keyScreenshot` =>
						screenShot( "screenshot" )
					case `keyQuit` =>
						finished = true
					case _ =>
					}
				}
			}
		
			// Mouse events
			while( Mouse.next ) {
				( getEventButton, getEventButtonState ) match {
					case (0 , true) => // left down
						MainWidget.invokeMouseDown(mousePos)
					case (0 , false) => // left up
						MainWidget.invokeMouseUp(mousePos)
					case (1 , true) => // right down
					case (1 , false) => // right up
						Mouse setGrabbed true
					case (-1, false) => // wheel
					case _ =>
				}
			}
		
		}
		
		val factor = if(turbo) cameraTurboSpeed else cameraSpeed
		Player.move(factor*(delta/max(1,length(delta)))*timeStep)
		Player.rotate(2.0*delta_angle)

	}
}
