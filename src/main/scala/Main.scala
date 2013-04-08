package openworld

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.{
  Display, ARBShaderObjects, ARBVertexShader, ARBFragmentShader
}
import org.lwjgl.input._

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import Util._
import Config._

import gui.{MainWidget, GUI}

object Main {
	var finished = false
	
	var textCache:List[(Vec2i,String)] = Nil
	
	def time = System.currentTimeMillis
	val starttime = time
	def uptime = time - starttime

	var lastframe = uptime
	var timestep = 0f
	var currentfps = 0
	var timestamp = starttime
	var framecounter = 0
	
	var shader = 0
	var vertshader = 0
	var fragshader = 0
	
	def main(args:Array[String]) {
		init
		while(!finished) {
			BulletPhysics.update
			input
			draw
			frame
		}
		terminate
	}

	def init {
		Display	setTitle "Open World"
		if(fullscreen) //TODO: reset mousegrab after switching fullscreen
			Display.setDisplayModeAndFullscreen(displayMode)
		else
			Display.setDisplayMode(displayMode)
		Display.create()
		
		if(useshaders)
			initshaders

		World
		
		Mouse setGrabbed true
	}

	def terminate {
		Display.destroy()
		//WorldSerializer.save(World.octree)
		sys.exit(0)
	}
	
	// Berechnet die Aktuelle Framerate
	def frame {
		if(time - timestamp > 1000){
			currentfps = framecounter
			timestamp = time
			framecounter = 0
		}
		else
			framecounter += 1	
	
		timestep = (uptime - lastframe)/1000f
		lastframe = uptime
		
		Display.sync(fpsLimit)
		Display.update
	}
	
	def setFullscreen(set:Boolean = true) {
		fullscreen = set
		Display.setFullscreen(fullscreen)
		Display.setDisplayMode(displayMode)
		if( fullscreen ) {
			Mouse setGrabbed true
		}
		
		MainWidget.size := Vec2i(screenWidth, screenHeight)
		
		if( ! GUI.inventory.moved )
			GUI.inventory.setTopRight
	}
	
	var mousePos = Vec2i(Mouse.getX, screenHeight-Mouse.getY)
	var lastMousePos = mousePos.clone
	// Behandelt alle Benutzereingaben über Maus und Tastatur
	def input {
		import Mouse._
		import Keyboard._

		if(Display.isCloseRequested)
			finished = true

		lastMousePos := mousePos
		mousePos := Vec2i(getX, screenHeight-Mouse.getY)


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

		
		if( Mouse isGrabbed ) {
		
			// rotate with mouse
			delta_angle.y = -mouseDelta.x/300f
			delta_angle.x = mouseDelta.y/300f
			
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
					case `keyFullScreen` =>
						setFullscreen(!fullscreen)
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
						Mouse setCursorPosition(screenWidth / 2, screenHeight / 2)
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
					case `keyFullScreen` =>
						setFullscreen(!fullscreen)
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
		Player.move(factor*(delta/max(1,length(delta)))*timestep)
		Player.rotate(2f*delta_angle)

	}

	def draw {
		glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT )
		
		Player.camera.renderScene
		GUI.renderScene
	}

	def initshaders {
		shader = ARBShaderObjects.glCreateProgramObjectARB
		if( shader != 0 ) {
			vertshader = ARBShaderObjects.glCreateShaderObjectARB(ARBVertexShader.GL_VERTEX_SHADER_ARB)
			fragshader=ARBShaderObjects.glCreateShaderObjectARB(ARBFragmentShader.GL_FRAGMENT_SHADER_ARB)
			if( vertshader != 0 ) {
				val vertexPath = getClass.getClassLoader.getResource("shaders/screen.vert").getPath
				val vertexCode = io.Source.fromFile(vertexPath).mkString
				ARBShaderObjects.glShaderSourceARB(vertshader, vertexCode)
				ARBShaderObjects.glCompileShaderARB(vertshader)
			}
			
			if( fragshader != 0 ) {
				val fragPath = getClass.getClassLoader.getResource("shaders/screen.frag").getPath
				val fragCode = io.Source.fromFile(fragPath).mkString
				ARBShaderObjects.glShaderSourceARB(fragshader, fragCode)
				ARBShaderObjects.glCompileShaderARB(fragshader)
			}
			
			if(vertshader !=0 && fragshader !=0) {
				ARBShaderObjects.glAttachObjectARB(shader, vertshader)
				ARBShaderObjects.glAttachObjectARB(shader, fragshader)
				ARBShaderObjects.glLinkProgramARB(shader)
				ARBShaderObjects.glValidateProgramARB(shader)
			}
		}
		printLogInfo(shader)
		printLogInfo(vertshader)
		printLogInfo(fragshader)
	}
	
	// mit dieser Methode kann ein Bereich umschlossen werden,
	// der mit Shadern gerendert werden soll
	def activateShader(func: => Unit) {
		import ARBShaderObjects._
		val useshaders = shader != 0 && vertshader != 0 && fragshader != 0
		if(useshaders) {
			glUseProgramObjectARB(shader)
			glUniform1fARB( glGetUniformLocationARB( shader, "time" ), uptime.toFloat/1000f )
		}
		
		func
		
		if(useshaders)
			glUseProgramObjectARB(0)
	}
}
