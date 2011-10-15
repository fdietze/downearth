package openworld

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.{
  Display, ARBShaderObjects, ARBVertexShader, ARBFragmentShader
}
import org.lwjgl.input._
import Keyboard._

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import Util._
import Config._

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
			logic
			draw
			frame
		}
		terminate
	}

	def init {
		Display.setTitle("Open World")
		if(fullscreen)
			Display.setDisplayModeAndFullscreen(displayMode)
		else
			Display.setDisplayMode(displayMode)
		Display.create()
		
		if(useshaders)
			initshaders
		
		glEnable(GL_CULL_FACE)
		
		// TODO: in die Kamera?
		glEnable(GL_LIGHTING)
		glEnable(GL_COLOR_MATERIAL)
		glEnable(GL_LIGHT0)

		//initilisiert die Welt, um danach erst die Maus zu fangen.
		World 
		
		Mouse.setGrabbed(true)
	}

	def terminate {
		Display.destroy()
		WorldSerializer.save(World.octree)
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

	def showfps {
		val fps = "%d fps" format currentfps
		Draw addText fps
	}
	
	// Behandelt alle Benutzereingaben über Maus und Tastatur
	def logic {
		if(Display.isCloseRequested)
			finished = true
		
		val delta = Vec3(0)
		val delta_angle = Vec3(0)
		
		import Mouse._
		
		delta_angle.y -= getDX/300f
		delta_angle.x = getDY/300f
		
		if(isKeyDown(keyForward))
			delta.z -= 1
		if(isKeyDown(keyBackward))
			delta.z += 1
		if(isKeyDown(keyLeft))
			delta.x -= 1
		if(isKeyDown(keyRight))
			delta.x += 1
		
		val factor = if(turbo) cameraTurboSpeed else cameraSpeed
		Player.move(factor*(delta/max(1,length(delta)))*timestep)
		
		if(Mouse.isGrabbed) 
			Player.rotate(2f*delta_angle)
		
		if( wireframe ){
			glPolygonMode( GL_FRONT_AND_BACK, GL_LINE )
			glDisable(GL_LIGHTING)
		}
		else {
			glPolygonMode( GL_FRONT_AND_BACK, GL_FILL )
			glEnable(GL_LIGHTING)
		}
		
		while ( Keyboard.next ) {
			if (getEventKeyState) {
				getEventKey match {
				case `keyMouseGrab` =>
					Mouse setGrabbed !(Mouse isGrabbed)
				case `keyPlayerReset` =>
					Player.resetPos
				case `keyStreaming` =>
					streamWorld = !streamWorld
				case `keyWireframe` =>
					wireframe = !wireframe
				case `keyFrustumCulling` =>
					frustumCulling = !frustumCulling
				case `keyFullScreen` =>
					fullscreen = !fullscreen
					Display.setFullscreen(fullscreen)
					Display.setDisplayMode(displayMode)
					if( fullscreen )
						Mouse setGrabbed true
				case `keyTurbo` =>
					turbo = ! turbo
				case `keyQuit` =>
					finished = true
				case `keyPausePhysics` =>
					BulletPhysics.togglePause
				case `keyDebugDraw` =>
					debugDraw = !debugDraw
				case `keyToggleGhostPlayer` =>
					Player.toggleGhost
				case `keyJump` =>
					Player.jump
				case `keyChooseHex0` =>
					BuildInterface.id = 0
				case `keyChooseHex1` =>
					BuildInterface.id = 1
				case `keyChooseHex2` =>
					BuildInterface.id = 2
				case `keyChooseHex3` =>
					BuildInterface.id = 3
				case `keyChooseHex4` =>
					BuildInterface.id = 4
				case `keyChooseHex5` =>
					BuildInterface.id = 5
				case `keyChooseHex6` =>
					BuildInterface.id = 6
				case `keyChooseHex7` =>
					BuildInterface.id = 7
				case `keyChooseHex8` =>
					BuildInterface.id = 8
				case `keyChooseHex9` =>
					BuildInterface.id = 9
				case _ =>
				}
			}
		}
		
		// Mouse Event Input
		
		if(turbo) {
			if( Mouse isButtonDown 0 )
				BuildInterface.build(Player.position, Player.direction)
		}
		
		while( Mouse.next ) {
			if( getEventButtonState ) {
				getEventButton match {
				case 0 => // Left Click
					if( !(Mouse isGrabbed) )
						Mouse setGrabbed true
					else
						if(!turbo)
							BuildInterface.build(Player.position, Player.direction)
				case 1 => // Right Click
					BuildInterface.buildStatus = !BuildInterface.buildStatus
				case _ =>
				}
			}
		}
		
		//TODO: why 120?
		BuildInterface.rotate( Mouse.getDWheel / 120 )
	}

	def draw {
		glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT )
		
		BulletPhysics.update
		
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
