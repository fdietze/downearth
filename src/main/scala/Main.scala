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
		
		if( Config.fullscreen && displayMode.isFullscreenCapable )
			Display.setFullscreen(true)

		Display.setDisplayMode(displayMode)
		
		Display.create()
		
		if(Config.useshaders)
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
		
		if(isKeyDown(KEY_Q))
			delta_angle.z += 0.5f*timestep
		if(isKeyDown(KEY_E))
			delta_angle.z -= 0.5f*timestep
		if(isKeyDown(KEY_W))
			delta.z -= 1
		if(isKeyDown(KEY_S))
			delta.z += 1
		if(isKeyDown(KEY_A))
			delta.x -= 1
		if(isKeyDown(KEY_D))
			delta.x += 1
		
		val factor = if(turbo) Config.CameraTurboSpeed else Config.CameraSpeed
		Controller.move(factor*(delta/max(1,length(delta)))*timestep)
		
		if(Mouse.isGrabbed) 
			Controller.rotate(2f*delta_angle)
		
		if(isKeyDown(KEY_F)){
			glPolygonMode( GL_FRONT_AND_BACK, GL_LINE )
			glDisable(GL_LIGHTING)
		}
		else {
			glPolygonMode( GL_FRONT_AND_BACK, GL_FILL )
			glEnable(GL_LIGHTING)
		}
		// TODO: wofür dieser Kommentar?:
		// switches the mode how 3d noise is transformed into a 3d Hexaeder
		
		while ( Keyboard.next ) {
			if (getEventKeyState) {
				getEventKey match {
				case KEY_G =>
					if(Mouse isGrabbed)
						Mouse setGrabbed false
					else
						Mouse setGrabbed true
				case KEY_R =>
					Player.resetPos
				case KEY_RIGHT =>
					World.octree.move(Vec3i( 1,0,0))
				case KEY_LEFT =>
					World.octree.move(Vec3i(-1,0,0))
				case KEY_UP =>
					World.octree.move(Vec3i(0, 1,0))
				case KEY_DOWN =>
					World.octree.move(Vec3i(0,-1,0))
				case KEY_PRIOR =>
					World.octree.move(Vec3i(0,0, 1))
				case KEY_NEXT =>
					World.octree.move(Vec3i(0,0,-1))
				case KEY_F7 =>
					Config.streamWorld = !Config.streamWorld
				case KEY_F8 =>
					Config.frustumCulling = !Config.frustumCulling
				case KEY_T =>
					turbo = ! turbo
				case KEY_ESCAPE =>
					finished = true
				case KEY_P | KEY_PAUSE =>
					BulletPhysics.togglePause
				case KEY_F1 =>
					Config.debugDraw = !Config.debugDraw
				case KEY_TAB =>
					Player.toggleGhost
				case KEY_SPACE =>
					Controller.jump
				case KEY_1 =>
					DefaultHexaeder.id = 0
				case KEY_2 =>
					DefaultHexaeder.id = 1
				case KEY_3 =>
					DefaultHexaeder.id = 2
				case KEY_4 =>
					DefaultHexaeder.id = 3
				case KEY_5 =>
					DefaultHexaeder.id = 4
				case KEY_6 =>
					DefaultHexaeder.id = 5
				case KEY_7 =>
					DefaultHexaeder.id = 6
				case KEY_8 =>
					DefaultHexaeder.id = 7
				case KEY_9 =>
					DefaultHexaeder.id = 8
				case KEY_0 =>
					DefaultHexaeder.id = 9
				case _ =>
				}
			}
		}
		
		// Mouse Event Input
		
		if(turbo) {
			if( Mouse isButtonDown 0 )
				Controller.remove
			if( Mouse isButtonDown 1 )
				Controller.build
		}
		else {
			while( Mouse.next ) {
				if( getEventButtonState ) {
					getEventButton match {
					case 1 =>
						Controller.build
					case 0 =>
						Controller.remove
					case _ =>
					}
				}
			}
		}
		
		DefaultHexaeder.rotate( Mouse.getDWheel / 120 )
	}

	def draw {
		glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT )
		
		// the active Camera
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
