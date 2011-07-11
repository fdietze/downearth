package xöpäx

// What GL version you plan on using
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.{
  Display, DisplayMode,
  ARBShaderObjects, ARBVertexShader, ARBFragmentShader
}
import org.lwjgl.input._
import Keyboard._

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import simplex3d.data._
import simplex3d.data.float._
import org.newdawn.slick.Font

import Util._

object Main {
	import org.lwjgl.BufferUtils
	import java.nio.FloatBuffer
	
	implicit def sequence2FloatBuffer(s:Seq[Float]):FloatBuffer = {
		val buffer = BufferUtils.createFloatBuffer(s.size)
		s.foreach(buffer.put)
		buffer.flip
		buffer
	}
	
	val FRAMERATE = 6000
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
	
	init
	
	def showfps{
		val fps = "%d fps" format currentfps
		Draw addText fps
	}
	
	def frame{
		if(time-timestamp > 1000){
			currentfps = framecounter
			timestamp = time
			framecounter = 0
		}
		else
			framecounter += 1	
	
		timestep = (uptime - lastframe)/1000f
		lastframe = uptime
		
		Display.sync(FRAMERATE)
		Display.update
	}
	
	def main(args:Array[String]){
		while(!finished){
			logic
			Camera.apply
			lighting
			draw
			
			frame
		}
		terminate
	}

	def init{
		val displayMode = new DisplayMode(Camera.WIDTH, Camera.HEIGHT)
		Display.setTitle("Worldgen")
		Display.setDisplayMode(displayMode)
		Display.create()

		initshaders
		
		glEnable(GL_CULL_FACE)

		glEnable(GL_LIGHTING)
		glEnable(GL_COLOR_MATERIAL)
		glEnable(GL_LIGHT0)

		World
		
		Mouse.setGrabbed(true)
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

	def terminate{
		Display.destroy()
		sys.exit(0)
	}
	
	def lighting{
		//Add ambient light
		glLightModel(GL_LIGHT_MODEL_AMBIENT, Seq(0.2f, 0.2f, 0.2f, 1.0f));
	
		//Add positioned light

		glLight(GL_LIGHT0, GL_POSITION, Seq(Camera.position.x, Camera.position.y, Camera.position.z, 1.0f));

		//Add directed light
//		glLight(GL_LIGHT1, GL_DIFFUSE, Seq(0.5f, 0.2f, 0.2f, 1.0f));
//		glLight(GL_LIGHT1, GL_POSITION, Seq(-1.0f, 0.5f, 0.5f, 0.0f));
	}
	
	var turbo = false
	
	def logic{
		if(Display.isCloseRequested || isKeyDown(KEY_ESCAPE) )
			finished = true;
		
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
		
		val dingensdelta = Vec3(0)
		
		if(isKeyDown(KEY_I))
			dingensdelta.y += 1
		if(isKeyDown(KEY_K))
			dingensdelta.y -= 1
		if(isKeyDown(KEY_J))
			dingensdelta.x -= 1
		if(isKeyDown(KEY_L))
			dingensdelta.x += 1
		if(isKeyDown(KEY_PRIOR))
			dingensdelta.z += 1
		if(isKeyDown(KEY_NEXT))
			dingensdelta.z -= 1
		
		if(isKeyDown(KEY_F)){
			glPolygonMode( GL_FRONT_AND_BACK, GL_LINE )
			glDisable(GL_LIGHTING)
		}
		else{
			glPolygonMode( GL_FRONT_AND_BACK, GL_FILL )
			glEnable(GL_LIGHTING)
		}
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
					Dingens.resetBallPos			
//				case KEY_X =>
//					World.cube.move(Vec3i(1,0,0))
				case KEY_T =>
					turbo = if(turbo) false else true
				case _ =>
				}
			}
		}
		
		// Mouse Event Input
		
		
		if(turbo){
			if( Mouse isButtonDown 0 )
				Player.remove
			if( Mouse isButtonDown 1 )
				Player.build
		}
		else {
			while( Mouse.next ) {
				if( getEventButtonState ) {
					getEventButton match {
					case 1 =>
							Player.build
					case 0 =>
						Player.remove
					case _ =>
					}
				}
			}
		}
		
		//Dingens.move(4*dingensdelta*timestep)
		val factor = if(turbo) 16f else 4f
		Camera.move(factor*(delta/max(1,length(delta)))*timestep)
		//Dingens.force = Camera.makeRelative( delta/max(1,length(delta) ) )
		
		if(Mouse.isGrabbed) 
			Camera.turn(2f*delta_angle)
	}
	
	def draw{
		glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT )
		
		Camera.applyfrustum

//		Physics.update

		ARBShaderObjects.glUseProgramObjectARB(shader)
		World.draw
//		Dingens.draw
		ARBShaderObjects.glUseProgramObjectARB(0)
		
		glLoadIdentity
		Camera.applyortho
		showfps
		Draw.drawTexts
		glTranslatef(Camera.WIDTH/2,Camera.HEIGHT/2,0)
		Draw.crossHair
	}
}


