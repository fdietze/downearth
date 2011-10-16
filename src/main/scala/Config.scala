package openworld

import simplex3d.math.float.Vec3
import simplex3d.math.doublex.Vec3d
import noise.Noise.{noise3, noise3_prediction}
import noise.interval.{Interval, Volume}
import org.lwjgl.opengl.{Display, DisplayMode}

object Config{
	import ConfigLoader._
	
	val minMeshNodeSize = 8
	val worldWindowSize = 64
	val useshaders = loadBoolean("use_shaders") getOrElse false
	val vertexMaterials = false
	val smoothShading = loadBoolean("smooth_shading") getOrElse false
	
	val hexaederResolution = 8
	
	val skybox = loadBoolean("skybox") getOrElse false
	
	val ungeneratedDefault = UndefHexaeder
	val startpos = Vec3(0)
	
	val fpsLimit = loadInt("fps_limit") getOrElse 60
	
	// um den Meshjoin/-split Vorgang zu testen sollte dieser wert niedriger 
	// gesetzt werden (10000)
	val maxMeshVertexCount = 30000
	val numWorkingThreads = Runtime.getRuntime.availableProcessors
	val cameraSpeed = 2f
	val cameraTurboSpeed = 32f
	val startAsGhost = true
	
	def densityfunction(v:Vec3) = gen.density(v).toFloat
	def materialfunction(v:Vec3) = gen.material(v)
	def prediction(v1: Vec3, v2: Vec3) = gen.prediction(Volume(v1,v2))
	val saveWorld = false

	assert( worldWindowSize >= minMeshNodeSize )
	assert( worldWindowSize % minMeshNodeSize  == 0 )
	assert( (worldWindowSize / minMeshNodeSize) % 2 == 0 )
	
	var fullscreen = loadBoolean("fullscreen") getOrElse false
	
	// Vollbild-Modus mit höchster Auflösung
	val fullscreenDisplayMode = Display.getAvailableDisplayModes.maxBy( _.getWidth )
	
	val windowResolutionWidth  = loadInt("window_resolution_width")  getOrElse 1024
	val windowResolutionHeight = loadInt("window_resolution_height") getOrElse  768
	val windowDisplayMode     = new DisplayMode(windowResolutionWidth, windowResolutionHeight)
	
	def displayMode =
		if(fullscreen) 
			fullscreenDisplayMode
		else
			windowDisplayMode
	
	def screenWidth  = displayMode.getWidth
	def screenHeight = displayMode.getHeight

	val worldUpVector = Vec3.UnitZ
	
	import org.lwjgl.input.Keyboard._
	val keyForward  = loadKey("forward") getOrElse KEY_W
	val keyBackward = loadKey("backward") getOrElse KEY_S
	val keyLeft     = loadKey("left") getOrElse KEY_A
	val keyRight    = loadKey("right") getOrElse KEY_D
	val keyJump     = loadKey("jump") getOrElse KEY_SPACE
	
	val keyChooseHex0 = loadKey("choseHex01") getOrElse KEY_1
	val keyChooseHex1 = loadKey("choseHex02") getOrElse KEY_2
	val keyChooseHex2 = loadKey("choseHex03") getOrElse KEY_3
	val keyChooseHex3 = loadKey("choseHex04") getOrElse KEY_4
	val keyChooseHex4 = loadKey("choseHex05") getOrElse KEY_5
	val keyChooseHex5 = loadKey("choseHex06") getOrElse KEY_6
	val keyChooseHex6 = loadKey("choseHex07") getOrElse KEY_7
	val keyChooseHex7 = loadKey("choseHex08") getOrElse KEY_8
	val keyChooseHex8 = loadKey("choseHex09") getOrElse KEY_9
	val keyChooseHex9 = loadKey("choseHex10") getOrElse KEY_0
	
	val keyMouseGrab         = loadKey("mouse_grab") getOrElse KEY_G
	val keyPlayerReset       = loadKey("reset_pos") getOrElse KEY_R
	val keyTurbo             = loadKey("turbo") getOrElse KEY_T
	val keyQuit              = loadKey("quit") getOrElse KEY_ESCAPE
	val keyPausePhysics      = loadKey("pause") getOrElse KEY_P
	val keyToggleGhostPlayer = loadKey("toggle_ghost_player") getOrElse KEY_TAB

	val keyDebugDraw      = loadKey("debug_draw") getOrElse KEY_F1
	val keyWireframe      = loadKey("wire_frame") getOrElse KEY_F2
	val keyStreaming      = loadKey("streaming") getOrElse KEY_F3
	val keyFrustumCulling = loadKey("frustum_culling") getOrElse KEY_F4

	val keyFullScreen     = loadKey("fullscreen") getOrElse KEY_F11


	// settings changeable at runtime:
	var debugDraw = false
	var wireframe = false
	var streamWorld = true
	var frustumCulling = true
	var turbo = false
}

import xml.XML
import org.lwjgl.input.Keyboard.getKeyIndex

object ConfigLoader {
	val config = XML.load( getClass.getClassLoader.getResourceAsStream("config.xml") )
	
	def loadKey(name:String):Option[Int] = {
		config \ "keys" \ "key" find ( node => (node \ "@name").text == name) match {
		case Some(node) => 
			val key = getKeyIndex(node.text)
			if(key != 0)
				Some(key)
			else {
				System.err.println("Wrong Format in config.xml for key " + name)
				None
			}
		case None =>
			None
		}
	}
	
	def loadValue(name:String):Option[String] = {
		config \ "value" find ( node => (node \ "@name").text == name ) map ( _.text )
	}
	
	def loadBoolean(name:String):Option[Boolean] = 
		loadValue(name) match {
		case Some("false") => Some(false)
		case Some("true")  => Some(true)
		case Some(s)       => System.err.println("can't parse " + s + " as Boolean for key " + name); None
		case _ => None
	}
	
	def loadInt(name:String):Option[Int] = {
		val option = loadValue(name)
		try {
			loadValue(name) map ( _.toInt )
		}
		catch {
			case _ => 
				System.err.println("can't parse " + option.get + " as Int for key " + name)
				None
		}
	}
}

