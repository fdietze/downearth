package openworld

import simplex3d.math.float.Vec3
import simplex3d.math.doublex.Vec3d
import noise.Noise.{noise3, noise3_prediction}
import noise.interval.{Interval, Volume}
import org.lwjgl.opengl.{Display, DisplayMode}

object Config{
	val minMeshNodeSize = 16
	val worldWindowSize = 128
	val useshaders = false
	val smoothShading = false // experimental
	val hexaederResolution = 8
	
	val skybox = false
	
	val ungeneratedDefault = UndefHexaeder
	val startpos = Vec3(0)
	val fpsLimit = 60
	
	// um den Meshjoin/-split Vorgang zu testen sollte dieser wert niedriger 
	// gesetzt werden (10000)
	val maxMeshVertexCount = 30000
	val numWorkingThreads = Runtime.getRuntime.availableProcessors
	val cameraSpeed = 2f
	val cameraTurboSpeed = 32f
	
	def densityfunction(v:Vec3) = gen.density(v).toFloat
	def prediction(v1: Vec3, v2: Vec3) = gen.prediction(Volume(v1,v2))
	val saveWorld = false

	assert( worldWindowSize >= minMeshNodeSize )
	assert( worldWindowSize % minMeshNodeSize  == 0 )
	assert( (worldWindowSize / minMeshNodeSize) % 2 == 0 )
	
	var fullscreen = false
	
	// Vollbild-Modus mit höchster Auflösung
	val fullscreenDisplayMode = Display.getAvailableDisplayModes.maxBy( _.getWidth )
	val windowDisplayMode = new DisplayMode(1024, 768)
	def displayMode =
		if(fullscreen) 
			fullscreenDisplayMode
		else
			windowDisplayMode
	
	def screenWidth  = displayMode.getWidth
	def screenHeight = displayMode.getHeight

	val worldUpVector = Vec3.UnitZ
	
	
	import org.lwjgl.input.Keyboard._
	val keyForward  = KEY_W
	val keyBackward = KEY_S
	val keyLeft     = KEY_A
	val keyRight    = KEY_D
	val keyJump     = KEY_SPACE
	
	val keyChooseHex0 = KEY_1
	val keyChooseHex1 = KEY_2
	val keyChooseHex2 = KEY_3
	val keyChooseHex3 = KEY_4
	val keyChooseHex4 = KEY_5
	val keyChooseHex5 = KEY_6
	val keyChooseHex6 = KEY_7
	val keyChooseHex7 = KEY_8
	val keyChooseHex8 = KEY_9
	val keyChooseHex9 = KEY_0
	
	val keyMouseGrab         = KEY_G
	val keyPlayerReset       = KEY_R
	val keyTurbo             = KEY_T
	val keyQuit              = KEY_ESCAPE
	val keyPausePhysics      = KEY_P
	val keyToggleGhostPlayer = KEY_TAB

	val keyDebugDraw      = KEY_F1
	val keyWireframe      = KEY_F2
	val keyFrustumCulling = KEY_F3
	val keyStreaming      = KEY_F4

	val keyFullScreen     = KEY_F11


	// settings changeable at runtime:
	var debugDraw = false
	var wireframe = false
	var streamWorld = false
	var frustumCulling = true

	var turbo = false
}

