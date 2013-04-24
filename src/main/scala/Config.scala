package openworld

import simplex3d.math.double.Vec3
import simplex3d.math.doublex.Vec3d
import noise.Noise.{noise3, noise3_prediction}
import noise.interval.{Interval, Volume}
import org.lwjgl.opengl.{Display, DisplayMode}

object Config{
	import ConfigLoader._
	
	// TODO: minMeshNodeSize wird nurnoch fürs Streaming verwendet (?).
	// => umbenennen oder entfernen.
	// Wenn die Größe gleich der worldWindowSize gesetzt wird,
	// erhält man automatisch ein hierarchisches streaming.
	var minMeshNodeSize = loadInt("minMeshNodeSize") getOrElse 16
	var minPredictionSize = loadInt("minPredictionSize") getOrElse minMeshNodeSize
	var kdTreePrediction = loadBoolean("kdTreePrediction") getOrElse true
	
	var worldWindowSize = loadInt("worldWindowSize") getOrElse 64
	val useshaders = loadBoolean("useShaders") getOrElse false
	val vertexMaterials = false
	val smoothShading = loadBoolean("smoothShading") getOrElse false
	
	val hexaederResolution = 8
	
	val skybox = loadBoolean("skybox") getOrElse false
	
	val ungeneratedDefault = Leaf(FullHexaeder,-1)
	val startpos = Vec3(0,0,5) // TODO: Fix streaming with other start position
	
	val fpsLimit = loadInt("fpsLimit") getOrElse 60
	
	// um den Meshjoin/-split Vorgang zu testen sollte dieser wert niedriger 
	// gesetzt werden (10000)
	val maxMeshVertexCount = 30000
	val numWorkingThreads = Runtime.getRuntime.availableProcessors
	val cameraSpeed = 2
	val cameraTurboSpeed = 32
	val startAsGhost = false
	val buildrange = 100
	
	def densityfunction(v:Vec3) = gen.density(v)
	def materialfunction(v:Vec3) = gen.material(v)
	def prediction(v:Volume) = gen.prediction(v)
	val saveWorld = false

	var fullscreen = loadBoolean("fullscreen") getOrElse false

	// Vollbild-Modus mit höchster Auflösung
	def fullscreenDisplayMode = Display.getAvailableDisplayModes.maxBy( _.getWidth )
	
	val windowResolutionWidth  = loadInt("windowResolutionWidth")  getOrElse 1024
	val windowResolutionHeight = loadInt("windowResolutionHeight") getOrElse  768

	val worldUpVector = Vec3.UnitZ
	
	import org.lwjgl.input.Keyboard._
	val keyForward  = loadKey("forward") getOrElse KEY_W
	val keyBackward = loadKey("backward") getOrElse KEY_S
	val keyLeft     = loadKey("left") getOrElse KEY_A
	val keyRight    = loadKey("right") getOrElse KEY_D
	val keyJump     = loadKey("jump") getOrElse KEY_SPACE
	
	val keyMouseGrab         = loadKey("mouse_grab") getOrElse KEY_G
	val keyPlayerReset       = loadKey("reset_pos") getOrElse KEY_R
	val keyTurbo             = loadKey("turbo") getOrElse KEY_T
	val keyQuit              = loadKey("quit") getOrElse KEY_ESCAPE
	val keyPausePhysics      = loadKey("pause") getOrElse KEY_P
	val keyToggleGhostPlayer = loadKey("toggle_ghost_player") getOrElse KEY_TAB

	val keyDebugDraw      = loadKey("debug_draw") getOrElse KEY_F1
	val keyWireframe      = loadKey("wireframe") getOrElse KEY_F2
	val keyStreaming      = loadKey("streaming") getOrElse KEY_F3
	val keyFrustumCulling = loadKey("frustum_culling") getOrElse KEY_F4

	val keyScreenshot     = loadKey("screenshot") getOrElse KEY_F10
	val keyFullScreen     = loadKey("fullscreen") getOrElse KEY_F11

	// settings changeable at runtime:
	var debugDraw = false
	var wireframe = false
	var streamWorld = loadBoolean("streamWorld") getOrElse false
	var frustumCulling = true
	var turbo = false

	assert( worldWindowSize >= minMeshNodeSize )
	assert( worldWindowSize % minMeshNodeSize  == 0 )
	assert( Util.isPowerOfTwo(worldWindowSize / minMeshNodeSize) )
}

