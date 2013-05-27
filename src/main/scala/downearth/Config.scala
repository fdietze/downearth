package downearth

import simplex3d.math.double.Vec3
import org.lwjgl.opengl.Display
import downearth.worldoctree.{FullHexaeder, Leaf}
import gui.Listener
import util.isPowerOfTwo

object Config extends Listener {
	val loader = new ConfigLoader(this)

  import loader.preferences.{
    getInt => loadInt,
    getBoolean => loadBool
  }

	var minMeshNodeSize = loader.preferences.getInt("minMeshNodeSize", 16)
	var minPredictionSize = loadInt("minPredictionSize", 4)
	var kdTreePrediction = loadBool("kdTreePrediction", true)
	
	var worldWindowSize = loadInt("worldWindowSize", 64)
	val useShaders = loadBool("useShaders", false)
	val vertexMaterials = true
	val smoothShading = loadBool("smoothShading", false)
	
	val hexaederResolution = 8
	
	val skybox = loadBool("skybox", false)
	
	lazy val ungeneratedDefault = Leaf(FullHexaeder,-1)
	val startpos = Vec3(0,0,5) // TODO: Fix streaming with other start position
	
	val fpsLimit = loadInt("fpsLimit", 60)
	
	// um den Meshjoin/-split Vorgang zu testen sollte dieser wert niedriger 
	// gesetzt werden (10000)
	val maxMeshVertexCount = 30000
  val maxDebugDrawQubes = 200
	val numWorkingThreads = Runtime.getRuntime.availableProcessors
	val cameraSpeed = 2
	val cameraTurboSpeed = 32
	val startAsGhost = false
	val buildrange = 100
	
	val saveWorld = false

	var fullscreen = loadBool("fullscreen", false)

	// Vollbild-Modus mit höchster Auflösung
	def fullscreenDisplayMode = Display.getDesktopDisplayMode
	
	val windowResolutionWidth  = loadInt("windowResolutionWidth", 640)
	val windowResolutionHeight = loadInt("windowResolutionHeight", 480)

	val worldUpVector = Vec3.UnitZ
	
	import org.lwjgl.input.Keyboard._

  val keyForward  = KEY_W
  val keyBackward = KEY_S
  val keyLeft     = KEY_A
  val keyRight    = KEY_D
  val keyJump     = KEY_SPACE
	
	val keyMouseGrab         = KEY_G
	val keyPlayerReset       = KEY_R
	val keyTurbo             = KEY_T
	val keyQuit              = KEY_ESCAPE
	val keyPausePhysics      = KEY_P
	val keyToggleGhostPlayer = KEY_TAB
  val keyIncOctreeDepth    = KEY_X
  val keyToggleInventory   = KEY_Q

	val keyDebugDraw      = KEY_F1
	val keyWireframe      = KEY_F2
	val keyStreaming      = KEY_F3
	val keyFrustumCulling = KEY_F4

	val keyScreenshot       = KEY_F10
	val keyToggleFullScreen = KEY_F11

	// settings changeable at runtime:
	var debugDraw = false
	var wireframe = false
	var streamWorld = false
	var frustumCulling = true
	var turbo = false

  loader.load()

	assert( worldWindowSize >= minMeshNodeSize )
	assert( worldWindowSize % minMeshNodeSize  == 0 )
	assert( isPowerOfTwo(worldWindowSize / minMeshNodeSize) )
}

