package downearth

import simplex3d.math.double.Vec3
import org.lwjgl.opengl.Display
import downearth.worldoctree.{FullLeaf, FullHexaeder, Leaf}
import gui.Listener
import util.isPowerOfTwo

object Config extends Listener {
  val uCorrectChromaticAberation = true
  val uDistort = false

  val loader = new ConfigLoader(this)

  import loader.preferences.{
    getInt => loadInt
  }

	var minMeshNodeSize = loadInt("minMeshNodeSize", 16)
	var minPredictionSize = loadInt("minPredictionSize", 4)
	var worldWindowSize = loadInt("worldWindowSize", 64)

	val vertexMaterials = true
	val hexaederResolution = 8
	val skybox =false

  var a = 15.0
  var b = 3.5
  var c = 0.15
	
	lazy val ungeneratedDefault = FullLeaf
	val startpos = Vec3(0,0,5) // TODO: Fix streaming with other start position
	
	val fpsLimit = loadInt("fpsLimit", 60)
  def timeStep = 1.0 / fpsLimit

  var test = 0.29
	
	// um den Meshjoin/-split Vorgang zu testen sollte dieser wert niedriger 
	// gesetzt werden (10000)
	val maxMeshVertexCount = 30000
  val maxDebugDrawQubes = 200
	val numWorkingThreads = Runtime.getRuntime.availableProcessors
	val cameraSpeed = 2
	val cameraTurboSpeed = 32
	val startAsGhost = false
	val buildrange = 100
	var fullscreen = false
  var stereoRender = false
  var anaglyph = false

	// Vollbild-Modus mit höchster Auflösung
	def fullscreenDisplayMode = Display.getDesktopDisplayMode
	
	val windowResolutionWidth  = loadInt("windowResolutionWidth", 1280)
	val windowResolutionHeight = loadInt("windowResolutionHeight", 800)

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
  val DebugDrawOctreeBit = 0x01
  val DebugDrawPhysicsBit = 0x02
  val DebugDrawSampledNodesBit = 0x04

  var debugDraw = 0
  var backFaceCulling = true
  var wireframe = false
  var streamWorld = false
  var frustumCulling = true
  var turbo = false
  var occlusionTest = true
  var visibleOcclusionTest = true
  var prioritizeGenerationInFrustum = true

  loader.load()

	assert( worldWindowSize >= minMeshNodeSize )
	assert( worldWindowSize % minMeshNodeSize  == 0 )
	assert( isPowerOfTwo(worldWindowSize / minMeshNodeSize) )
}

