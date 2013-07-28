package downearth

import simplex3d.math.double.Vec3
import org.lwjgl.opengl.Display
import downearth.worldoctree.{FullLeaf, FullHexaeder, Leaf}
import gui.Listener
import util.isPowerOfTwo
import downearth.rendering.TextureMesh

object Config extends Listener {
  val maxMeshCount = 50

  val uCorrectChromaticAberation = true
  val uDistort = false

  val loader = new ConfigLoader(this)

  import loader.preferences.{
    getInt => loadInt
  }

	var minPredictionSize = loadInt("minPredictionSize", 8)
  var playerRadius = loadInt("worldWindowSize", 10)
  var playerSightRadius = 150.0 //TODO: set farplane to playerSightRadius

	val vertexMaterials = true
	val hexaederResolution = 8
	val skybox =false

  var occlusionTestMagicNumber = 4.0
  var a = 1.0
  var b = 1.0
  var c = 1.0
	
	lazy val ungeneratedDefault = FullLeaf
	val startpos = Vec3(0,0,5) // TODO: Fix streaming with other start position
	
	var fpsLimitd = 60.0
  def fpsLimit = fpsLimitd.toInt
  def timeStep = 1.0 / fpsLimit

  var test = 0.29
	
	// um den Meshjoin/-split Vorgang zu testen sollte dieser wert niedriger 
	// gesetzt werden (10000)
	val maxMeshByteSize = 30000 * TextureMesh.byteStride
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
  val keyGenerateNextUngenerated = KEY_N
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
  var generation = true
  var streamWorld = false
  var frustumCulling = true
  var turbo = false
  var occlusionTest = true
  var visibleOcclusionTest = true
  var prioritizeGenerationInFrustum = true

  loader.load()

  assert( isPowerOfTwo(minPredictionSize) )
}

