package downearth

import simplex3d.math.double.Vec3
import org.lwjgl.opengl.Display
import downearth.worldoctree.{FullLeaf, FullHexaeder, Leaf}
import gui.Listener
import util.isPowerOfTwo
import downearth.rendering.TextureMesh

object Config extends Listener {

  val uCorrectChromaticAberation = true
  val uDistort = false

  val loader = new ConfigLoader(this)

  import loader.preferences.{
    getInt => loadInt
  }

	var minPredictionSized = 8.0
  def minPredictionSize = minPredictionSized.toInt
  var minOcclusionSized = 32.0
  def minOcclusionSize = minOcclusionSized.toInt
  var playerRadius = 10.0
  var generationRadius = 200.0
  var farPlane = generationRadius * 2.0
  var nearPlane = 0.05
  var occlusionTestPixelThreshold = 10.0
  var maxOcclusionQueries = 10.0
  var occlusionTestMaxGeneratingNodesPerFrame = 20


  val vertexMaterials = true
	val hexaederResolution = 8
	val skybox =false

  var a = 1.0
  var b = 1.0
  var c = 1.0
	
	lazy val ungeneratedDefault = FullLeaf
	val startPos = Vec3(0,0,5)
	
	var fpsLimitd = 60.0
  def fpsLimit = fpsLimitd.toInt
  def timeStep = 1.0 / fpsLimit

  var test = 0.29
	
	// um den Meshjoin/-split Vorgang zu testen sollte dieser wert niedriger 
	// gesetzt werden (10000)
	val maxMeshByteSize = 30000 * TextureMesh.byteStride
  val maxDebugDrawCubes = 1000
	val numWorkingThreads = Runtime.getRuntime.availableProcessors
	val cameraSpeed = 2
	val cameraTurboSpeed = 32
	val startAsGhost = false
	val buildrange = 100
	var fullscreen = false
  var stereoRender = false
  var anaglyph = false
  var lwjglDebug = true

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
  var physics = true
  var predictionDebug = false
  var backFaceCulling = true
  var wireframe = false
  var frustumCulling = true
  var frustumCullingOptimized = false
  var turbo = false

  var streaming = true
  var testUngenerated = true
  var occlusionTest = true
  var adaptingOcclusionTestSphere = false
  var visibleOcclusionTest = false
  var generation = true
  var freeOldMeshes = true
  val maxMeshCount = 50

  loader.load()
}

