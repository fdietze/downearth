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
	val smoothShading = false
	val hexaederResolution = 8
	
	val skybox = true
	val fullscreen = true
	
	val ungeneratedDefault = UndefHexaeder
	val startpos = Vec3(0)
	val fpsLimit = 60

	val maxMeshVertexCount = 30000
	val numWorkingThreads = Runtime.getRuntime.availableProcessors
	val patchAtNodeInsert = false
	val CameraSpeed = 2f
	val CameraTurboSpeed = 32f
	
	def densityfunction(v:Vec3) = gen.density(v).toFloat
	def prediction(v1: Vec3, v2: Vec3) = gen.prediction(Volume(v1,v2))
	val saveWorld = false

	assert( worldWindowSize >= minMeshNodeSize )
	assert( worldWindowSize % minMeshNodeSize  == 0 )
	assert( (worldWindowSize / minMeshNodeSize) % 2 == 0 )


	val displayMode =
		if(fullscreen) // Vollbild-Modus mit höchster Auflösung
			Display.getAvailableDisplayModes.maxBy( _.getWidth )
		else
			new DisplayMode(1024, 768)
	
	def screenWidth  = displayMode.getWidth
	def screenHeight = displayMode.getHeight

	val worldUpVector = Vec3.UnitZ


	// settings changeable at runtime:
	var debugDraw = false
	var streamWorld = false
	var turbo = false
	var frustumCulling = true
}

