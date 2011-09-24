package xöpäx

import simplex3d.math.float.Vec3
import simplex3d.math.doublex.Vec3d
import noise.Noise.{noise3, noise3_prediction}
import noise.interval.{Interval, Volume}

object Config{

	val minMeshNodeSize = 16
	val worldWindowSize = 128
	val useshaders = true
	val smoothShading = false

	val skybox = true //TODO: on false, disable loading of texture (check with low-end gpu!)
	val fullscreen = false //TODO: fix fullscreen
	
	val ungeneratedDefault = UndefHexaeder
	val startpos = Vec3(0)
	val FPS_LIMIT = 60

	val maxMeshVertexCount = 30000
	val numWorkingThreads = Runtime.getRuntime.availableProcessors
	val patchAtNodeInsert = false
	val CameraSpeed = 4f
	val CameraTurboSpeed = 32f
	
	def densityfunction(v:Vec3) = gen.density(v).toFloat
	def prediction(v1: Vec3, v2: Vec3) = gen.prediction(Volume(v1,v2))
	val saveWorld = false
	
	assert( worldWindowSize >= minMeshNodeSize )
	assert( worldWindowSize % minMeshNodeSize  == 0 )
	assert( (worldWindowSize / minMeshNodeSize) % 2 == 0 )
	
	val screenWidth = 1024
	val screenHeight = screenWidth*3/4
	val worldUpVector = Vec3.UnitZ


	// settings changeable at runtime:
	var debugDraw = false
	var streamWorld = false
}

