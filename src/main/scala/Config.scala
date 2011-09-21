package xöpäx

import simplex3d.math.float.Vec3
import simplex3d.math.doublex.Vec3d
import noise.Noise.{noise3, noise3_prediction}
import noise.interval.{Interval, Volume}

object Config{

	val minMeshNodeSize = 16
	val worldWindowSize = 128
	val useshaders = false
	val smoothShading = false

	val skybox = true //TODO: on false, disable loading of texture
	val fullscreen = true
	
	val ungeneratedDefault = UndefHexaeder
	val startpos = Vec3(0)//Vec3(-10,34,164)
	val FPS_LIMIT = 60

	val maxMeshVertexCount = 30000
	val numWorkingThreads = Runtime.getRuntime.availableProcessors
	val patchAtNodeInsert = false
	val CameraSpeed = 4f
	val CameraTurboSpeed = 32f
	
	val densityfunction:(Vec3 => Float) = v => gen.density(v).toFloat
	val prediction:( (Vec3,Vec3) => Interval ) = { case (v1,v2) => gen.prediction(Volume(v1,v2)) }	
	val saveWorld = false
	
	assert( worldWindowSize >= minMeshNodeSize )
	assert( worldWindowSize % minMeshNodeSize  == 0 )
	assert( (worldWindowSize / minMeshNodeSize) % 2 == 0 )
	
	// changeable at runtime
	var debugDraw = false
	var streamWorld = false
	
}

