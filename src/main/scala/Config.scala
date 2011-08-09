package xöpäx

import simplex3d.math.float.Vec3
import simplex3d.math.doublex.Vec3d
import noise.Noise.{noise3, noise3_prediction}
import noise.intervals._

object Config{
	val minMeshNodeSize = 32
	val worldWindowSize = 256
	val useshaders = true
	val ungeneratedDefault = FullHexaeder // UndefHexaeder
	val startpos = Vec3(0)//Vec3(-10,34,164)
	val FPS_LIMIT = 6000
	val maxMeshVertexCount = 12345
	val numWorkingThreads = 8
	val patchAtNodeInsert = false
	val CameraSpeed = 4f
	val CameraTurboSpeed = 32f
	
	val scale = 1/20.0f
	val densityfunction:(Vec3 => Float) = v => gen.density(v).toFloat
	//TODO: Statt Tupel ein Volume übergeben
	val prediction:( (Vec3,Vec3) => Interval ) = { case (v1,v2) => gen.prediction(Volume(v1,v2)) }
	
	val saveWorld = false
	
	assert( worldWindowSize >= minMeshNodeSize )
	assert( worldWindowSize % minMeshNodeSize  == 0 )
	assert( (worldWindowSize / minMeshNodeSize) % 2 == 0 )
	
	// changeable at runtime
	var debugDraw = false
	var streamWorld = true
	
}

