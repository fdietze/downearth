package xöpäx

import simplex3d.math.float.Vec3
import simplex3d.math.doublex.Vec3d
import noise.Noise.{Interval, noise3, noise3_prediction}

object Config{
	val minMeshNodeSize = 16
	val worldWindowSize = 256
	val useshaders = true
	val ungeneratedDefault = FullHexaeder
	val startpos = Vec3(0)//Vec3(-10,34,164)
	val FPS_LIMIT = 6000
	val maxMeshVertexCount = 12345
	
	val scale = 1/256.0
	val densityfunction:(Vec3 => Float) = v => noise3(Vec3d(v)*scale).toFloat
	val prediction:( (Vec3,Vec3) => Interval ) = (v,w) => noise3_prediction(v.x*scale,v.y*scale,v.z*scale,w.x*scale,w.y*scale,w.z*scale)
	val saveWorld = true
	
	assert( worldWindowSize >= minMeshNodeSize )
	assert( worldWindowSize % minMeshNodeSize  == 0 )
	assert( (worldWindowSize / minMeshNodeSize) % 2 == 0 )
	
	// changeable at runtime
	var debugDraw = false
	var streamWorld = false
}

