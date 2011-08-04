package xöpäx

import simplex3d.math.float.Vec3

object Config{
	val minMeshNodeSize = 32
	val worldWindowSize = 64
	val useshaders = true
	val ungeneratedDefault = FullHexaeder
	val startpos = Vec3(0)//Vec3(-10,34,164)
	val FPS_LIMIT = 6000
	
	val densityfunction:(Vec3 => Float) = v => gen.proceduralworld(v)._1.toFloat
	
	assert(worldWindowSize >= minMeshNodeSize)
	assert(worldWindowSize % minMeshNodeSize  == 0)
	assert( (worldWindowSize / minMeshNodeSize) % 2 == 0 )
	
	// changeable at runtime
	var debugDraw = false
}

