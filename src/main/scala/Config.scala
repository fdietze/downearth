package xöpäx

import simplex3d.math.float.Vec3

object Config{
	val minMeshNodeSize = 32
	val worldWindowSize = 64
	val useshaders = false
	val ungeneratedDefault = FullHexaeder
	val startpos = Vec3(-10,34,164)
	val FPS_LIMIT = 60
	
	assert(worldWindowSize >= minMeshNodeSize)
	assert(worldWindowSize % minMeshNodeSize  == 0)
	assert( (worldWindowSize / minMeshNodeSize) % 2 == 0 )
}

