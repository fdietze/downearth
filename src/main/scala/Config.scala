package xöpäx

object Config{
	val minMeshNodeSize = 32
	val worldWindowSize = 64
	
	assert(worldWindowSize >= minMeshNodeSize)
	assert(worldWindowSize % minMeshNodeSize  == 0)
	assert( (worldWindowSize / minMeshNodeSize) % 2 == 0 )
}
