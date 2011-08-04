package xöpäx

object Config{
	val minMeshNodeSize = 32
	val worldWindowSize = 128
	val useshaders = true
	val ungeneratedDefault = FullHexaeder
	
	assert(worldWindowSize >= minMeshNodeSize)
	assert(worldWindowSize % minMeshNodeSize  == 0)
	assert( (worldWindowSize / minMeshNodeSize) % 2 == 0 )
}
