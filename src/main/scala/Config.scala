package xöpäx

import simplex3d.math.float.Vec3
import simplex3d.math.doublex.Vec3d
import noise.Noise.{noise3, noise3_prediction}
import noise.intervals._

object Config{
	val minMeshNodeSize = 16
	val worldWindowSize = 64
	val useshaders = false
	val ungeneratedDefault = FullHexaeder // UndefHexaeder
	val startpos = Vec3(0)//Vec3(-10,34,164)
	val FPS_LIMIT = 6000
	val maxMeshVertexCount = 12345
	val numWorkingThreads = 4
	val patchAtNodeInsert = false
	
	val scale = 1/20.0f
	val densityfunction:(Vec3 => Float) = v => (noise3(Vec3d(v)*scale)/scale - v.z).toFloat //noise3(Vec3d(v)*scale).toFloat//gen.proceduralworld(v)._1.toFloat//
	val prediction:( (Vec3,Vec3) => Interval ) = (a,b) => {
		print(a,b)
		val u = a*scale
		val w = b*scale
		val i = noise3_prediction(u.x,u.y,u.z, w.x,w.y,w.z)/scale - Interval(a.z, b.z)
		println(i)
		i
	}
	
	val saveWorld = false
	
	assert( worldWindowSize >= minMeshNodeSize )
	assert( worldWindowSize % minMeshNodeSize  == 0 )
	assert( (worldWindowSize / minMeshNodeSize) % 2 == 0 )
	
	// changeable at runtime
	var debugDraw = false
	var streamWorld = true
}

