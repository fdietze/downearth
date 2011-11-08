package noise

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


object Worley {

	def intfloor(x:Double) = x.floor.toInt
	def coordhash(X:Int,Y:Int,Z:Int) = (702395077*X + 915488749*Y + 2120969693*Z) & 0x7FFFFFFF
	def nextrandom(last:Int) = (1402024253*last + 586950981) & 0x7FFFFFFF
	def distance(x0:Double,y0:Double,z0:Double,x1:Double,y1:Double,z1:Double) = (x0-x1)*(x0-x1) + (y0-y1)*(y0-y1) + (z0-z1)*(z0-z1)
	def randomfloat(rand:Int) = rand / 2147483647.0 // maxRand


	val pointspercube = Array(
		4, 3, 1, 1, 1, 2, 4, 2, 2, 2, 5, 1, 0, 2, 1, 2, 2, 0, 4, 3, 2, 1, 2, 
		1, 3, 2, 2, 4, 2, 2, 5, 1, 2, 3, 2, 2, 2, 2, 2, 3, 2, 4, 2, 5, 3, 2, 
		2, 2, 5, 3, 3, 5, 2, 1, 3, 3, 4, 4, 2, 3, 0, 4, 2, 2, 2, 1, 3, 2, 2, 
		2, 3, 3, 3, 1, 2, 0, 2, 1, 1, 2, 2, 2, 2, 5, 3, 2, 3, 2, 3, 2, 2, 1, 
		0, 2, 1, 1, 2, 1, 2, 2, 1, 3, 4, 2, 2, 2, 5, 4, 2, 4, 2, 2, 5, 4, 3, 
		2, 2, 5, 4, 3, 3, 3, 5, 2, 2, 2, 2, 2, 3, 1, 1, 5, 2, 1, 3, 3, 4, 3, 
		2, 4, 3, 3, 3, 4, 5, 1, 4, 2, 4, 3, 1, 2, 3, 5, 3, 2, 1, 3, 1, 3, 3, 
		3, 2, 3, 1, 5, 5, 4, 2, 2, 4, 1, 3, 4, 1, 5, 3, 3, 5, 3, 4, 3, 2, 2, 
		1, 1, 1, 1, 1, 2, 4, 5, 4, 5, 4, 2, 1, 5, 1, 1, 2, 3, 3, 3, 2, 5, 2, 
		3, 3, 2, 0, 2, 1, 1, 4, 2, 1, 3, 2, 1, 2, 2, 3, 2, 5, 5, 3, 4, 5, 5, 
		2, 4, 4, 5, 3, 2, 2, 2, 1, 4, 2, 3, 3, 4, 2, 5, 4, 2, 4, 2, 2, 2, 4, 
		5, 3, 2 )

	def cellnoise(v:Vec3):Vec4 = cellnoise(v.x, v.y, v.z)
	
	def cellnoise(x:Double,y:Double,z:Double):Vec4 = {
		val X = intfloor(x)
		val Y = intfloor(y)
		val Z = intfloor(z)

		val relx = x - X
		val rely = y - Y
		val relz = z - Z
		
		val mindistances = Vec4( scala.Double.MaxValue )
		def maxdistance  = mindistances.w
		
		// cube face distances
		val dstx = relx*relx
		val dsty = rely*rely
		val dstz = relz*relz
		val dstmx = (1.0-relx)*(1.0-relx)
		val dstmy = (1.0-rely)*(1.0-rely)
		val dstmz = (1.0-relz)*(1.0-relz)
		
		testcube(0,0,0)
		
		// test 6 facing neighbours
		if(  dstx < maxdistance ) testcube(-1, 0, 0)
		if(  dsty < maxdistance ) testcube( 0,-1, 0)
		if(  dstz < maxdistance ) testcube( 0, 0,-1)
		if( dstmx < maxdistance ) testcube( 1, 0, 0)
		if( dstmy < maxdistance ) testcube( 0, 1, 0)
		if( dstmz < maxdistance ) testcube( 0, 0, 1)
		
		// test 12 edge cubes
		if( dstx +  dsty < maxdistance) testcube(-1,-1, 0)
		if( dstx +  dstz < maxdistance) testcube(-1, 0,-1)
		if( dsty +  dstz < maxdistance) testcube( 0,-1,-1)
		if(dstmx + dstmy < maxdistance) testcube( 1, 1, 0)
		if(dstmx + dstmz < maxdistance) testcube( 1, 0, 1)
		if(dstmy + dstmz < maxdistance) testcube( 0, 1, 1)
		if( dstx + dstmy < maxdistance) testcube(-1, 1, 0)
		if( dstx + dstmz < maxdistance) testcube(-1, 0, 1)
		if( dsty + dstmz < maxdistance) testcube( 0,-1, 1)
		if(dstmx +  dsty < maxdistance) testcube( 1,-1, 0)
		if(dstmx +  dstz < maxdistance) testcube( 1, 0,-1)
		if(dstmy +  dstz < maxdistance) testcube( 0, 1,-1)
		
		// test 8 corner cubes
		if( dstx +  dsty +  dstz < maxdistance) testcube(-1,-1,-1)
		if( dstx +  dsty + dstmz < maxdistance) testcube(-1,-1, 1)
		if( dstx + dstmy +  dstz < maxdistance) testcube(-1, 1,-1)
		if( dstx + dstmy + dstmz < maxdistance) testcube(-1, 1, 1)
		if(dstmx +  dsty +  dstz < maxdistance) testcube( 1,-1,-1)
		if(dstmx +  dsty + dstmz < maxdistance) testcube( 1,-1, 1)
		if(dstmx + dstmy +  dstz < maxdistance) testcube( 1, 1,-1)
		if(dstmx + dstmy + dstmz < maxdistance) testcube( 1, 1, 1)



		def testcube(dx:Int, dy:Int, dz:Int) {
			val cubex = X + dx
			val cubey = Y + dy
			val cubez = Z + dz
	 		
			var rng = coordhash(cubex,cubey,cubez)
			var p = 0
			val pointcount = pointspercube(rng & 0xFF)
			while( p < pointcount ) {
				rng = nextrandom(rng)
				val pointx = cubex + randomfloat(rng)
				rng = nextrandom(rng)
				val pointy = cubey + randomfloat(rng)
				rng = nextrandom(rng)
				val pointz = cubez + randomfloat(rng)

				val dst = distance(x, y, z, pointx, pointy, pointz)
				
				// insertion sort
				if( dst < mindistances.w ) {
					if( dst < mindistances.z ) {
						mindistances.w = mindistances.z
						if( dst < mindistances.y ) {
							mindistances.z = mindistances.y
							if( dst < mindistances.x ) {
								mindistances.y = mindistances.x
								mindistances.x = dst
							}
							else
								mindistances.y = dst
						}
						else
							mindistances.z = dst
					} 
					else
						mindistances.w = dst
				}
				
				p += 1
			}
		}

		return mindistances
	}
}
