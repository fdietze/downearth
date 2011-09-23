package noise

import interval.{Interval, Volume}

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


object Noise {
	def fastfloor(x:Double) = (if(x > 0) x else (x-1)).toInt
	def fastceil(x:Double) = x.ceil.toInt
	def fade(t:Double) = t * t * t * (t * (t * 6 - 15) + 10)
	def lerp(t:Double, a:Double, b:Double) = a + t * (b - a)

	// Hash
	/*var seed = 0
	val a = (seed ^ 0xB5C18E6A) | ((1 << 16) + 1)
	val c = seed ^ 0xF292D0B2
	def hash(x: Int) :Int = (a*(x ^ c)) >>> 16*/
	//def hash(k:Int) = ((k*0x12345678) >> (k*0x87754351)) & 0x7FFFFFFF
	def hash(k:Int) = mod(((k*34)+1)*k, 289).toInt



	// Split Bezier Curves
	def splitleft(h:Array[Double],t:Double):Array[Double] = {
		val n = h.size - 1
		val result = h.clone
		var tmp = 0.0
		var save = 0.0
		for( i <- 0 to n-1 ) {
			tmp = result(i)
			for( j <- i+1 to n ) {
				save = lerp(t, tmp, result(j))
				tmp = result(j)
				result(j) = save
			}
		}
		result
	}

	def splitright(h:Array[Double],t1:Double):Array[Double] = {
		import collection.immutable.Range.inclusive
		val t = 1-t1
		val n = h.size - 1
		val result = h.clone
		var tmp = 0.0
		var save = 0.0
		for( i <- 0 to n-1 ) {
			tmp = result(n-i)
			for( j <- inclusive(n-i-1,0,-1) ) {
				save = lerp(t, tmp, result(j))
				tmp = result(j)
				result(j) = save
			}
		}
		result
	}
	
	def slice(h:Array[Double], t0:Double, t1:Double) = splitright(splitleft(h,t1),t0/t1)
	
	val gradients3 = Array(
		Vec3( 1, 1, 0),
		Vec3(-1, 1, 0),
		Vec3( 1,-1, 0),
		Vec3(-1,-1, 0),

		Vec3( 1, 0, 1),
		Vec3(-1, 0, 1),
		Vec3( 1, 0,-1),
		Vec3(-1, 0,-1),

		Vec3( 0, 1, 1),
		Vec3( 0,-1, 1),
		Vec3( 0, 1,-1),
		Vec3( 0,-1,-1),

		Vec3( 1, 1, 0),
		Vec3( 0,-1, 1),
		Vec3(-1, 1, 0),
		Vec3( 0,-1,-1)
	)

	def gradientat3(X:Int, Y:Int, Z:Int) = {
		gradients3(hash(hash(hash(X)+Y)+Z) & 15)
	}
	
	def noise3_prediction(v:Volume):Interval = {
		import v.x.{low => x0, high => x1}
		import v.y.{low => y0, high => y1}
		import v.z.{low => z0, high => z1}
		
		// Edges of the unit cube
		val X = fastfloor(x0)
		val Y = fastfloor(y0)
		val Z = fastfloor(z0)
		
		// Interval needs to stay inside one unit cube of the lattice
		if( X < fastceil(x1)-1
		 || Y < fastceil(y1)-1
		 || Z < fastceil(z1)-1 )
			return Interval(-1,1) // no recursion here, because the octree doesn't need more precision
		
		// relative positions in unit cube
		val relx0 = x0 - X
		val rely0 = y0 - Y
		val relz0 = z0 - Z
		val relx1 = x1 - X
		val rely1 = y1 - Y
		val relz1 = z1 - Z
		
		// Get the Pseudorandom Gradients for each Lattice point
		val Vec3(g0x,g0y,g0z) = gradientat3(X  ,Y  ,Z  )
		val Vec3(g1x,g1y,g1z) = gradientat3(X+1,Y  ,Z  )
		val Vec3(g2x,g2y,g2z) = gradientat3(X  ,Y+1,Z  )
		val Vec3(g3x,g3y,g3z) = gradientat3(X+1,Y+1,Z  )
		val Vec3(g4x,g4y,g4z) = gradientat3(X  ,Y  ,Z+1)
		val Vec3(g5x,g5y,g5z) = gradientat3(X+1,Y  ,Z+1)
		val Vec3(g6x,g6y,g6z) = gradientat3(X  ,Y+1,Z+1)
		val Vec3(g7x,g7y,g7z) = gradientat3(X+1,Y+1,Z+1)
	
		// Calculate the heights of the bezier curve, converted from the 3d perlin noise polynomial with fade-function of degree 5
		// resulting polynomial has degree 6. This gives 7^3 Bezier points
		val bezierheights = 
Array(Array(Array(0,g0z/6,g0z/3,-(g4z-g0z)/2,-g4z/3,-g4z/6,0),Array(g0y/6,(g0z+g0y)/6,(2*g0z+g0y)/6,-(6*g4z-g4y-6*g0z-g0y)/12,-(2*g4z-g4y)/6,-(g4z-g4y)/6,g4y/6),Array(g0y/3,
(g0z+2*g0y)/6,(g0z+g0y)/3,-(3*g4z-g4y-3*g0z-g0y)/6,-(g4z-g4y)/3,-(g4z-2*g4y)/6,g4y/3),Array(-(g2y-g0y)/2,(g2z-6*g2y+g0z+6*g0y)/12,(g2z-3*g2y+g0z+3*g0y)/6,-
(g6z+g6y+g4z-g4y-g2z+g2y-g0z-g0y)/4,-(g6z+3*g6y+g4z-3*g4y)/6,-(g6z+6*g6y+g4z-6*g4y)/12,-(g6y-g4y)/2),Array(-g2y/3,(g2z-2*g2y)/6,(g2z-g2y)/3,-
(3*g6z+g6y-3*g2z+g2y)/6,-(g6z+g6y)/3,-(g6z+2*g6y)/6,-g6y/3),Array(-g2y/6,(g2z-g2y)/6,(2*g2z-g2y)/6,-(6*g6z+g6y-6*g2z+g2y)/12,-(2*g6z+g6y)/6,-(g6z+g6y)/6,-g6y/6),Array(0,g2z/6
,g2z/3,-(g6z-g2z)/2,-g6z/3,-g6z/6,0)),Array(Array(g0x/6,(g0z+g0x)/6,(2*g0z+g0x)/6,-(6*g4z-g4x-6*g0z-g0x)/12,-(2*g4z-g4x)/6,-(g4z-g4x)/6,g4x/6),Array((g0y+g0x)/6,(g0z+g0y+g0x)/6,
(2*g0z+g0y+g0x)/6,-(6*g4z-g4y-g4x-6*g0z-g0y-g0x)/12,-(2*g4z-g4y-g4x)/6,-(g4z-g4y-g4x)/6,(g4y+g4x)/6),Array((2*g0y+g0x)/6,(g0z+2*g0y+g0x)/6,(2*g0z+2*g0y+g0x)/6,-
(6*g4z-2*g4y-g4x-6*g0z-2*g0y-g0x)/12,-(2*g4z-2*g4y-g4x)/6,-(g4z-2*g4y-g4x)/6,(2*g4y+g4x)/6),Array(-(6*g2y-g2x-6*g0y-g0x)/12,(g2z-6*g2y+g2x+g0z+6*g0y+g0x)/12,
(2*g2z-6*g2y+g2x+2*g0z+6*g0y+g0x)/12,-(6*g6z+6*g6y-g6x+6*g4z-6*g4y-g4x-6*g2z+6*g2y-g2x-6*g0z-6*g0y-g0x)/24,-(2*g6z+6*g6y-g6x+2*g4z-6*g4y-g4x)/12,-
(g6z+6*g6y-g6x+g4z-6*g4y-g4x)/12,-(6*g6y-g6x-6*g4y-g4x)/12),Array(-(2*g2y-g2x)/6,(g2z-2*g2y+g2x)/6,(2*g2z-2*g2y+g2x)/6,-(6*g6z+2*g6y-g6x-6*g2z+2*g2y-g2x)/12,-
(2*g6z+2*g6y-g6x)/6,-(g6z+2*g6y-g6x)/6,-(2*g6y-g6x)/6),Array(-(g2y-g2x)/6,(g2z-g2y+g2x)/6,(2*g2z-g2y+g2x)/6,-(6*g6z+g6y-g6x-6*g2z+g2y-g2x)/12,-(2*g6z+g6y-g6x)/6,-
(g6z+g6y-g6x)/6,-(g6y-g6x)/6),Array(g2x/6,(g2z+g2x)/6,(2*g2z+g2x)/6,-(6*g6z-g6x-6*g2z-g2x)/12,-(2*g6z-g6x)/6,-(g6z-g6x)/6,g6x/6)),Array(Array(g0x/3,(g0z+2*g0x)/6,(g0z+g0x)/3,-
(3*g4z-g4x-3*g0z-g0x)/6,-(g4z-g4x)/3,-(g4z-2*g4x)/6,g4x/3),Array((g0y+2*g0x)/6,(g0z+g0y+2*g0x)/6,(2*g0z+g0y+2*g0x)/6,-(6*g4z-g4y-2*g4x-6*g0z-g0y-2*g0x)/12,-
(2*g4z-g4y-2*g4x)/6,-(g4z-g4y-2*g4x)/6,(g4y+2*g4x)/6),Array((g0y+g0x)/3,(g0z+2*g0y+2*g0x)/6,(g0z+g0y+g0x)/3,-(3*g4z-g4y-g4x-3*g0z-g0y-g0x)/6,-(g4z-g4y-g4x)/3,-
(g4z-2*g4y-2*g4x)/6,(g4y+g4x)/3),Array(-(3*g2y-g2x-3*g0y-g0x)/6,(g2z-6*g2y+2*g2x+g0z+6*g0y+2*g0x)/12,(g2z-3*g2y+g2x+g0z+3*g0y+g0x)/6,-
(3*g6z+3*g6y-g6x+3*g4z-3*g4y-g4x-3*g2z+3*g2y-g2x-3*g0z-3*g0y-g0x)/12,-(g6z+3*g6y-g6x+g4z-3*g4y-g4x)/6,-(g6z+6*g6y-2*g6x+g4z-6*g4y-2*g4x)/12,-
(3*g6y-g6x-3*g4y-g4x)/6),Array(-(g2y-g2x)/3,(g2z-2*g2y+2*g2x)/6,(g2z-g2y+g2x)/3,-(3*g6z+g6y-g6x-3*g2z+g2y-g2x)/6,-(g6z+g6y-g6x)/3,-(g6z+2*g6y-2*g6x)/6,-(g6y-g6x)/3)
,Array(-(g2y-2*g2x)/6,(g2z-g2y+2*g2x)/6,(2*g2z-g2y+2*g2x)/6,-(6*g6z+g6y-2*g6x-6*g2z+g2y-2*g2x)/12,-(2*g6z+g6y-2*g6x)/6,-(g6z+g6y-2*g6x)/6,-(g6y-2*g6x)/6),Array(g2x/3,
(g2z+2*g2x)/6,(g2z+g2x)/3,-(3*g6z-g6x-3*g2z-g2x)/6,-(g6z-g6x)/3,-(g6z-2*g6x)/6,g6x/3)),Array(Array(-(g1x-g0x)/2,(g1z-6*g1x+g0z+6*g0x)/12,(g1z-3*g1x+g0z+3*g0x)/6,-
(g5z+g5x+g4z-g4x-g1z+g1x-g0z-g0x)/4,-(g5z+3*g5x+g4z-3*g4x)/6,-(g5z+6*g5x+g4z-6*g4x)/12,-(g5x-g4x)/2),Array((g1y-6*g1x+g0y+6*g0x)/12,
(g1z+g1y-6*g1x+g0z+g0y+6*g0x)/12,(2*g1z+g1y-6*g1x+2*g0z+g0y+6*g0x)/12,-(6*g5z-g5y+6*g5x+6*g4z-g4y-6*g4x-6*g1z-g1y+6*g1x-6*g0z-g0y-6*g0x)/24,-
(2*g5z-g5y+6*g5x+2*g4z-g4y-6*g4x)/12,-(g5z-g5y+6*g5x+g4z-g4y-6*g4x)/12,(g5y-6*g5x+g4y+6*g4x)/12),Array((g1y-3*g1x+g0y+3*g0x)/6,
(g1z+2*g1y-6*g1x+g0z+2*g0y+6*g0x)/12,(g1z+g1y-3*g1x+g0z+g0y+3*g0x)/6,-(3*g5z-g5y+3*g5x+3*g4z-g4y-3*g4x-3*g1z-g1y+3*g1x-3*g0z-g0y-3*g0x)/12,-
(g5z-g5y+3*g5x+g4z-g4y-3*g4x)/6,-(g5z-2*g5y+6*g5x+g4z-2*g4y-6*g4x)/12,(g5y-3*g5x+g4y+3*g4x)/6),Array(-(g3y+g3x+g2y-g2x-g1y+g1x-g0y-g0x)/4,
(g3z-6*g3y-6*g3x+g2z-6*g2y+6*g2x+g1z+6*g1y-6*g1x+g0z+6*g0y+6*g0x)/24,(g3z-3*g3y-3*g3x+g2z-3*g2y+3*g2x+g1z+3*g1y-3*g1x+g0z+3*g0y+3*g0x)/12,-
(g7z+g7y+g7x+g6z+g6y-g6x+g5z-g5y+g5x+g4z-g4y-g4x-g3z+g3y+g3x-g2z+g2y-g2x-g1z-g1y+g1x-g0z-g0y-g0x)/8,-
(g7z+3*g7y+3*g7x+g6z+3*g6y-3*g6x+g5z-3*g5y+3*g5x+g4z-3*g4y-3*g4x)/12,-(g7z+6*g7y+6*g7x+g6z+6*g6y-6*g6x+g5z-6*g5y+6*g5x+g4z-6*g4y-6*g4x)/24,-
(g7y+g7x+g6y-g6x-g5y+g5x-g4y-g4x)/4),Array(-(g3y+3*g3x+g2y-3*g2x)/6,(g3z-2*g3y-6*g3x+g2z-2*g2y+6*g2x)/12,(g3z-g3y-3*g3x+g2z-g2y+3*g2x)/6,-
(3*g7z+g7y+3*g7x+3*g6z+g6y-3*g6x-3*g3z+g3y+3*g3x-3*g2z+g2y-3*g2x)/12,-(g7z+g7y+3*g7x+g6z+g6y-3*g6x)/6,-(g7z+2*g7y+6*g7x+g6z+2*g6y-6*g6x)/12,-
(g7y+3*g7x+g6y-3*g6x)/6),Array(-(g3y+6*g3x+g2y-6*g2x)/12,(g3z-g3y-6*g3x+g2z-g2y+6*g2x)/12,(2*g3z-g3y-6*g3x+2*g2z-g2y+6*g2x)/12,-
(6*g7z+g7y+6*g7x+6*g6z+g6y-6*g6x-6*g3z+g3y+6*g3x-6*g2z+g2y-6*g2x)/24,-(2*g7z+g7y+6*g7x+2*g6z+g6y-6*g6x)/12,-(g7z+g7y+6*g7x+g6z+g6y-6*g6x)/12,-
(g7y+6*g7x+g6y-6*g6x)/12),Array(-(g3x-g2x)/2,(g3z-6*g3x+g2z+6*g2x)/12,(g3z-3*g3x+g2z+3*g2x)/6,-(g7z+g7x+g6z-g6x-g3z+g3x-g2z-g2x)/4,-(g7z+3*g7x+g6z-3*g6x)/6,-
(g7z+6*g7x+g6z-6*g6x)/12,-(g7x-g6x)/2)),Array(Array(-g1x/3,(g1z-2*g1x)/6,(g1z-g1x)/3,-(3*g5z+g5x-3*g1z+g1x)/6,-(g5z+g5x)/3,-(g5z+2*g5x)/6,-g5x/3),Array((g1y-2*g1x)/6,
(g1z+g1y-2*g1x)/6,(2*g1z+g1y-2*g1x)/6,-(6*g5z-g5y+2*g5x-6*g1z-g1y+2*g1x)/12,-(2*g5z-g5y+2*g5x)/6,-(g5z-g5y+2*g5x)/6,(g5y-2*g5x)/6),Array((g1y-g1x)/3,(g1z+2*g1y-2*g1x)/6
,(g1z+g1y-g1x)/3,-(3*g5z-g5y+g5x-3*g1z-g1y+g1x)/6,-(g5z-g5y+g5x)/3,-(g5z-2*g5y+2*g5x)/6,(g5y-g5x)/3),Array(-(3*g3y+g3x-3*g1y+g1x)/6,
(g3z-6*g3y-2*g3x+g1z+6*g1y-2*g1x)/12,(g3z-3*g3y-g3x+g1z+3*g1y-g1x)/6,-(3*g7z+3*g7y+g7x+3*g5z-3*g5y+g5x-3*g3z+3*g3y+g3x-3*g1z-3*g1y+g1x)/12,-
(g7z+3*g7y+g7x+g5z-3*g5y+g5x)/6,-(g7z+6*g7y+2*g7x+g5z-6*g5y+2*g5x)/12,-(3*g7y+g7x-3*g5y+g5x)/6),Array(-(g3y+g3x)/3,(g3z-2*g3y-2*g3x)/6,(g3z-g3y-g3x)/3,-
(3*g7z+g7y+g7x-3*g3z+g3y+g3x)/6,-(g7z+g7y+g7x)/3,-(g7z+2*g7y+2*g7x)/6,-(g7y+g7x)/3),Array(-(g3y+2*g3x)/6,(g3z-g3y-2*g3x)/6,(2*g3z-g3y-2*g3x)/6,-
(6*g7z+g7y+2*g7x-6*g3z+g3y+2*g3x)/12,-(2*g7z+g7y+2*g7x)/6,-(g7z+g7y+2*g7x)/6,-(g7y+2*g7x)/6),Array(-g3x/3,(g3z-2*g3x)/6,(g3z-g3x)/3,-(3*g7z+g7x-3*g3z+g3x)/6,-
(g7z+g7x)/3,-(g7z+2*g7x)/6,-g7x/3)),Array(Array(-g1x/6,(g1z-g1x)/6,(2*g1z-g1x)/6,-(6*g5z+g5x-6*g1z+g1x)/12,-(2*g5z+g5x)/6,-(g5z+g5x)/6,-g5x/6),Array((g1y-g1x)/6,(g1z+g1y-g1x)/6,
(2*g1z+g1y-g1x)/6,-(6*g5z-g5y+g5x-6*g1z-g1y+g1x)/12,-(2*g5z-g5y+g5x)/6,-(g5z-g5y+g5x)/6,(g5y-g5x)/6),Array((2*g1y-g1x)/6,(g1z+2*g1y-g1x)/6,(2*g1z+2*g1y-g1x)/6,-
(6*g5z-2*g5y+g5x-6*g1z-2*g1y+g1x)/12,-(2*g5z-2*g5y+g5x)/6,-(g5z-2*g5y+g5x)/6,(2*g5y-g5x)/6),Array(-(6*g3y+g3x-6*g1y+g1x)/12,(g3z-6*g3y-g3x+g1z+6*g1y-g1x)/12,
(2*g3z-6*g3y-g3x+2*g1z+6*g1y-g1x)/12,-(6*g7z+6*g7y+g7x+6*g5z-6*g5y+g5x-6*g3z+6*g3y+g3x-6*g1z-6*g1y+g1x)/24,-(2*g7z+6*g7y+g7x+2*g5z-6*g5y+g5x)/12,-
(g7z+6*g7y+g7x+g5z-6*g5y+g5x)/12,-(6*g7y+g7x-6*g5y+g5x)/12),Array(-(2*g3y+g3x)/6,(g3z-2*g3y-g3x)/6,(2*g3z-2*g3y-g3x)/6,-(6*g7z+2*g7y+g7x-6*g3z+2*g3y+g3x)/12,-
(2*g7z+2*g7y+g7x)/6,-(g7z+2*g7y+g7x)/6,-(2*g7y+g7x)/6),Array(-(g3y+g3x)/6,(g3z-g3y-g3x)/6,(2*g3z-g3y-g3x)/6,-(6*g7z+g7y+g7x-6*g3z+g3y+g3x)/12,-(2*g7z+g7y+g7x)/6,-
(g7z+g7y+g7x)/6,-(g7y+g7x)/6),Array(-g3x/6,(g3z-g3x)/6,(2*g3z-g3x)/6,-(6*g7z+g7x-6*g3z+g3x)/12,-(2*g7z+g7x)/6,-(g7z+g7x)/6,-g7x/6)),Array(Array(0,g1z/6,g1z/3,-(g5z-g1z)/2,-g5z/3,-
g5z/6,0),Array(g1y/6,(g1z+g1y)/6,(2*g1z+g1y)/6,-(6*g5z-g5y-6*g1z-g1y)/12,-(2*g5z-g5y)/6,-(g5z-g5y)/6,g5y/6),Array(g1y/3,(g1z+2*g1y)/6,(g1z+g1y)/3,-(3*g5z-g5y-3*g1z-g1y)/6,-
(g5z-g5y)/3,-(g5z-2*g5y)/6,g5y/3),Array(-(g3y-g1y)/2,(g3z-6*g3y+g1z+6*g1y)/12,(g3z-3*g3y+g1z+3*g1y)/6,-(g7z+g7y+g5z-g5y-g3z+g3y-g1z-g1y)/4,-
(g7z+3*g7y+g5z-3*g5y)/6,-(g7z+6*g7y+g5z-6*g5y)/12,-(g7y-g5y)/2),Array(-g3y/3,(g3z-2*g3y)/6,(g3z-g3y)/3,-(3*g7z+g7y-3*g3z+g3y)/6,-(g7z+g7y)/3,-(g7z+2*g7y)/6,-g7y/3),Array(-
g3y/6,(g3z-g3y)/6,(2*g3z-g3y)/6,-(6*g7z+g7y-6*g3z+g3y)/12,-(2*g7z+g7y)/6,-(g7z+g7y)/6,-g7y/6),Array(0,g3z/6,g3z/3,-(g7z-g3z)/2,-g7z/3,-g7z/6,0)))		
		

		val n = bezierheights.size

		// Split Curves in X-Direction
		for( i <- 0 until n; j <- 0 until n ) {
			val points = slice((0 until n).map( p => bezierheights(p)(i)(j) ).toArray,relx0,relx1)
			for( p <- 0 until n ) bezierheights(p)(i)(j) = points(p)
		}

		// Split Curves in Y-Direction
		for( i <- 0 until n; j <- 0 until n ) {
			val points = slice((0 until n).map( p => bezierheights(i)(p)(j) ).toArray,rely0,rely1)
			for( p <- 0 until n ) bezierheights(i)(p)(j) = points(p)
		}

		// Split Curves in Z-Direction
		for( i <- 0 until n; j <- 0 until n ) {
			val points = slice((0 until n).map( p => bezierheights(i)(j)(p) ).toArray,relz0,relz1)
			for( p <- 0 until n ) bezierheights(i)(j)(p) = points(p)
		}
		
		// Extract minimum and maximum possible value
		// This works because the polynomial is inside the convex hull of the control points
		var imin = scala.Double.MaxValue
		var imax = scala.Double.MinValue
		for( i <- 0 until n; j <- 0 until n; k <- 0 until n ) {
			val value = bezierheights(i)(j)(k)
			imin = min(imin, value)
			imax = max(imax, value)
		}
		
		Interval(imin,imax)
	}
	
	// Improved Perlin Noise
	def noise3(v:Vec3):Double = noise3(v.x, v.y, v.z)
	def noise3(x:Double, y:Double, z:Double):Double = {

		def grad(hash:Int, x:Double, y:Double, z:Double) = {
			val h = hash & 15
			val u = if(h<8) x else y
			val v = if(h<4) y else {if(h==12 || h==14) x else z}
			(if((h&1) == 0) u else -u) + (if((h&2) == 0) v else -v)
		}
		
		val X = fastfloor(x)
		val Y = fastfloor(y)
		val Z = fastfloor(z)

		val relx = x - X
		val rely = y - Y
		val relz = z - Z

		val u = fade(relx)
		val v = fade(rely)
		val w = fade(relz)
		
		val A = hash(X  )+Y; val AA = hash(A)+Z; val AB = hash(A+1)+Z		// HASH COORDINATES OF
		val	B = hash(X+1)+Y; val BA = hash(B)+Z; val BB = hash(B+1)+Z		// THE 8 CUBE CORNERS,

		lerp(w,	lerp(v,	lerp(u, grad(hash(AA  ), relx  , rely  , relz	),  // AND ADD
								grad(hash(BA  ), relx-1, rely  , relz	)), // BLENDED
						lerp(u, grad(hash(AB  ), relx  , rely-1, relz	),  // RESULTS
								grad(hash(BB  ), relx-1, rely-1, relz	))),// FROM  8
				lerp(v, lerp(u, grad(hash(AA+1), relx  , rely  , relz-1 ),  // CORNERS
								grad(hash(BA+1), relx-1, rely  , relz-1 )), // OF CUBE
						lerp(u, grad(hash(AB+1), relx  , rely-1, relz-1 ),
								grad(hash(BB+1), relx-1, rely-1, relz-1 ))))
	}
}
