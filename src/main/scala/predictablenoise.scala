package noise

import interval.{Interval, Volume}

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._


object Noise {
	def fastfloor(x:Double) = x.floor.toInt //(if(x > 0) x else (x-1)).toInt
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
		
		var i = 0
		var j = 0
		val nm1 = n-1
		while( i <= nm1 ) {
			tmp = result(i)
			j = i+1
			while( j <= n ) {
				save = lerp(t, tmp, result(j))
				tmp = result(j)
				result(j) = save
				j += 1
			}
			i += 1
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

		var i = 0
		var j = 0
		val nm1 = n-1

		while( i <= nm1 ) {
			tmp = result(n-i)
			j = nm1-i
			while( j >= 0 ) {
				save = lerp(t, tmp, result(j))
				tmp = result(j)
				result(j) = save
				j -= 1
			}
			i += 1
		}

		result
	}
	
	def slice(h:Array[Double], t0:Double, t1:Double) = {
		if( t1 == 0 )
			splitleft(splitright(h,t0),-t0/(1-t0))
		else
			splitright(splitleft(h,t1),t0/t1)
	}
	
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

	def noise3_prediction = noise3_prediction_bezier _
	
	def noise3_prediction_trivial(v:Volume) = Interval(-1,1)
	
	// TODO: Code dry machen
	def noise3_prediction_bezier(v:Volume):Interval = {
		import v.x.{low => x0, high => x1}
		import v.y.{low => y0, high => y1}
		import v.z.{low => z0, high => z1}
		
		// Edges of the unit cube
		val X = fastfloor(x0)
		val Y = fastfloor(y0)
		val Z = fastfloor(z0)
		
		// Interval needs to stay inside one unit cube of the lattice
		// If it only touches a few neighbouring lattices, evaluate all
		// and build the hull of the intervals.
		
		// if one of the intervals spreads over more than 2 unit cubes
		if( fastceil(x1) - X > 2 || fastceil(y1) - Y > 2 || fastceil(z1) - Z > 2 ) {
			return Interval(-1,1)
		}
		
		// if interval spreads over more than one unit cube
		if( fastceil(x1) - X > 1 )
			return interval.hull(
				noise3_prediction(Volume(Interval(x0,fastfloor(x0)+1),v.y,v.z)),
				noise3_prediction(Volume(Interval(fastceil(x1)-1,x1),v.y,v.z))
			)
			
		if( fastceil(y1) - Y > 1 )
			return interval.hull(
				noise3_prediction(Volume(v.x, Interval(y0,fastfloor(y0)+1),v.z)),
				noise3_prediction(Volume(v.x, Interval(fastceil(y1)-1,y1),v.z))
			)
			
		if( fastceil(z1) - Z > 1 )
			return interval.hull(
				noise3_prediction(Volume(v.x,v.y,Interval(z0,fastfloor(z0)+1))),
				noise3_prediction(Volume(v.x,v.y,Interval(fastceil(z1)-1,z1)))
			)
		
		
		// relative positions in unit cube
		val relx0 = x0 - X
		val rely0 = y0 - Y
		val relz0 = z0 - Z
		val relx1 = x1 - X
		val rely1 = y1 - Y
		val relz1 = z1 - Z
		
/*		assert(relx0 >= 0 && relx0 <= 1, Interval(relx0,relx1))
		assert(rely0 >= 0 && rely0 <= 1, Interval(rely0,rely1))
		assert(relz0 >= 0 && relz0 <= 1, Interval(relz0,relz1))
		assert(relx1 >= 0 && relx1 <= 1, Interval(relx0,relx1))
		assert(rely1 >= 0 && rely1 <= 1, Interval(rely0,rely1))
		assert(relz1 >= 0 && relz1 <= 1, Interval(relz0,relz1))*/
		
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
Array(Array(Array(0,g0z*0.16666666666666666,g0z*0.3333333333333333,-(g4z-g0z)*0.5,-g4z*0.3333333333333333,-g4z*0.16666666666666666,0),Array(g0y*0.16666666666666666,(g0z+g0y)*0.16666666666666666,(2*g0z+g0y)*0.16666666666666666,-(6*g4z-g4y-6*g0z-g0y)*0.08333333333333333,-(2*g4z-g4y)*0.16666666666666666,-(g4z-g4y)*0.16666666666666666,g4y*0.16666666666666666),Array(g0y*0.3333333333333333,
(g0z+2*g0y)*0.16666666666666666,(g0z+g0y)*0.3333333333333333,-(3*g4z-g4y-3*g0z-g0y)*0.16666666666666666,-(g4z-g4y)*0.3333333333333333,-(g4z-2*g4y)*0.16666666666666666,g4y*0.3333333333333333),Array(-(g2y-g0y)*0.5,(g2z-6*g2y+g0z+6*g0y)*0.08333333333333333,(g2z-3*g2y+g0z+3*g0y)*0.16666666666666666,-
(g6z+g6y+g4z-g4y-g2z+g2y-g0z-g0y)*0.25,-(g6z+3*g6y+g4z-3*g4y)*0.16666666666666666,-(g6z+6*g6y+g4z-6*g4y)*0.08333333333333333,-(g6y-g4y)*0.5),Array(-g2y*0.3333333333333333,(g2z-2*g2y)*0.16666666666666666,(g2z-g2y)*0.3333333333333333,-
(3*g6z+g6y-3*g2z+g2y)*0.16666666666666666,-(g6z+g6y)*0.3333333333333333,-(g6z+2*g6y)*0.16666666666666666,-g6y*0.3333333333333333),Array(-g2y*0.16666666666666666,(g2z-g2y)*0.16666666666666666,(2*g2z-g2y)*0.16666666666666666,-(6*g6z+g6y-6*g2z+g2y)*0.08333333333333333,-(2*g6z+g6y)*0.16666666666666666,-(g6z+g6y)*0.16666666666666666,-g6y*0.16666666666666666),Array(0,g2z*0.16666666666666666
,g2z*0.3333333333333333,-(g6z-g2z)*0.5,-g6z*0.3333333333333333,-g6z*0.16666666666666666,0)),Array(Array(g0x*0.16666666666666666,(g0z+g0x)*0.16666666666666666,(2*g0z+g0x)*0.16666666666666666,-(6*g4z-g4x-6*g0z-g0x)*0.08333333333333333,-(2*g4z-g4x)*0.16666666666666666,-(g4z-g4x)*0.16666666666666666,g4x*0.16666666666666666),Array((g0y+g0x)*0.16666666666666666,(g0z+g0y+g0x)*0.16666666666666666,
(2*g0z+g0y+g0x)*0.16666666666666666,-(6*g4z-g4y-g4x-6*g0z-g0y-g0x)*0.08333333333333333,-(2*g4z-g4y-g4x)*0.16666666666666666,-(g4z-g4y-g4x)*0.16666666666666666,(g4y+g4x)*0.16666666666666666),Array((2*g0y+g0x)*0.16666666666666666,(g0z+2*g0y+g0x)*0.16666666666666666,(2*g0z+2*g0y+g0x)*0.16666666666666666,-
(6*g4z-2*g4y-g4x-6*g0z-2*g0y-g0x)*0.08333333333333333,-(2*g4z-2*g4y-g4x)*0.16666666666666666,-(g4z-2*g4y-g4x)*0.16666666666666666,(2*g4y+g4x)*0.16666666666666666),Array(-(6*g2y-g2x-6*g0y-g0x)*0.08333333333333333,(g2z-6*g2y+g2x+g0z+6*g0y+g0x)*0.08333333333333333,
(2*g2z-6*g2y+g2x+2*g0z+6*g0y+g0x)*0.08333333333333333,-(6*g6z+6*g6y-g6x+6*g4z-6*g4y-g4x-6*g2z+6*g2y-g2x-6*g0z-6*g0y-g0x)*0.041666666666666664,-(2*g6z+6*g6y-g6x+2*g4z-6*g4y-g4x)*0.08333333333333333,-
(g6z+6*g6y-g6x+g4z-6*g4y-g4x)*0.08333333333333333,-(6*g6y-g6x-6*g4y-g4x)*0.08333333333333333),Array(-(2*g2y-g2x)*0.16666666666666666,(g2z-2*g2y+g2x)*0.16666666666666666,(2*g2z-2*g2y+g2x)*0.16666666666666666,-(6*g6z+2*g6y-g6x-6*g2z+2*g2y-g2x)*0.08333333333333333,-
(2*g6z+2*g6y-g6x)*0.16666666666666666,-(g6z+2*g6y-g6x)*0.16666666666666666,-(2*g6y-g6x)*0.16666666666666666),Array(-(g2y-g2x)*0.16666666666666666,(g2z-g2y+g2x)*0.16666666666666666,(2*g2z-g2y+g2x)*0.16666666666666666,-(6*g6z+g6y-g6x-6*g2z+g2y-g2x)*0.08333333333333333,-(2*g6z+g6y-g6x)*0.16666666666666666,-
(g6z+g6y-g6x)*0.16666666666666666,-(g6y-g6x)*0.16666666666666666),Array(g2x*0.16666666666666666,(g2z+g2x)*0.16666666666666666,(2*g2z+g2x)*0.16666666666666666,-(6*g6z-g6x-6*g2z-g2x)*0.08333333333333333,-(2*g6z-g6x)*0.16666666666666666,-(g6z-g6x)*0.16666666666666666,g6x*0.16666666666666666)),Array(Array(g0x*0.3333333333333333,(g0z+2*g0x)*0.16666666666666666,(g0z+g0x)*0.3333333333333333,-
(3*g4z-g4x-3*g0z-g0x)*0.16666666666666666,-(g4z-g4x)*0.3333333333333333,-(g4z-2*g4x)*0.16666666666666666,g4x*0.3333333333333333),Array((g0y+2*g0x)*0.16666666666666666,(g0z+g0y+2*g0x)*0.16666666666666666,(2*g0z+g0y+2*g0x)*0.16666666666666666,-(6*g4z-g4y-2*g4x-6*g0z-g0y-2*g0x)*0.08333333333333333,-
(2*g4z-g4y-2*g4x)*0.16666666666666666,-(g4z-g4y-2*g4x)*0.16666666666666666,(g4y+2*g4x)*0.16666666666666666),Array((g0y+g0x)*0.3333333333333333,(g0z+2*g0y+2*g0x)*0.16666666666666666,(g0z+g0y+g0x)*0.3333333333333333,-(3*g4z-g4y-g4x-3*g0z-g0y-g0x)*0.16666666666666666,-(g4z-g4y-g4x)*0.3333333333333333,-
(g4z-2*g4y-2*g4x)*0.16666666666666666,(g4y+g4x)*0.3333333333333333),Array(-(3*g2y-g2x-3*g0y-g0x)*0.16666666666666666,(g2z-6*g2y+2*g2x+g0z+6*g0y+2*g0x)*0.08333333333333333,(g2z-3*g2y+g2x+g0z+3*g0y+g0x)*0.16666666666666666,-
(3*g6z+3*g6y-g6x+3*g4z-3*g4y-g4x-3*g2z+3*g2y-g2x-3*g0z-3*g0y-g0x)*0.08333333333333333,-(g6z+3*g6y-g6x+g4z-3*g4y-g4x)*0.16666666666666666,-(g6z+6*g6y-2*g6x+g4z-6*g4y-2*g4x)*0.08333333333333333,-
(3*g6y-g6x-3*g4y-g4x)*0.16666666666666666),Array(-(g2y-g2x)*0.3333333333333333,(g2z-2*g2y+2*g2x)*0.16666666666666666,(g2z-g2y+g2x)*0.3333333333333333,-(3*g6z+g6y-g6x-3*g2z+g2y-g2x)*0.16666666666666666,-(g6z+g6y-g6x)*0.3333333333333333,-(g6z+2*g6y-2*g6x)*0.16666666666666666,-(g6y-g6x)*0.3333333333333333)
,Array(-(g2y-2*g2x)*0.16666666666666666,(g2z-g2y+2*g2x)*0.16666666666666666,(2*g2z-g2y+2*g2x)*0.16666666666666666,-(6*g6z+g6y-2*g6x-6*g2z+g2y-2*g2x)*0.08333333333333333,-(2*g6z+g6y-2*g6x)*0.16666666666666666,-(g6z+g6y-2*g6x)*0.16666666666666666,-(g6y-2*g6x)*0.16666666666666666),Array(g2x*0.3333333333333333,
(g2z+2*g2x)*0.16666666666666666,(g2z+g2x)*0.3333333333333333,-(3*g6z-g6x-3*g2z-g2x)*0.16666666666666666,-(g6z-g6x)*0.3333333333333333,-(g6z-2*g6x)*0.16666666666666666,g6x*0.3333333333333333)),Array(Array(-(g1x-g0x)*0.5,(g1z-6*g1x+g0z+6*g0x)*0.08333333333333333,(g1z-3*g1x+g0z+3*g0x)*0.16666666666666666,-
(g5z+g5x+g4z-g4x-g1z+g1x-g0z-g0x)*0.25,-(g5z+3*g5x+g4z-3*g4x)*0.16666666666666666,-(g5z+6*g5x+g4z-6*g4x)*0.08333333333333333,-(g5x-g4x)*0.5),Array((g1y-6*g1x+g0y+6*g0x)*0.08333333333333333,
(g1z+g1y-6*g1x+g0z+g0y+6*g0x)*0.08333333333333333,(2*g1z+g1y-6*g1x+2*g0z+g0y+6*g0x)*0.08333333333333333,-(6*g5z-g5y+6*g5x+6*g4z-g4y-6*g4x-6*g1z-g1y+6*g1x-6*g0z-g0y-6*g0x)*0.041666666666666664,-
(2*g5z-g5y+6*g5x+2*g4z-g4y-6*g4x)*0.08333333333333333,-(g5z-g5y+6*g5x+g4z-g4y-6*g4x)*0.08333333333333333,(g5y-6*g5x+g4y+6*g4x)*0.08333333333333333),Array((g1y-3*g1x+g0y+3*g0x)*0.16666666666666666,
(g1z+2*g1y-6*g1x+g0z+2*g0y+6*g0x)*0.08333333333333333,(g1z+g1y-3*g1x+g0z+g0y+3*g0x)*0.16666666666666666,-(3*g5z-g5y+3*g5x+3*g4z-g4y-3*g4x-3*g1z-g1y+3*g1x-3*g0z-g0y-3*g0x)*0.08333333333333333,-
(g5z-g5y+3*g5x+g4z-g4y-3*g4x)*0.16666666666666666,-(g5z-2*g5y+6*g5x+g4z-2*g4y-6*g4x)*0.08333333333333333,(g5y-3*g5x+g4y+3*g4x)*0.16666666666666666),Array(-(g3y+g3x+g2y-g2x-g1y+g1x-g0y-g0x)*0.25,
(g3z-6*g3y-6*g3x+g2z-6*g2y+6*g2x+g1z+6*g1y-6*g1x+g0z+6*g0y+6*g0x)*0.041666666666666664,(g3z-3*g3y-3*g3x+g2z-3*g2y+3*g2x+g1z+3*g1y-3*g1x+g0z+3*g0y+3*g0x)*0.08333333333333333,-
(g7z+g7y+g7x+g6z+g6y-g6x+g5z-g5y+g5x+g4z-g4y-g4x-g3z+g3y+g3x-g2z+g2y-g2x-g1z-g1y+g1x-g0z-g0y-g0x)*0.125,-
(g7z+3*g7y+3*g7x+g6z+3*g6y-3*g6x+g5z-3*g5y+3*g5x+g4z-3*g4y-3*g4x)*0.08333333333333333,-(g7z+6*g7y+6*g7x+g6z+6*g6y-6*g6x+g5z-6*g5y+6*g5x+g4z-6*g4y-6*g4x)*0.041666666666666664,-
(g7y+g7x+g6y-g6x-g5y+g5x-g4y-g4x)*0.25),Array(-(g3y+3*g3x+g2y-3*g2x)*0.16666666666666666,(g3z-2*g3y-6*g3x+g2z-2*g2y+6*g2x)*0.08333333333333333,(g3z-g3y-3*g3x+g2z-g2y+3*g2x)*0.16666666666666666,-
(3*g7z+g7y+3*g7x+3*g6z+g6y-3*g6x-3*g3z+g3y+3*g3x-3*g2z+g2y-3*g2x)*0.08333333333333333,-(g7z+g7y+3*g7x+g6z+g6y-3*g6x)*0.16666666666666666,-(g7z+2*g7y+6*g7x+g6z+2*g6y-6*g6x)*0.08333333333333333,-
(g7y+3*g7x+g6y-3*g6x)*0.16666666666666666),Array(-(g3y+6*g3x+g2y-6*g2x)*0.08333333333333333,(g3z-g3y-6*g3x+g2z-g2y+6*g2x)*0.08333333333333333,(2*g3z-g3y-6*g3x+2*g2z-g2y+6*g2x)*0.08333333333333333,-
(6*g7z+g7y+6*g7x+6*g6z+g6y-6*g6x-6*g3z+g3y+6*g3x-6*g2z+g2y-6*g2x)*0.041666666666666664,-(2*g7z+g7y+6*g7x+2*g6z+g6y-6*g6x)*0.08333333333333333,-(g7z+g7y+6*g7x+g6z+g6y-6*g6x)*0.08333333333333333,-
(g7y+6*g7x+g6y-6*g6x)*0.08333333333333333),Array(-(g3x-g2x)*0.5,(g3z-6*g3x+g2z+6*g2x)*0.08333333333333333,(g3z-3*g3x+g2z+3*g2x)*0.16666666666666666,-(g7z+g7x+g6z-g6x-g3z+g3x-g2z-g2x)*0.25,-(g7z+3*g7x+g6z-3*g6x)*0.16666666666666666,-
(g7z+6*g7x+g6z-6*g6x)*0.08333333333333333,-(g7x-g6x)*0.5)),Array(Array(-g1x*0.3333333333333333,(g1z-2*g1x)*0.16666666666666666,(g1z-g1x)*0.3333333333333333,-(3*g5z+g5x-3*g1z+g1x)*0.16666666666666666,-(g5z+g5x)*0.3333333333333333,-(g5z+2*g5x)*0.16666666666666666,-g5x*0.3333333333333333),Array((g1y-2*g1x)*0.16666666666666666,
(g1z+g1y-2*g1x)*0.16666666666666666,(2*g1z+g1y-2*g1x)*0.16666666666666666,-(6*g5z-g5y+2*g5x-6*g1z-g1y+2*g1x)*0.08333333333333333,-(2*g5z-g5y+2*g5x)*0.16666666666666666,-(g5z-g5y+2*g5x)*0.16666666666666666,(g5y-2*g5x)*0.16666666666666666),Array((g1y-g1x)*0.3333333333333333,(g1z+2*g1y-2*g1x)*0.16666666666666666
,(g1z+g1y-g1x)*0.3333333333333333,-(3*g5z-g5y+g5x-3*g1z-g1y+g1x)*0.16666666666666666,-(g5z-g5y+g5x)*0.3333333333333333,-(g5z-2*g5y+2*g5x)*0.16666666666666666,(g5y-g5x)*0.3333333333333333),Array(-(3*g3y+g3x-3*g1y+g1x)*0.16666666666666666,
(g3z-6*g3y-2*g3x+g1z+6*g1y-2*g1x)*0.08333333333333333,(g3z-3*g3y-g3x+g1z+3*g1y-g1x)*0.16666666666666666,-(3*g7z+3*g7y+g7x+3*g5z-3*g5y+g5x-3*g3z+3*g3y+g3x-3*g1z-3*g1y+g1x)*0.08333333333333333,-
(g7z+3*g7y+g7x+g5z-3*g5y+g5x)*0.16666666666666666,-(g7z+6*g7y+2*g7x+g5z-6*g5y+2*g5x)*0.08333333333333333,-(3*g7y+g7x-3*g5y+g5x)*0.16666666666666666),Array(-(g3y+g3x)*0.3333333333333333,(g3z-2*g3y-2*g3x)*0.16666666666666666,(g3z-g3y-g3x)*0.3333333333333333,-
(3*g7z+g7y+g7x-3*g3z+g3y+g3x)*0.16666666666666666,-(g7z+g7y+g7x)*0.3333333333333333,-(g7z+2*g7y+2*g7x)*0.16666666666666666,-(g7y+g7x)*0.3333333333333333),Array(-(g3y+2*g3x)*0.16666666666666666,(g3z-g3y-2*g3x)*0.16666666666666666,(2*g3z-g3y-2*g3x)*0.16666666666666666,-
(6*g7z+g7y+2*g7x-6*g3z+g3y+2*g3x)*0.08333333333333333,-(2*g7z+g7y+2*g7x)*0.16666666666666666,-(g7z+g7y+2*g7x)*0.16666666666666666,-(g7y+2*g7x)*0.16666666666666666),Array(-g3x*0.3333333333333333,(g3z-2*g3x)*0.16666666666666666,(g3z-g3x)*0.3333333333333333,-(3*g7z+g7x-3*g3z+g3x)*0.16666666666666666,-
(g7z+g7x)*0.3333333333333333,-(g7z+2*g7x)*0.16666666666666666,-g7x*0.3333333333333333)),Array(Array(-g1x*0.16666666666666666,(g1z-g1x)*0.16666666666666666,(2*g1z-g1x)*0.16666666666666666,-(6*g5z+g5x-6*g1z+g1x)*0.08333333333333333,-(2*g5z+g5x)*0.16666666666666666,-(g5z+g5x)*0.16666666666666666,-g5x*0.16666666666666666),Array((g1y-g1x)*0.16666666666666666,(g1z+g1y-g1x)*0.16666666666666666,
(2*g1z+g1y-g1x)*0.16666666666666666,-(6*g5z-g5y+g5x-6*g1z-g1y+g1x)*0.08333333333333333,-(2*g5z-g5y+g5x)*0.16666666666666666,-(g5z-g5y+g5x)*0.16666666666666666,(g5y-g5x)*0.16666666666666666),Array((2*g1y-g1x)*0.16666666666666666,(g1z+2*g1y-g1x)*0.16666666666666666,(2*g1z+2*g1y-g1x)*0.16666666666666666,-
(6*g5z-2*g5y+g5x-6*g1z-2*g1y+g1x)*0.08333333333333333,-(2*g5z-2*g5y+g5x)*0.16666666666666666,-(g5z-2*g5y+g5x)*0.16666666666666666,(2*g5y-g5x)*0.16666666666666666),Array(-(6*g3y+g3x-6*g1y+g1x)*0.08333333333333333,(g3z-6*g3y-g3x+g1z+6*g1y-g1x)*0.08333333333333333,
(2*g3z-6*g3y-g3x+2*g1z+6*g1y-g1x)*0.08333333333333333,-(6*g7z+6*g7y+g7x+6*g5z-6*g5y+g5x-6*g3z+6*g3y+g3x-6*g1z-6*g1y+g1x)*0.041666666666666664,-(2*g7z+6*g7y+g7x+2*g5z-6*g5y+g5x)*0.08333333333333333,-
(g7z+6*g7y+g7x+g5z-6*g5y+g5x)*0.08333333333333333,-(6*g7y+g7x-6*g5y+g5x)*0.08333333333333333),Array(-(2*g3y+g3x)*0.16666666666666666,(g3z-2*g3y-g3x)*0.16666666666666666,(2*g3z-2*g3y-g3x)*0.16666666666666666,-(6*g7z+2*g7y+g7x-6*g3z+2*g3y+g3x)*0.08333333333333333,-
(2*g7z+2*g7y+g7x)*0.16666666666666666,-(g7z+2*g7y+g7x)*0.16666666666666666,-(2*g7y+g7x)*0.16666666666666666),Array(-(g3y+g3x)*0.16666666666666666,(g3z-g3y-g3x)*0.16666666666666666,(2*g3z-g3y-g3x)*0.16666666666666666,-(6*g7z+g7y+g7x-6*g3z+g3y+g3x)*0.08333333333333333,-(2*g7z+g7y+g7x)*0.16666666666666666,-
(g7z+g7y+g7x)*0.16666666666666666,-(g7y+g7x)*0.16666666666666666),Array(-g3x*0.16666666666666666,(g3z-g3x)*0.16666666666666666,(2*g3z-g3x)*0.16666666666666666,-(6*g7z+g7x-6*g3z+g3x)*0.08333333333333333,-(2*g7z+g7x)*0.16666666666666666,-(g7z+g7x)*0.16666666666666666,-g7x*0.16666666666666666)),Array(Array(0,g1z*0.16666666666666666,g1z*0.3333333333333333,-(g5z-g1z)*0.5,-g5z*0.3333333333333333,-
g5z*0.16666666666666666,0),Array(g1y*0.16666666666666666,(g1z+g1y)*0.16666666666666666,(2*g1z+g1y)*0.16666666666666666,-(6*g5z-g5y-6*g1z-g1y)*0.08333333333333333,-(2*g5z-g5y)*0.16666666666666666,-(g5z-g5y)*0.16666666666666666,g5y*0.16666666666666666),Array(g1y*0.3333333333333333,(g1z+2*g1y)*0.16666666666666666,(g1z+g1y)*0.3333333333333333,-(3*g5z-g5y-3*g1z-g1y)*0.16666666666666666,-
(g5z-g5y)*0.3333333333333333,-(g5z-2*g5y)*0.16666666666666666,g5y*0.3333333333333333),Array(-(g3y-g1y)*0.5,(g3z-6*g3y+g1z+6*g1y)*0.08333333333333333,(g3z-3*g3y+g1z+3*g1y)*0.16666666666666666,-(g7z+g7y+g5z-g5y-g3z+g3y-g1z-g1y)*0.25,-
(g7z+3*g7y+g5z-3*g5y)*0.16666666666666666,-(g7z+6*g7y+g5z-6*g5y)*0.08333333333333333,-(g7y-g5y)*0.5),Array(-g3y*0.3333333333333333,(g3z-2*g3y)*0.16666666666666666,(g3z-g3y)*0.3333333333333333,-(3*g7z+g7y-3*g3z+g3y)*0.16666666666666666,-(g7z+g7y)*0.3333333333333333,-(g7z+2*g7y)*0.16666666666666666,-g7y*0.3333333333333333),Array(-
g3y*0.16666666666666666,(g3z-g3y)*0.16666666666666666,(2*g3z-g3y)*0.16666666666666666,-(6*g7z+g7y-6*g3z+g3y)*0.08333333333333333,-(2*g7z+g7y)*0.16666666666666666,-(g7z+g7y)*0.16666666666666666,-g7y*0.16666666666666666),Array(0,g3z*0.16666666666666666,g3z*0.3333333333333333,-(g7z-g3z)*0.5,-g7z*0.3333333333333333,-g7z*0.16666666666666666,0)))		


		val n = 7

		var i = 0; var j = 0; var k = 0; var p = 0
		var points =  Array.ofDim[Double](7)
		
		// Split Curves in X-Direction
		i = 0
		while( i < n ) {
			j = 0
			while( j < n ) {
				p = 0; while( p < n ) { points(p) = bezierheights(p)(i)(j); p+= 1 }
				points = slice(points, relx0, relx1)
				p = 0; while( p < n ) { bezierheights(p)(i)(j) = points(p); p+= 1 }
				j += 1
			}
			i += 1
		}

		// Split Curves in Y-Direction
		i = 0
		while( i < n ) {
			j = 0
			while( j < n ) {
				p = 0; while( p < n ) { points(p) = bezierheights(i)(p)(j); p+= 1 }
				points = slice(points, rely0, rely1)
				p = 0; while( p < n ) { bezierheights(i)(p)(j) = points(p); p+= 1 }
				j += 1
			}
			i += 1
		}

		// Split Curves in Z-Direction
		i = 0
		while( i < n ) {
			j = 0
			while( j < n ) {
				p = 0; while( p < n ) { points(p) = bezierheights(i)(j)(p); p+= 1 }
				points = slice(points, relz0, relz1)
				p = 0; while( p < n ) { bezierheights(i)(j)(p) = points(p); p+= 1 }
				j += 1
			}
			i += 1
		}
		
		// Extract minimum and maximum possible value
		// This works because the polynomial is inside the convex hull of the control points
		var imin = scala.Double.MaxValue
		var imax = scala.Double.MinValue
		i = 0
		while( i < n ) {
			j = 0
			while( j < n ) {
				k = 0
				while( k < n ) {

					val value = bezierheights(i)(j)(k)
					imin = min(imin, value)
					imax = max(imax, value)

					k += 1
				}
				j += 1
			}
			i += 1
		}
				
		Interval(max(imin,-1), min(imax,1))
	}
	
////////////////////////////////////////////////////////
	
	def isEven(a:Int) = (a & 1) == 0
	def isOdd(a:Int) = (a & 1) == 1
//	def sum(start:Int, end:Int, func:(Int) => Double ) = (start to end).map( func ).sum
	def sum(start:Int, end:Int, func:(Int) => Double ) = {
		var sum = 0.0
		var i = start
		while( i <= end ) {
			sum += func(i)
			i += 1
		}
		sum
	}

	def binomial(n:Int, k:Int):Int = {
		assert(k <= n)
		if( k == 0 ) return 1
		else if( 2*k > n ) return binomial(n, n-k)
		else {
			var result = n - k + 1
			for( i <- 2 to k ) {
				result *= n - k + i
				result /= i
			}
			return result
		}
	}
	
	object Tensor {
		def apply(dim0:Int, dim1:Int) = new Tensor2(Array.ofDim[Double](dim0,dim1))
		def apply(dim0:Int, dim1:Int, dim2:Int) = new Tensor3(Array.ofDim[Double](dim0,dim1,dim2))
	}

	class Tensor2(data:Array[Array[Double]]) {
		def dim0 = data.size
		def dim1 = data(0).size

		def apply(x:Int, y:Int) = data(x)(y)
		def update(x:Int, y:Int, value:Double) {
			data(x)(y) = value
		}
	
		// Tensor2 *_x Tensor3: R^(m x o) * R^(m x n x l) = R^(o x n x l)
		def prodx(B:Tensor3) = {
			assert( A.dim0 == B.dim0, "Dimension mismatch: %d != %d" format(A.dim0, B.dim0))
			def A = this
			val C = Tensor(A.dim1, B.dim1, B.dim2)
			var i = 0; var j = 0; var k = 0; var h = 0; var sum = 0.0
			while( i < C.dim0 ) {
				j = 0
				while( j < C.dim1 ) {
					k = 0
					while( k < C.dim2 ) {
						sum = 0.0
						h = 0
						while( h <= B.dim0-1 ) {
							sum += A(i,h)* B(h,j,k)
							h += 1
						}
						C(i,j,k) = sum
					
						//C(i,j,k) = sum(0, B.dim0-1,h => A(i,h)* B(h,j,k) )
						k += 1
					}
					j += 1
				}
				i += 1
			}
			
			
			/*for( i <- 0 until C.dim0; j <- 0 until C.dim1; k <- 0 until C.dim2 )
				C(i,j,k) = sum(0, B.dim0-1,h => A(i,h)* B(h,j,k) )*/
			C
		}

		// Tensor2 *_z Tensor3: R^(l x o) * R^(m x n x l) = R^(m x n x o)
		def prodz(B:Tensor3) = {
			assert( A.dim0 == B.dim2, "Dimension mismatch: %d != %d" format(A.dim0, B.dim2))
			def A = this
			val C = Tensor(B.dim0, B.dim1, A.dim1)

			var i = 0; var j = 0; var k = 0
			while( i < C.dim0 ) {
				j = 0
				while( j < C.dim1 ) {
					k = 0
					while( k < C.dim2 ) {
						C(i,j,k) = sum(0, B.dim0-1,h => A(k,h)* B(i,j,h) )
						k += 1
					}
					j += 1
				}
				i += 1
			}


//			for( i <- 0 until C.dim0; j <- 0 until C.dim1; k <- 0 until C.dim2 )
//				C(i,j,k) = sum(0, B.dim0-1,h => A(k,h)* B(i,j,h) )
			C
		}

		override def toString = 
			"Tensor(%d,%d)".format(dim0,dim1) +
			(for( i <- 0 until dim0 ) yield
				(for(j <- 0 until dim1) yield
							(apply(i,j)*10).round/10.0
				).mkString(" ") + "\n"
			).mkString
	}


	class Tensor3(data:Array[Array[Array[Double]]]) {
		def dim0 = data.size
		def dim1 = data(0).size
		def dim2 = data(0)(0).size
	
		def apply(x:Int,y:Int,z:Int) = data(x)(y)(z)
		def update(x:Int, y:Int, z:Int, value:Double) {
			data(x)(y)(z) = value
		}
	
		// Tensor3 *_y Tensor2: R^(m x n x l) * R^(n x o) = R^(m x o x l)
		def prody(B:Tensor2) = {
			assert( A.dim1 == B.dim0, "Dimension mismatch: %d != %d" format(A.dim1, B.dim0))
			def A = this
			val C = Tensor(A.dim0, B.dim0, A.dim2)

			var i = 0; var j = 0; var k = 0
			while( i < C.dim0 ) {
				j = 0
				while( j < C.dim1 ) {
					k = 0
					while( k < C.dim2 ) {
						C(i,j,k) = sum(0,A.dim1-1,h => A(i,h,k) * B(h,j))
						k += 1
					}
					j += 1
				}
				i += 1
			}


//			for( i <- 0 until C.dim0; j <- 0 until C.dim1; k <- 0 until C.dim2 )
//				C(i,j,k) = sum(0,A.dim1-1,h => A(i,h,k) * B(h,j))
			C
		}

		override def toString = 
			"Tensor(%d,%d,%d)".format(dim0,dim1,dim2) +
			(for( k <- 0 until dim2 ) yield
				(for( i <- 0 until dim0 ) yield
					(for(j <- 0 until dim1) yield
							(apply(i,j,k)*10).round/10.0
					).mkString(" ") + "\n"
				).mkString
			).mkString("\n")
	}	


	def noise3_prediction_MAA(v:Volume):Interval = {
		import v.x.{low => x0a, high => x1a}
		import v.y.{low => y0a, high => y1a}
		import v.z.{low => z0a, high => z1a}
		
		// Edges of the unit cube
		val X = fastfloor(x0a)
		val Y = fastfloor(y0a)
		val Z = fastfloor(z0a)
		
		// Interval needs to stay inside one unit cube of the lattice
		// If it only touches a few neighbouring lattices, evaluate all
		// and build the hull of the intervals.
		
		// if one of the intervals spreads over more than 2 unit cubes
		if( fastceil(x1a) - X > 2 || fastceil(y1a) - Y > 2 || fastceil(z1a) - Z > 2 ) {
			return Interval(-1,1)
		}
		
		// if interval spreads over more than one unit cube
		if( fastceil(x1a) - X > 1 )
			return interval.hull(
				noise3_prediction(Volume(Interval(x0a,fastfloor(x0a)+1),v.y,v.z)),
				noise3_prediction(Volume(Interval(fastceil(x1a)-1,x1a),v.y,v.z))
			)
			
		if( fastceil(y1a) - Y > 1 )
			return interval.hull(
				noise3_prediction(Volume(v.x, Interval(y0a,fastfloor(y0a)+1),v.z)),
				noise3_prediction(Volume(v.x, Interval(fastceil(y1a)-1,y1a),v.z))
			)
			
		if( fastceil(z1a) - Z > 1 )
			return interval.hull(
				noise3_prediction(Volume(v.x,v.y,Interval(z0a,fastfloor(z0a)+1))),
				noise3_prediction(Volume(v.x,v.y,Interval(fastceil(z1a)-1,z1a)))
			)
		
		
		// relative positions in unit cube
		val relx0 = x0a - X
		val rely0 = y0a - Y
		val relz0 = z0a - Z
		val relx1 = x1a - X
		val rely1 = y1a - Y
		val relz1 = z1a - Z
		
/*		assert(relx0 >= 0 && relx0 <= 1, Interval(relx0,relx1))
		assert(rely0 >= 0 && rely0 <= 1, Interval(rely0,rely1))
		assert(relz0 >= 0 && relz0 <= 1, Interval(relz0,relz1))
		assert(relx1 >= 0 && relx1 <= 1, Interval(relx0,relx1))
		assert(rely1 >= 0 && rely1 <= 1, Interval(rely0,rely1))
		assert(relz1 >= 0 && relz1 <= 1, Interval(relz0,relz1))*/
		
		// Get the Pseudorandom Gradients for each Lattice point
		val Vec3(g0x,g0y,g0z) = gradientat3(X  ,Y  ,Z  )
		val Vec3(g1x,g1y,g1z) = gradientat3(X+1,Y  ,Z  )
		val Vec3(g2x,g2y,g2z) = gradientat3(X  ,Y+1,Z  )
		val Vec3(g3x,g3y,g3z) = gradientat3(X+1,Y+1,Z  )
		val Vec3(g4x,g4y,g4z) = gradientat3(X  ,Y  ,Z+1)
		val Vec3(g5x,g5y,g5z) = gradientat3(X+1,Y  ,Z+1)
		val Vec3(g6x,g6y,g6z) = gradientat3(X  ,Y+1,Z+1)
		val Vec3(g7x,g7y,g7z) = gradientat3(X+1,Y+1,Z+1)
		
		val (xl,xh) = (relx0, relx1)
		val (yl,yh) = (rely0, rely1)
		val (zl,zh) = (relz0, relz1)

		val (x0,x1) = ((xh+xl)*0.5, (xh-xl)*0.5)
		val (y0,y1) = ((yh+yl)*0.5, (yh-yl)*0.5)
		val (z0,z1) = ((zh+zl)*0.5, (zh-zl)*0.5)
		
		val A = new Tensor3(
Array(Array(Array(0,g0z,0,-10*g4z,5*(5*g4z-2*g0z),-3*(7*g4z-5*g0z),6*(g4z-g0z)),Array(g0y,0,0,10*(g4y-g0y),-15*(g4y-g0y),6*(g4y-g0y),0),Array(0,
0,0,0,0,0,0),Array(-10*g2y,10*(g2z-g0z),0,-100*(g6z+g6y-g4z-g2y),50*(5*g6z+3*g6y-5*g4z-2*g2z-3*g2y+2*g0z),-30*
(7*g6z+2*g6y-7*g4z-5*g2z-2*g2y+5*g0z),60*(g6z-g4z-g2z+g0z)),Array(5*(5*g2y-2*g0y),-15*(g2z-g0z),0,50*
(3*g6z+5*g6y-3*g4z-2*g4y-5*g2y+2*g0y),-75*(5*g6z+5*g6y-5*g4z-2*g4y-2*g2z-5*g2y+2*g0z+2*g0y),15*
(21*g6z+10*g6y-21*g4z-4*g4y-15*g2z-10*g2y+15*g0z+4*g0y),-90*(g6z-g4z-g2z+g0z)),Array(-3*(7*g2y-5*g0y),6*(g2z-g0z),0,-30*
(2*g6z+7*g6y-2*g4z-5*g4y-7*g2y+5*g0y),15*(10*g6z+21*g6y-10*g4z-15*g4y-4*g2z-21*g2y+4*g0z+15*g0y),-18*
(7*g6z+7*g6y-7*g4z-5*g4y-5*g2z-7*g2y+5*g0z+5*g0y),36*(g6z-g4z-g2z+g0z)),Array(6*(g2y-g0y),0,0,60*(g6y-g4y-g2y+g0y),-90*
(g6y-g4y-g2y+g0y),36*(g6y-g4y-g2y+g0y),0)),Array(Array(g0x,0,0,10*(g4x-g0x),-15*(g4x-g0x),6*(g4x-g0x),0),Array(0,0,0,0,0,0,0),Array(0,0,0,0,
0,0,0),Array(10*(g2x-g0x),0,0,100*(g6x-g4x-g2x+g0x),-150*(g6x-g4x-g2x+g0x),60*(g6x-g4x-g2x+g0x),0),Array(-15*(g2x-g0x),0,0,-150*
(g6x-g4x-g2x+g0x),225*(g6x-g4x-g2x+g0x),-90*(g6x-g4x-g2x+g0x),0),Array(6*(g2x-g0x),0,0,60*(g6x-g4x-g2x+g0x),-90*
(g6x-g4x-g2x+g0x),36*(g6x-g4x-g2x+g0x),0),Array(0,0,0,0,0,0,0)),Array(Array(0,0,0,0,0,0,0),Array(0,0,0,0,0,0,0),Array(0,0,0,0,0,0,0),Array(0,0,0,0,0,0
,0),Array(0,0,0,0,0,0,0),Array(0,0,0,0,0,0,0),Array(0,0,0,0,0,0,0)),Array(Array(-10*g1x,10*(g1z-g0z),0,-100*(g5z+g5x-g4z-g1x),50*
(5*g5z+3*g5x-5*g4z-2*g1z-3*g1x+2*g0z),-30*(7*g5z+2*g5x-7*g4z-5*g1z-2*g1x+5*g0z),60*(g5z-g4z-g1z+g0z)),Array(10*(g1y-g0y),0,0,100*
(g5y-g4y-g1y+g0y),-150*(g5y-g4y-g1y+g0y),60*(g5y-g4y-g1y+g0y),0),Array(0,0,0,0,0,0,0),Array(-100*(g3y+g3x-g2y-g1x),100*
(g3z-g2z-g1z+g0z),0,-1000*(g7z+g7y+g7x-g6z-g6y-g5z-g5x+g4z-g3y-g3x+g2y+g1x),500*
(5*g7z+3*g7y+3*g7x-5*g6z-3*g6y-5*g5z-3*g5x+5*g4z-2*g3z-3*g3y-3*g3x+2*g2z+3*g2y+2*g1z+3*g1x-2*g0z),-300*
(7*g7z+2*g7y+2*g7x-7*g6z-2*g6y-7*g5z-2*g5x+7*g4z-5*g3z-2*g3y-2*g3x+5*g2z+2*g2y+5*g1z+2*g1x-5*g0z),600*
(g7z-g6z-g5z+g4z-g3z+g2z+g1z-g0z)),Array(50*(5*g3y+3*g3x-5*g2y-2*g1y-3*g1x+2*g0y),-150*(g3z-g2z-g1z+g0z),0,500*
(3*g7z+5*g7y+3*g7x-3*g6z-5*g6y-3*g5z-2*g5y-3*g5x+3*g4z+2*g4y-5*g3y-3*g3x+5*g2y+2*g1y+3*g1x-2*g0y),-750*
(5*g7z+5*g7y+3*g7x-5*g6z-5*g6y-5*g5z-2*g5y-3*g5x+5*g4z+2*g4y-2*g3z-5*g3y-3*g3x+2*g2z+5*g2y+2*g1z+2*g1y+3*g1x-2*g0z-2*g0y)
,150*(21*g7z+10*g7y+6*g7x-21*g6z-10*g6y-21*g5z-4*g5y-6*g5x+21*g4z+4*g4y-15*g3z-10*g3y-6*g3x+15*g2z+10*g2y+15*g1z+4*g1y+6*
g1x-15*g0z-4*g0y),-900*(g7z-g6z-g5z+g4z-g3z+g2z+g1z-g0z)),Array(-30*(7*g3y+2*g3x-7*g2y-5*g1y-2*g1x+5*g0y),60*
(g3z-g2z-g1z+g0z),0,-300*
(2*g7z+7*g7y+2*g7x-2*g6z-7*g6y-2*g5z-5*g5y-2*g5x+2*g4z+5*g4y-7*g3y-2*g3x+7*g2y+5*g1y+2*g1x-5*g0y),150*(10*g7z+21*g7y+6*g7x
-10*g6z-21*g6y-10*g5z-15*g5y-6*g5x+10*g4z+15*g4y-4*g3z-21*g3y-6*g3x+4*g2z+21*g2y+4*g1z+15*g1y+6*g1x-4*g0z-15*g0y),-180*
(7*g7z+7*g7y+2*g7x-7*g6z-7*g6y-7*g5z-5*g5y-2*g5x+7*g4z+5*g4y-5*g3z-7*g3y-2*g3x+5*g2z+7*g2y+5*g1z+5*g1y+2*g1x-5*g0z-5*g0y)
,360*(g7z-g6z-g5z+g4z-g3z+g2z+g1z-g0z)),Array(60*(g3y-g2y-g1y+g0y),0,0,600*(g7y-g6y-g5y+g4y-g3y+g2y+g1y-g0y),-900*
(g7y-g6y-g5y+g4y-g3y+g2y+g1y-g0y),360*(g7y-g6y-g5y+g4y-g3y+g2y+g1y-g0y),0)),Array(Array(5*(5*g1x-2*g0x),-15*(g1z-g0z),0,50*
(3*g5z+5*g5x-3*g4z-2*g4x-5*g1x+2*g0x),-75*(5*g5z+5*g5x-5*g4z-2*g4x-2*g1z-5*g1x+2*g0z+2*g0x),15*
(21*g5z+10*g5x-21*g4z-4*g4x-15*g1z-10*g1x+15*g0z+4*g0x),-90*(g5z-g4z-g1z+g0z)),Array(-15*(g1y-g0y),0,0,-150*(g5y-g4y-g1y+g0y),
225*(g5y-g4y-g1y+g0y),-90*(g5y-g4y-g1y+g0y),0),Array(0,0,0,0,0,0,0),Array(50*(3*g3y+5*g3x-3*g2y-2*g2x-5*g1x+2*g0x),-150*
(g3z-g2z-g1z+g0z),0,500*(3*g7z+3*g7y+5*g7x-3*g6z-3*g6y-2*g6x-3*g5z-5*g5x+3*g4z+2*g4x-3*g3y-5*g3x+3*g2y+2*g2x+5*g1x-2*g0x)
,-750*
(5*g7z+3*g7y+5*g7x-5*g6z-3*g6y-2*g6x-5*g5z-5*g5x+5*g4z+2*g4x-2*g3z-3*g3y-5*g3x+2*g2z+3*g2y+2*g2x+2*g1z+5*g1x-2*g0z-2*g0x)
,150*(21*g7z+6*g7y+10*g7x-21*g6z-6*g6y-4*g6x-21*g5z-10*g5x+21*g4z+4*g4x-15*g3z-6*g3y-10*g3x+15*g2z+6*g2y+4*g2x+15*g1z+10*
g1x-15*g0z-4*g0x),-900*(g7z-g6z-g5z+g4z-g3z+g2z+g1z-g0z)),Array(-75*(5*g3y+5*g3x-5*g2y-2*g2x-2*g1y-5*g1x+2*g0y+2*g0x),225*
(g3z-g2z-g1z+g0z),0,-750*
(3*g7z+5*g7y+5*g7x-3*g6z-5*g6y-2*g6x-3*g5z-2*g5y-5*g5x+3*g4z+2*g4y+2*g4x-5*g3y-5*g3x+5*g2y+2*g2x+2*g1y+5*g1x-2*g0y-2*g0x)
,1125*(5*g7z+5*g7y+5*g7x-5*g6z-5*g6y-2*g6x-5*g5z-2*g5y-5*g5x+5*g4z+2*g4y+2*g4x-2*g3z-5*g3y-5*g3x+2*g2z+5*g2y+2*g2x+2*g1z+
2*g1y+5*g1x-2*g0z-2*g0y-2*g0x),-225*(21*g7z+10*g7y+10*g7x-21*g6z-10*g6y-4*g6x-21*g5z-4*g5y-10*g5x+21*g4z+4*g4y+4*g4x-15*
g3z-10*g3y-10*g3x+15*g2z+10*g2y+4*g2x+15*g1z+4*g1y+10*g1x-15*g0z-4*g0y-4*g0x),1350*(g7z-g6z-g5z+g4z-g3z+g2z+g1z-g0z)),Array(
15*(21*g3y+10*g3x-21*g2y-4*g2x-15*g1y-10*g1x+15*g0y+4*g0x),-90*(g3z-g2z-g1z+g0z),0,150*(6*g7z+21*g7y+10*g7x-6*g6z-21*g6y-4
*g6x-6*g5z-15*g5y-10*g5x+6*g4z+15*g4y+4*g4x-21*g3y-10*g3x+21*g2y+4*g2x+15*g1y+10*g1x-15*g0y-4*g0x),-225*(10*g7z+21*g7y+10
*g7x-10*g6z-21*g6y-4*g6x-10*g5z-15*g5y-10*g5x+10*g4z+15*g4y+4*g4x-4*g3z-21*g3y-10*g3x+4*g2z+21*g2y+4*g2x+4*g1z+15*g1y+10*
g1x-4*g0z-15*g0y-4*g0x),90*(21*g7z+21*g7y+10*g7x-21*g6z-21*g6y-4*g6x-21*g5z-15*g5y-10*g5x+21*g4z+15*g4y+4*g4x-15*g3z-21*
g3y-10*g3x+15*g2z+21*g2y+4*g2x+15*g1z+15*g1y+10*g1x-15*g0z-15*g0y-4*g0x),-540*(g7z-g6z-g5z+g4z-g3z+g2z+g1z-g0z)),Array(-90*
(g3y-g2y-g1y+g0y),0,0,-900*(g7y-g6y-g5y+g4y-g3y+g2y+g1y-g0y),1350*(g7y-g6y-g5y+g4y-g3y+g2y+g1y-g0y),-540*
(g7y-g6y-g5y+g4y-g3y+g2y+g1y-g0y),0)),Array(Array(-3*(7*g1x-5*g0x),6*(g1z-g0z),0,-30*(2*g5z+7*g5x-2*g4z-5*g4x-7*g1x+5*g0x),15*
(10*g5z+21*g5x-10*g4z-15*g4x-4*g1z-21*g1x+4*g0z+15*g0x),-18*(7*g5z+7*g5x-7*g4z-5*g4x-5*g1z-7*g1x+5*g0z+5*g0x),36*
(g5z-g4z-g1z+g0z)),Array(6*(g1y-g0y),0,0,60*(g5y-g4y-g1y+g0y),-90*(g5y-g4y-g1y+g0y),36*(g5y-g4y-g1y+g0y),0),Array(0,0,0,0,0,0,0)
,Array(-30*(2*g3y+7*g3x-2*g2y-5*g2x-7*g1x+5*g0x),60*(g3z-g2z-g1z+g0z),0,-300*
(2*g7z+2*g7y+7*g7x-2*g6z-2*g6y-5*g6x-2*g5z-7*g5x+2*g4z+5*g4x-2*g3y-7*g3x+2*g2y+5*g2x+7*g1x-5*g0x),150*(10*g7z+6*g7y+21*g7x
-10*g6z-6*g6y-15*g6x-10*g5z-21*g5x+10*g4z+15*g4x-4*g3z-6*g3y-21*g3x+4*g2z+6*g2y+15*g2x+4*g1z+21*g1x-4*g0z-15*g0x),-180*
(7*g7z+2*g7y+7*g7x-7*g6z-2*g6y-5*g6x-7*g5z-7*g5x+7*g4z+5*g4x-5*g3z-2*g3y-7*g3x+5*g2z+2*g2y+5*g2x+5*g1z+7*g1x-5*g0z-5*g0x)
,360*(g7z-g6z-g5z+g4z-g3z+g2z+g1z-g0z)),Array(15*(10*g3y+21*g3x-10*g2y-15*g2x-4*g1y-21*g1x+4*g0y+15*g0x),-90*
(g3z-g2z-g1z+g0z),0,150*(6*g7z+10*g7y+21*g7x-6*g6z-10*g6y-15*g6x-6*g5z-4*g5y-21*g5x+6*g4z+4*g4y+15*g4x-10*g3y-21*g3x+10*
g2y+15*g2x+4*g1y+21*g1x-4*g0y-15*g0x),-225*(10*g7z+10*g7y+21*g7x-10*g6z-10*g6y-15*g6x-10*g5z-4*g5y-21*g5x+10*g4z+4*g4y+
15*g4x-4*g3z-10*g3y-21*g3x+4*g2z+10*g2y+15*g2x+4*g1z+4*g1y+21*g1x-4*g0z-4*g0y-15*g0x),90*(21*g7z+10*g7y+21*g7x-21*g6z-10*
g6y-15*g6x-21*g5z-4*g5y-21*g5x+21*g4z+4*g4y+15*g4x-15*g3z-10*g3y-21*g3x+15*g2z+10*g2y+15*g2x+15*g1z+4*g1y+21*g1x-15*g0z-
4*g0y-15*g0x),-540*(g7z-g6z-g5z+g4z-g3z+g2z+g1z-g0z)),Array(-18*(7*g3y+7*g3x-7*g2y-5*g2x-5*g1y-7*g1x+5*g0y+5*g0x),36*
(g3z-g2z-g1z+g0z),0,-180*
(2*g7z+7*g7y+7*g7x-2*g6z-7*g6y-5*g6x-2*g5z-5*g5y-7*g5x+2*g4z+5*g4y+5*g4x-7*g3y-7*g3x+7*g2y+5*g2x+5*g1y+7*g1x-5*g0y-5*g0x)
,90*(10*g7z+21*g7y+21*g7x-10*g6z-21*g6y-15*g6x-10*g5z-15*g5y-21*g5x+10*g4z+15*g4y+15*g4x-4*g3z-21*g3y-21*g3x+4*g2z+21*g2y
+15*g2x+4*g1z+15*g1y+21*g1x-4*g0z-15*g0y-15*g0x),-108*(7*g7z+7*g7y+7*g7x-7*g6z-7*g6y-5*g6x-7*g5z-5*g5y-7*g5x+7*g4z+5*g4y+
5*g4x-5*g3z-7*g3y-7*g3x+5*g2z+7*g2y+5*g2x+5*g1z+5*g1y+7*g1x-5*g0z-5*g0y-5*g0x),216*(g7z-g6z-g5z+g4z-g3z+g2z+g1z-g0z)),Array(
36*(g3y-g2y-g1y+g0y),0,0,360*(g7y-g6y-g5y+g4y-g3y+g2y+g1y-g0y),-540*(g7y-g6y-g5y+g4y-g3y+g2y+g1y-g0y),216*
(g7y-g6y-g5y+g4y-g3y+g2y+g1y-g0y),0)),Array(Array(6*(g1x-g0x),0,0,60*(g5x-g4x-g1x+g0x),-90*(g5x-g4x-g1x+g0x),36*
(g5x-g4x-g1x+g0x),0),Array(0,0,0,0,0,0,0),Array(0,0,0,0,0,0,0),Array(60*(g3x-g2x-g1x+g0x),0,0,600*(g7x-g6x-g5x+g4x-g3x+g2x+g1x-g0x)
,-900*(g7x-g6x-g5x+g4x-g3x+g2x+g1x-g0x),360*(g7x-g6x-g5x+g4x-g3x+g2x+g1x-g0x),0),Array(-90*(g3x-g2x-g1x+g0x),0,0,-900*
(g7x-g6x-g5x+g4x-g3x+g2x+g1x-g0x),1350*(g7x-g6x-g5x+g4x-g3x+g2x+g1x-g0x),-540*(g7x-g6x-g5x+g4x-g3x+g2x+g1x-g0x)
,0),Array(36*(g3x-g2x-g1x+g0x),0,0,360*(g7x-g6x-g5x+g4x-g3x+g2x+g1x-g0x),-540*(g7x-g6x-g5x+g4x-g3x+g2x+g1x-g0x),216*
(g7x-g6x-g5x+g4x-g3x+g2x+g1x-g0x),0),Array(0,0,0,0,0,0,0)))
)

		// Calculate Matrix B_ij = if(i <= j):  (j over i) * x0^(j-i) * x1^i else 0
		val B = Tensor(7,7)
		for( i <- 0 to 6; j <- 0 to 6 )
			B(i,j) = if(i <= j) binomial(j,i) * pow(x0,j-i) * pow(x1,i) else 0
	
	
		// Calculate Matrix C_ij = if(i <  j):  0 else (i over j)*y0^(i-j)*y1^j
		val C = Tensor(7,7)
		for( i <- 0 to 6; j <- 0 to 6 )
			C(i,j) = if(i < j) 0 else binomial(i,j) * pow(y0,i-j) * pow(y1,j)
	
	
		// Calculate Matrix D_ij = if(i <= j):  (j over i) * z0^(j-i) * z1^i else 0
		val D = Tensor(7,7)
		for( i <- 0 to 6; j <- 0 to 6 )
			D(i,j) = if(i <= j) binomial(j,i) * pow(z0,j-i) * pow(z1,i) else 0
	
		// Calculate Tensor G = B *_x ( D *_z A) *_y C
		val G = B prodx ( D prodz A) prody C

		// Calculate resulting Interval [f0,f1]
		val f0 = G(0,0,0) + sum(1,6, k => if( isEven(k) ) min(0,G(0,0,k)) else -abs(G(0,0,k)) ) +
			sum(1,6, j => sum(0,6,k => if(isEven(j) && isEven(k)) min(0,G(0,j,k)) else -abs(G(0,j,k)) )) +
			sum(1,6, i => sum(0,6,j => sum(0,6,k => if(isEven(i) && isEven(j) && isEven(k)) min(0,G(i,j,k)) else -abs(G(i,j,k)) )))

		val f1 = G(0,0,0) + sum(1,6, k => if( isEven(k) ) max(0,G(0,0,k)) else abs(G(0,0,k)) ) +
			sum(1,6, j => sum(0,6,k => if(isEven(j) && isEven(k)) max(0,G(0,j,k)) else abs(G(0,j,k)) )) +
			sum(1,6, i => sum(0,6,j => sum(0,6,k => if(isEven(i) && isEven(j) && isEven(k)) max(0,G(i,j,k)) else abs(G(i,j,k)) )))

		Interval(max(f0,-1), min(f1,1))
	}
	
///////////////////////////////////////////////	
	
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
