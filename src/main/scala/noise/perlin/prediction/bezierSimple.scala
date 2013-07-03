package noise.perlin.prediction

import interval.{functions, Interval3, Interval}
import simplex3d.math.double.Vec3
import scala.Array
import simplex3d.math.doublex.functions._
import interval.{functions => intervalfunctions}
import noise.perlin._

object bezierSimple {
  def apply(range:Interval3):Interval = {
    import range.x.{low => x0, high => x1}
    import range.y.{low => y0, high => y1}
    import range.z.{low => z0, high => z1}
    apply(x0,y0,z0,x1,y1,z1)
  }

  def apply(x0:Double,y0:Double,z0:Double,x1:Double,y1:Double,z1:Double):Interval = {
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
      return intervalfunctions.hull(
        apply(Interval3(Interval(x0,fastfloor(x0)+1),Interval(y0,y1),Interval(z0,z1))),
        apply(Interval3(Interval(fastceil(x1)-1,x1),Interval(y0,y1),Interval(z0,z1)))
      )

    if( fastceil(y1) - Y > 1 )
      return intervalfunctions.hull(
        apply(Interval3(Interval(x0,x1), Interval(y0,fastfloor(y0)+1),Interval(z0,z1))),
        apply(Interval3(Interval(x0,x1), Interval(fastceil(y1)-1,y1),Interval(z0,z1)))
      )

    if( fastceil(z1) - Z > 1 )
      return intervalfunctions.hull(
        apply(Interval3(Interval(x0,x1),Interval(y0,y1),Interval(z0,fastfloor(z0)+1))),
        apply(Interval3(Interval(x0,x1),Interval(y0,y1),Interval(fastceil(z1)-1,z1)))
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
    val Vec3(g0x,g0y,g0z) = gradientAt3(X  ,Y  ,Z  )
    val Vec3(g1x,g1y,g1z) = gradientAt3(X+1,Y  ,Z  )
    val Vec3(g2x,g2y,g2z) = gradientAt3(X  ,Y+1,Z  )
    val Vec3(g3x,g3y,g3z) = gradientAt3(X+1,Y+1,Z  )
    val Vec3(g4x,g4y,g4z) = gradientAt3(X  ,Y  ,Z+1)
    val Vec3(g5x,g5y,g5z) = gradientAt3(X+1,Y  ,Z+1)
    val Vec3(g6x,g6y,g6z) = gradientAt3(X  ,Y+1,Z+1)
    val Vec3(g7x,g7y,g7z) = gradientAt3(X+1,Y+1,Z+1)

    // Calculate the heights of the bezier curve, converted from the 3d perlin noise polynomial with fadeSimple-function of degree 3
    // resulting polynomial has degree 4. This gives 5^3 Bezier points
    //TODO: use flat array
    val bezierheights =
      Array(Array(Array(0,g0z*0.25,-(g4z-g0z)*0.5,-g4z*0.25,0),Array(g0y*0.25,(g0z+g0y)*0.25,-(4*g4z-g4y-4*g0z-g0y)*0.125,-(g4z-g4y)*0.25,g4y*0.25),Array(-
        (g2y-g0y)*0.5,(g2z-4*g2y+g0z+4*g0y)*0.125,-(g6z+g6y+g4z-g4y-g2z+g2y-g0z-g0y)*0.25,-(g6z+4*g6y+g4z-4*g4y)*0.125,-
        (g6y-g4y)*0.5),Array(-g2y*0.25,(g2z-g2y)*0.25,-(4*g6z+g6y-4*g2z+g2y)*0.125,-(g6z+g6y)*0.25,-g6y*0.25),Array(0,g2z*0.25,-(g6z-g2z)*0.5,-g6z*0.25,0)),Array(
        Array(g0x*0.25,(g0z+g0x)*0.25,-(4*g4z-g4x-4*g0z-g0x)*0.125,-(g4z-g4x)*0.25,g4x*0.25),Array((g0y+g0x)*0.25,(g0z+g0y+g0x)*0.25,-
          (4*g4z-g4y-g4x-4*g0z-g0y-g0x)*0.125,-(g4z-g4y-g4x)*0.25,(g4y+g4x)*0.25),Array(-(4*g2y-g2x-4*g0y-g0x)*0.125,
          (g2z-4*g2y+g2x+g0z+4*g0y+g0x)*0.125,-(4*g6z+4*g6y-g6x+4*g4z-4*g4y-g4x-4*g2z+4*g2y-g2x-4*g0z-4*g0y-g0x)*0.0625,-
            (g6z+4*g6y-g6x+g4z-4*g4y-g4x)*0.125,-(4*g6y-g6x-4*g4y-g4x)*0.125),Array(-(g2y-g2x)*0.25,(g2z-g2y+g2x)*0.25,-
          (4*g6z+g6y-g6x-4*g2z+g2y-g2x)*0.125,-(g6z+g6y-g6x)*0.25,-(g6y-g6x)*0.25),Array(g2x*0.25,(g2z+g2x)*0.25,-(4*g6z-g6x-4*g2z-g2x)*0.125,-
          (g6z-g6x)*0.25,g6x*0.25)),Array(Array(-(g1x-g0x)*0.5,(g1z-4*g1x+g0z+4*g0x)*0.125,-(g5z+g5x+g4z-g4x-g1z+g1x-g0z-g0x)*0.25,-
        (g5z+4*g5x+g4z-4*g4x)*0.125,-(g5x-g4x)*0.5),Array((g1y-4*g1x+g0y+4*g0x)*0.125,(g1z+g1y-4*g1x+g0z+g0y+4*g0x)*0.125,-
        (4*g5z-g5y+4*g5x+4*g4z-g4y-4*g4x-4*g1z-g1y+4*g1x-4*g0z-g0y-4*g0x)*0.0625,-(g5z-g5y+4*g5x+g4z-g4y-4*g4x)*0.125,
        (g5y-4*g5x+g4y+4*g4x)*0.125),Array(-(g3y+g3x+g2y-g2x-g1y+g1x-g0y-g0x)*0.25,
        (g3z-4*g3y-4*g3x+g2z-4*g2y+4*g2x+g1z+4*g1y-4*g1x+g0z+4*g0y+4*g0x)*0.0625,-(g7z+g7y+g7x+g6z+g6y-g6x
          +g5z-g5y+g5x+g4z-g4y-g4x-g3z+g3y+g3x-g2z+g2y-g2x-g1z-g1y+g1x-g0z-g0y-g0x)*0.125,-
          (g7z+4*g7y+4*g7x+g6z+4*g6y-4*g6x+g5z-4*g5y+4*g5x+g4z-4*g4y-4*g4x)*0.0625,-
          (g7y+g7x+g6y-g6x-g5y+g5x-g4y-g4x)*0.25),Array(-(g3y+4*g3x+g2y-4*g2x)*0.125,(g3z-g3y-4*g3x+g2z-g2y+4*g2x)*0.125,-
        (4*g7z+g7y+4*g7x+4*g6z+g6y-4*g6x-4*g3z+g3y+4*g3x-4*g2z+g2y-4*g2x)*0.0625,-(g7z+g7y+4*g7x+g6z+g6y-4*g6x)*0.125,
        -(g7y+4*g7x+g6y-4*g6x)*0.125),Array(-(g3x-g2x)*0.5,(g3z-4*g3x+g2z+4*g2x)*0.125,-(g7z+g7x+g6z-g6x-g3z+g3x-g2z-g2x)*0.25,-
        (g7z+4*g7x+g6z-4*g6x)*0.125,-(g7x-g6x)*0.5)),Array(Array(-g1x*0.25,(g1z-g1x)*0.25,-(4*g5z+g5x-4*g1z+g1x)*0.125,-(g5z+g5x)*0.25,-g5x*0.25),Array(
        (g1y-g1x)*0.25,(g1z+g1y-g1x)*0.25,-(4*g5z-g5y+g5x-4*g1z-g1y+g1x)*0.125,-(g5z-g5y+g5x)*0.25,(g5y-g5x)*0.25),Array(-
        (4*g3y+g3x-4*g1y+g1x)*0.125,(g3z-4*g3y-g3x+g1z+4*g1y-g1x)*0.125,-
        (4*g7z+4*g7y+g7x+4*g5z-4*g5y+g5x-4*g3z+4*g3y+g3x-4*g1z-4*g1y+g1x)*0.0625,-(g7z+4*g7y+g7x+g5z-4*g5y+g5x)*0.125,
        -(4*g7y+g7x-4*g5y+g5x)*0.125),Array(-(g3y+g3x)*0.25,(g3z-g3y-g3x)*0.25,-(4*g7z+g7y+g7x-4*g3z+g3y+g3x)*0.125,-(g7z+g7y+g7x)*0.25,-
        (g7y+g7x)*0.25),Array(-g3x*0.25,(g3z-g3x)*0.25,-(4*g7z+g7x-4*g3z+g3x)*0.125,-(g7z+g7x)*0.25,-g7x*0.25)),Array(Array(0,g1z*0.25,-(g5z-g1z)*0.5,-g5z*0.25,0),
        Array(g1y*0.25,(g1z+g1y)*0.25,-(4*g5z-g5y-4*g1z-g1y)*0.125,-(g5z-g5y)*0.25,g5y*0.25),Array(-(g3y-g1y)*0.5,(g3z-4*g3y+g1z+4*g1y)*0.125,-
          (g7z+g7y+g5z-g5y-g3z+g3y-g1z-g1y)*0.25,-(g7z+4*g7y+g5z-4*g5y)*0.125,-(g7y-g5y)*0.5),Array(-g3y*0.25,(g3z-g3y)*0.25,-
          (4*g7z+g7y-4*g3z+g3y)*0.125,-(g7z+g7y)*0.25,-g7y*0.25),Array(0,g3z*0.25,-(g7z-g3z)*0.5,-g7z*0.25,0)))


    val n = 5

    var i = 0; var j = 0; var k = 0; var p = 0
    var points =  Array.ofDim[Double](n)

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

    Interval(max(imin,-1.036353811211803), min(imax,1.036353811211803))
  }
}

