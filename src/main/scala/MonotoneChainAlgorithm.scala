package openworld

//===================================================================
// chainHull_2D(): Andrew's monotone chain 2D convex hull algorithm
//     Input:  P[] = an array of 2D points
//                   presorted by increasing x- and y-coordinates
//             n = the number of points in P[]
//     Output: H[] = an array of the convex hull vertices (max is n)
//     Return: the number of points in H[]

import simplex3d.math.double._
import collection.mutable.Stack

object ChainHull2D {
	@inline def isLeft(p0:Vec2,p1:Vec2,p2:Vec2) = (p1.x - p0.x)*(p2.y - p0.y) - (p2.x - p0.x)*(p1.y - p0.y)
	val compare = (v1:Vec2,v2:Vec2) => if( v1.x == v2.x ) (v1.y < v2.y) else (v1.x < v2.x)
	def apply( in: IndexedSeq[Vec2]):Stack[Vec2] = {
		val verts = in sortWith compare
		val n = verts.size
		
		val minmin = 0
		val xmin = verts(minmin).x
		var minmax = 0
		while( minmax < n-1 && verts(minmax+1).x == xmin )
			minmax += 1
		
		val maxmax = n-1
		val xmax = verts(maxmax).x
		var maxmin = n-1
		while( maxmin > 1 && verts(maxmin-1).x == xmax ){
			maxmin -= 1
		}
		
		val stack = new Stack[Vec2]()
		
		if (minmax == n-1) {// degenerate case: all x-coords == xmin
			stack push verts(minmin)
			if( verts(minmin).y != verts(minmax).y )
				stack push verts(minmax) // a nontrivial segment
			stack
		}
		else {
			// Compute the lower hull on the stack H
			stack push verts(minmin)
			
			for(i ← minmax+1 to maxmin) {
				// the lower line joins P[minmin] with P[maxmin]
				// ignore P[i] above or on the lower line
				if( isLeft( verts(minmin), verts(maxmin), verts(i) ) < 0 || i == maxmin ) {
					while ( stack.size > 1 && isLeft( stack(1), stack(0), verts(i) ) <= 0 )
						stack.pop
					stack push verts(i)
				}
			}
			
			if (maxmax != maxmin)
				stack push verts(maxmax)
			
			val bottom = stack.size
			
			// Next, compute the upper hull on the stack H above the bottom hull
			for(i ← (maxmin-1).to(minmax,-1) ) {
				// the upper line joins P[maxmax] with P[minmax]
				if( isLeft( verts(maxmax), verts(minmax), verts(i) ) < 0 || i == minmax ) {
					// ignore P[i] below or on the upper line
					while ( stack.size > bottom && isLeft( stack(1), stack(0), verts(i) ) <= 0 )
						stack.pop
					stack push verts(i)
				}
			}
			
			// push joining endpoint onto stack
			if (minmax == minmin)
				stack.pop
				
			stack
		}
	}
}

