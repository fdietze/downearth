package openworld

//===================================================================
// chainHull_2D(): Andrew's monotone chain 2D convex hull algorithm
//     Input:  P[] = an array of 2D points
//                   presorted by increasing x- and y-coordinates
//             n = the number of points in P[]
//     Output: H[] = an array of the convex hull vertices (max is n)
//     Return: the number of points in H[]

import simplex3d.math.float._
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
		while( minmax < n-1 && verts(minmax+1) == xmin )
			minmax += 1
		
		val maxmax = n-1
		var maxmin = n-1
		while( maxmin > 1 && verts(minmax+1) == xmin ){
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
				if( !( isLeft( verts(minmin), verts(maxmin), verts(i) ) >= 0 && i < maxmin ) ) {
					while ( stack.size > 1 && isLeft( stack(1), stack(0), verts(i) ) <= 0 )
						stack.pop
					stack push verts(i)
				}
			}
			
			if (maxmax != maxmin)
				stack push verts(maxmax)
			
			val bottom = stack.size
			
			// Next, compute the upper hull on the stack H above the bottom hull
			for(i ← (maxmin-1).until(minmax,-1) ) {
				// the upper line joins P[maxmax] with P[minmax]
				if( !(isLeft( verts(maxmax), verts(minmax), verts(i) ) >= 0 && i > minmax) ) {
					// ignore P[i] below or on the upper line
					while ( stack.size > bottom && isLeft( stack(1), stack(0), verts(i) ) <= 0 )
						stack.pop
					stack push verts(i)
				}
			}
			
			if (minmax != minmin)
				stack push verts(minmax) // push joining endpoint onto stack
				
			stack
		}
	}
	
	/*
	def apply( in: IndexedSeq[Vec2]):Vector[Vec2] = {
		// muss noch sortiert werden
		val P = in sortWith compare
		
		// the output array H[] will be used as the stack
		val n = P.size
		val H = new Array[Vec2](n)
		var top = -1
		var i = 1
		
		// Get the indices of points with min x-coord and min|max y-coord
		val minmin = 0
		var xmin = P(0).x;
		while( (P(i).x == xmin) && i < n )
			i += 1
		
		val minmax = i-1;
		if (minmax == n-1) {// degenerate case: all x-coords == xmin
			top += 1
			H(top) = P(minmin)
			if (P(minmax).y != P(minmin).y){ // a nontrivial segment
				top += 1
				H(top) = P(minmax);
			}
			top += 1
			H(top) = P(minmin);// add polygon endpoint
			return Vector(H.view(0,top+1):_*)
		}
		
		// Get the indices of points with max x-coord and min|max y-coord
		val maxmax = n-1;
		val xmax = P(n-1).x;
		i=n-2
		while( i>=0 && P(i).x == xmax)
		    i -= 1
		val maxmin = i+1;
		
		// Compute the lower hull on the stack H
		top += 1
		H(top) = P(minmin);      // push minmin point onto stack
		i = minmax+1;
		while (i <= maxmin)
		{
			// the lower line joins P[minmin] with P[maxmin]
			// ignore P[i] above or on the lower line
			if( !( isLeft( P(minmin), P(maxmin), P(i) ) >= 0 && i < maxmin ) ) {
				while (top > 0 && isLeft( H(top-1), H(top), P(i)) <= 0 )        // there are at least 2 points on the stack
				{
					// test if P[i] is left of the line at the stack top
					// P[i] is a new hull vertex
					top -= 1; // pop top point off stack
				}
				top += 1
				H(top) = P(i);       // push P[i] onto stack
			}
			i += 1
		}
		
		// Next, compute the upper hull on the stack H above the bottom hull
		if (maxmax != maxmin){// if distinct xmax points
			top += 1
			H(top) = P(maxmax)
		}// push maxmax point onto stack
		
		val bot = top                 // the bottom point of the upper hull stack
		i = maxmin-1
		
		while (i > minmax)
		{
			// the upper line joins P[maxmax] with P[minmax]
			if( !(isLeft( P(maxmax), P(minmax), P(i) ) >= 0 && i > minmax) ) {
				// ignore P[i] below or on the upper line
				while (top > bot && isLeft( H(top-1), H(top), P(i)) <= 0) {// at least 2 points on the upper stack 
					top -= 1;// pop top point off stack
				}
				top += 1
				H(top) = 
					P(i);       // push P[i] onto stack
			}
			i -= 1
		}
		if (minmax != minmin){
			top += 1
			H(top) = P(minmax);  // push joining endpoint onto stack
		}
		
		return Vector(H.view(0,top+1):_*)
	}
	*/
}
