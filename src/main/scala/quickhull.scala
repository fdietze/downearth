import simplex3d.math.double.Vec2

class QuickHull
{
	case class Line(p0:Vec2,p1:Vec2){
		//TODO implementieren
		def isRight(p:Vec2) = false
	}

	var p = Array[Vec2]()
	var n = 0
	var h = 0
	val eps=1e-3

	def computeHull( p_ : Array[Vec2] ):Int = {
		p=p_
		n=p.length
		h=0
		quickHull
		h
	}

	def quickHull {
		exchange(0, indexOfLowestPoint)
		h += 1
		val g = new Line(p(0), p(0)-Vec2(eps,0))
		computeHullPoints(g, 1, n-1)
	}

	def computeHullPoints(g:Line, lo:Int, hi:Int)
	{
		if (lo>hi) 
			return

		val k = indexOfFurthestPoint(g, lo, hi)
		val g0 = new Line(g.p0, p(k))
		val g1 = new Line(p(k), g.p1)
		exchange(k, hi);

		val i = partition(g0, lo, hi-1)
		// alle Punkte von lo bis i-1 liegen rechts von g0
		// alle Punkte von i bis hi-1 liegen links von g0
		computeHullPoints(g0, lo, i-1)

		// alle eben rekursiv erzeugten Punkte liegen
		// auf dem Hüllpolygonzug vor p[hi]
		exchange(hi, i)
		exchange(i, h)
		h += 1

		val j = partition(g1, i+1, hi)
		// alle Punkte von i+1 bis j-1 liegen rechts von g1,
		// alle Punkte von j bis hi liegen im Inneren
		computeHullPoints(g1, i+1, j-1)
	}

	def indexOfLowestPoint:Int = {
		var min = 0;
		
		for(i <- 1 until n)
			if (p(i).y < p(min).y || p(i).y==p(min).y && p(i).x < p(min).x)
				min=i;
		return min;
	}

	def exchange( i:Int, j:Int )
	{
		val t=p(i)
		p(i)=p(j)
		p(j)=t
	}
	
	//TODO implementieren
	def area(p:Vec2,l:Line) = 0.0

	def indexOfFurthestPoint(g:Line, lo:Int, hi:Int):Int =
	{
		var i = lo
		var f = lo

		var d  = 0.0
		var mx = 0.0
		
		for( i <- lo until hi )
		{
			d = -area(p(i),g)
			if (d>mx || d == mx && p(i).x > p(f).x )
			{
				mx = d
				f = i
			}
		}
		
		f
	}

	def partition( g:Line, lo:Int, hi:Int):Int = 
	{
		var i=lo
		var j=hi
		while (i<=j)
		{
			while ( i <= j &&  (g isRight p(i)) )
				i += 1;
			while ( i <= j && ! (g isRight p(j)) )
				j -= 1;
			if (i<=j){
				exchange(i, j)
				i += 1
				j -= 1
			}
		}
		return i;
	}
}   // end class QuickHull
