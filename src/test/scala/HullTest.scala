import org.scalatest.FunSuite

import openworld._
import simplex3d.math.double._
import scala.collection.mutable.Stack

class ConvexHullTest extends FunSuite {
	
	test("test1"){
		val data       = Vector( Vec2(0,3), Vec2(2,3), Vec2(3,1), Vec2(2,1) )
		val dataSorted = Vector( Vec2(0,3), Vec2(2,1), Vec2(2,3), Vec2(3,1) )
		val hull       =  Stack( Vec2(2,3), Vec2(3,1), Vec2(2,1), Vec2(0,3) )
		
		assert( dataSorted === data.sortWith(ChainHull2D.compare))
		assert( hull       === ChainHull2D(data) )
	}
	
	test("test2"){
		val data       = Vector( Vec2(0,3), Vec2(1,0), Vec2(2,1), Vec2(3,0), Vec2(4,3) )
		val hull       =  Stack(Vec2(4.0f, 3.0), Vec2(3.0, 0.0), Vec2(1.0, 0.0), Vec2(0.0, 3.0))
		assert( hull === ChainHull2D(data) )
	}
	
	test("test3"){
		val data = Vector( Vec2(0,0), Vec2(0,1), Vec2(0,2), Vec2(0,3) )
		val hull = Stack( Vec2(0,3), Vec2(0,0) )
		assert( hull === ChainHull2D(data) )
	}
	
	/*
	test("test4") {
		val data   = Vector(Vec2(0.55752814f, -0.3837424f), Vec2(0.7032331f, 0.39506763f), Vec2(1.4141924f, -0.82710135f), Vec2(0.0f, -0.8313947f))
		val hull   = Util.convexHull2d(data.toList)
		val result = ChainHull2D(data)
		assert(hull.toSet === result.toSet)
	}
	*/
}
