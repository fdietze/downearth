
import xöpäx._

import org.scalatest.FunSuite

import simplex3d.math.float._

class QuckHullTest extends FunSuite{
	val data = Array(Vec2(0.0f, 0.0f), Vec2(1.0f, 0.0f), Vec2(1.0f, 1.0f), Vec2(1.0f, 0.0f), Vec2(0.25f, 1.0f), Vec2(0.25f, 1.0f), Vec2(1.0f, 1.0f), Vec2(1.0f, 1.0f), Vec2(0.0f, 0.0f))
	test("QuickHullTest"){
		val result = QuickHull.computeHull(data)
		assert(result === 4)
		assert(data.view(0,result).toSet === Set(Vec2(0,0),Vec2(1,0),Vec2(1,1),Vec2(0.25f,1)))
	}
}

