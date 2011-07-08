

import xöpäx._

import org.scalatest.FunSuite

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

class CollisionTest extends FunSuite{
	test("sphere hexaeder intersection") {
		val h = new Hexaeder
		val hpos = Vec3i(0)
		val radius = 1
		val spos = Vec3(1.5f)
		
		//println(h.vertices.mkString(" "))
		
		assert(Collision.hexaedersperetest(hpos,h,spos,1.5f) === true)
		assert(Collision.hexaedersperetest(hpos,h,spos,0.6f) === false)
	}
}

