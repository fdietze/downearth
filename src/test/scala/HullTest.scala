import org.scalatest.FunSuite

import openworld._
import simplex3d.math.float._

class ExampleSuite extends FunSuite {
	test("Convex Hull Test"){
		val hull1 = ChainHull2D( Vector( Vec2(0,3),Vec2(2,3),Vec2(2,1),Vec2(3,1) ) )
		assert( hull1 === Vector( Vec2(0,3),Vec2(2,1),Vec2(3,1),Vec2(2,3) ) )
		
		val hull2 = ChainHull2D( Vector( Vec2(0,3),Vec2(2,3),Vec2(2,2),Vec2(3,1) ) )
		assert( hull2 === Vector( Vec2(0,3),Vec2(3,1),Vec2(2,3) ) )
	}
}
