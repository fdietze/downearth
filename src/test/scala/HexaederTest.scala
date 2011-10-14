import org.scalatest.FunSuite

import openworld._
import simplex3d.math._
import simplex3d.math.float._
import scala.collection.mutable.Stack

class HexaederTest extends FunSuite {
	test("test1"){
		val pos1 = Vec3i(7, -7, -5)
		val hex1 = new PartialHexaeder(0x80808080, 0x88008800, 0x88860000)
		val pos2 = Vec3i(7, -7, -4)
		val hex2 = new PartialHexaeder(0x80808080, 0x88088808, 0x80000000)
		
		println(hex1.vertices)
		println(hex2.vertices)
	}
}
