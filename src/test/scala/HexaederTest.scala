    
    
import xöpäx._

import org.scalatest.FunSuite

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

class HexaederTest extends FunSuite{
	/*test("hexaeder tests") {
		
		val h = new Hexaeder

		def left (data:Int) = (data & 0xFF) >> 4
		def right(data:Int) = data & 0x0F
		def readOdd (n:Int) = Vec3i(right(h.data(n+1)), left(h.data(n+1)), right(h.data(n)))
		def readEven(n:Int) = Vec3i(left(h.data(n+1)),  right(h.data(n)),  left(h.data(n)))
		def readComponentEven(n:Int, a:Int) = a match{ case 0 => left(n+1); case 1 => right(n); case 2 => left(n); case _ => throw new IndexOutOfBoundsException}
		def readComponentOdd (n:Int, a:Int) = a match{ case 0 => right(n+1); case 1 => left(n+1); case 2 => right(n); case _ => throw new IndexOutOfBoundsException}
		
		
		for( i <- 0 until 256 ) yield {
			assert(h.readNibble(i.toByte,0) === right(i))
			assert(h.readNibble(i.toByte,1) === left(i))
		}
		
		for( p <- 0 until 8 ) {
			assert( h.readVertex(p) === (if( (p&1) == 1 ) readEven(p/2*3) else readOdd(p/2*3+1)) )
		}
	}*/
	
	test("noch n hexaedertest"){
		val h1 = new PartialHexaeder
		val h2 = new PartialHexaeder
		val h3 = new PartialHexaeder
		
		for( i <- 0 until 8 ){
			h3(i) = h2(i)
		}
		
		assert( h2 === h3 )

		h2(0) = Vec3(0,0,0)
		h2(1) = Vec3(1,0,0)
		h2(2) = Vec3(0,1,0)
		h2(3) = Vec3(1,1,0)
		h2(4) = Vec3(0,0,1)
		h2(5) = Vec3(1,0,1)
		h2(6) = Vec3(0,1,1)
		h2(7) = Vec3(1,1,1)
		
		assert( h1 === h2 )
		
	}
}


























