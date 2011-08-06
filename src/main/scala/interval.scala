package noise

package object intervals {

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

def intervalmin(a:Interval,b:Interval) = Interval(functions.min(a.low,b.low), functions.min(a.high, b.high))
def intervalmax(a:Interval,b:Interval) = Interval(functions.max(a.low,b.low), functions.max(a.high, b.high))

case class Interval (a:Double = scala.Double.NegativeInfinity, b:Double = scala.Double.PositiveInfinity) {
	def low = min(a,b)
	def high = max(a,b)

	def isPositive = low >= 0 
	def isNegative = high <= 0
	def apply(value:Double) = low <= value && value <= high
	
	def + (that:Interval) = Interval(this.low + that.low, this.high + that.high)
	def - (that:Interval) = Interval(this.low - that.low, this.high - that.high)
	def * (that:Interval) = 
		//TODO: More simple cases possible
		if( this.low >= 0 && that.low >= 0 )
			Interval(this.low * that.low, this.high * that.high)
		else
			Interval(
				functions.min(functions.min(this.low * that.low, this.low * that.high), functions.min(this.high * that.low, this.high * that.high)),
				functions.max(functions.max(this.low * that.low, this.low * that.high), functions.max(this.high * that.low, this.high * that.high))
				)
	
	def / (that:Interval) = {
		assert( !that(0) ) //TODO: return infinity
		this * Interval(1 / that.low, 1 / that.high)
	}

	def + (that:Double) = Interval(this.low + that, this.high + that)
	def - (that:Double) = Interval(this.low - that, this.high - that)
	def * (that:Double) = Interval(this.low * that, this.high * that)
	def / (that:Double) = Interval(this.low / that, this.high / that)
	
	override def toString = "Interval("+low+","+high+")"
}

case class Volume(x:Interval = Interval(), y:Interval = Interval(), z:Interval = Interval()) {
	def low  = Vec3(x.low , y.low , z.low )
	def high = Vec3(x.high, y.high, z.high)
	
	def + (that:Volume) = Volume(this.x + that.x, this.y + that.y, this.z + that.z)
	def - (that:Volume) = Volume(this.x - that.x, this.y - that.y, this.z - that.z)
	def * (that:Volume) = Volume(this.x * that.x, this.y * that.y, this.z * that.z)
	def / (that:Volume) = Volume(this.x / that.x, this.y / that.y, this.z / that.z)

	def + (that:Double) = Volume(this.x + that, this.y + that, this.z + that)
	def - (that:Double) = Volume(this.x - that, this.y - that, this.z - that)
	def * (that:Double) = Volume(this.x * that, this.y * that, this.z * that)
	def / (that:Double) = Volume(this.x / that, this.y / that, this.z / that)
}

object Volume {
	def apply(v1:Vec3, v2:Vec3) {
		Volume(
			Interval(v1.x, v2.x),
			Interval(v1.y, v2.y),
			Interval(v1.z, v2.z)
		)
	}
}

}
