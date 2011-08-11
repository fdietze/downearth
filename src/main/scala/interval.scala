package noise

import simplex3d.math._
import simplex3d.math.double._

package object interval {

case class Interval (low:Double = 0.0, high:Double = 0.0) {
	assert(low <= high, "Invalid Interval: ["+low+", "+high+"], low > high")

	def isPositive = low >= 0 
	def isNegative = high <= 0
	def apply(value:Double) = low <= value && value <= high
	
	// -Interval
	def unary_- = Interval(-high, -low)
	
	// Interval <op> Interval
	def + (that:Interval) = Interval(this.low + that.low,  this.high + that.high)
	def - (that:Interval) = Interval(this.low - that.high, this.high - that.low )
	def * (that:Interval) = 
		//TODO: More simple cases possible
		if( this.isPositive && that.isPositive )
			Interval(this.low * that.low, this.high * that.high)
		else
			Interval(
				functions.min(functions.min(this.low * that.low, this.low * that.high), functions.min(this.high * that.low, this.high * that.high)),
				functions.max(functions.max(this.low * that.low, this.low * that.high), functions.max(this.high * that.low, this.high * that.high))
				)
	
	def / (that:Interval) = {
		if( that(0) )
			Interval(scala.Double.NegativeInfinity, scala.Double.PositiveInfinity)
		else
			this * Interval(1 / that.high, 1 / that.low)
	}

	// Interval <op> Scalar
	def + (that:Double) = Interval(this.low + that, this.high + that)
	def - (that:Double) = Interval(this.low - that, this.high - that)
	def * (that:Double) = if(that >= 0) Interval(this.low * that, this.high * that) else Interval(this.high * that, this.low * that)
	def / (that:Double) = if(that > 0) Interval(this.low / that, this.high / that) else Interval(this.high / that, this.low / that)
	
	override def toString = "Interval("+low+","+high+")"
}

case class Volume(x:Interval = Interval(), y:Interval = Interval(), z:Interval = Interval()) {
	def low  = Vec3(x.low , y.low , z.low )
	def high = Vec3(x.high, y.high, z.high)

	def apply(v:Vec3) = x(v.x) && y(v.y) && z(v.z)
	
	// -Volume
	def unary_- = Volume(-x, -y, -z)

	// Volume <op> Volume
	def + (that:Volume) = Volume(this.x + that.x, this.y + that.y, this.z + that.z)
	def - (that:Volume) = Volume(this.x - that.x, this.y - that.y, this.z - that.z)
	def * (that:Volume) = Volume(this.x * that.x, this.y * that.y, this.z * that.z)
	def / (that:Volume) = Volume(this.x / that.x, this.y / that.y, this.z / that.z)

	// Volume <op> Scalar
	def + (that:Double) = Volume(this.x + that, this.y + that, this.z + that)
	def - (that:Double) = Volume(this.x - that, this.y - that, this.z - that)
	def * (that:Double) = Volume(this.x * that, this.y * that, this.z * that)
	def / (that:Double) = Volume(this.x / that, this.y / that, this.z / that)
}

/*
// Scalar <op> Interval
implicit def scalarplusinterval(value:Double)  = new { def + (i:Interval) = i + value }
implicit def scalarminusinterval(value:Double) = new { def - (i:Interval) = -i + value }
implicit def scalartimesinterval(value:Double) = new { def * (i:Interval) = i * value }
implicit def scalaroverinterval(value:Double)  = new { def / (i:Interval) = Interval(value) / i }

// Scalar <op> Volume
implicit def scalarplusvolume(value:Double)  = new { def + (i:Volume) = i + value }
implicit def scalarminusvolume(value:Double) = new { def - (i:Volume) = -i + value }
implicit def scalartimesvolume(value:Double) = new { def * (i:Volume) = i * value }
implicit def scalarovervolume(value:Double)  = new { def / (i:Volume) = Volume(value) / i }
*/

// math functions on Intervals/Volumes
def min(a:Interval,b:Interval) = Interval(functions.min(a.low,b.low), functions.min(a.high, b.high))
def max(a:Interval,b:Interval) = Interval(functions.max(a.low,b.low), functions.max(a.high, b.high))
def dot(a:Volume, b:Volume) = a.x*b.x + a.y*b.y + a.z*b.z
def sqrt(i:Interval) = Interval(functions.sqrt(i.low), functions.sqrt(i.high))
def square(i:Interval) = {
	if(i.isPositive)
		Interval(i.low*i.low, i.high*i.high)
	else if( i.high < 0 )
		Interval(i.high*i.high, i.low*i.low)
	else
		Interval(0, functions.max(i.low*i.low, i.high*i.high))
}
def length(v:Volume) = sqrt(square(v.x) + square(v.y) + square(v.z))

// Factories
object Interval {
	def apply(value:Double):Interval = Interval(value,value)
}

object Volume {
	def apply(v1:Vec3, v2:Vec3):Volume = {
		Volume(
			Interval(v1.x, v2.x),
			Interval(v1.y, v2.y),
			Interval(v1.z, v2.z)
		)
	}
	def apply(v:Vec3):Volume = Volume(v,v)
	def apply(x:Double, y:Double, z:Double):Volume = Volume(Interval(x), Interval(y), Interval(z))
	def apply(value:Double):Volume = Volume(value, value, value)
}

}
