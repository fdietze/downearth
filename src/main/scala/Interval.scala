package noise

object interval {

import simplex3d.math._
import simplex3d.math.double._
//import simplex3d.math.double.functions._

case class Interval (low:Double = 0.0, high:Double = 0.0) {
	assert(low <= high, "Invalid Interval: ["+low+", "+high+"], low > high")

	def isPositive = low > 0 
	def isNegative = high < 0
	def isDegenerate = low == high

	def width = high - low
	def radius = 0.5*width
	def midpoint = 0.5*(low + high)

	// Does the Interval contain value?
	def apply(value:Double) = low <= value && value <= high
	
	// -Interval
	def unary_- = Interval(-high, -low)
	
	// Interval <op> Interval
	def + (that:Interval) = Interval(this.low + that.low,  this.high + that.high)
	def - (that:Interval) = Interval(this.low - that.high, this.high - that.low )
	def * (that:Interval) = {
		if( this.isPositive && that.isPositive )
			Interval(this.low * that.low, this.high * that.high)
		else {
			val S = Seq(this.low*that.low, this.low*that.high, this.high*that.low, this.high*that.high)
			Interval(S.min, S.max)
		}
	}
	def / (that:Interval) = {
		if( that(0) )
			Interval.infinity
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

object Interval {
	// Degenerate Interval
	def apply(value:Double):Interval = Interval(value,value)
	def infinity = Interval(scala.Double.NegativeInfinity, scala.Double.PositiveInfinity)
}

def hull(x:Interval,y:Interval) = Interval(functions.min(x.low, y.low), functions.max(x.high, y.high))
def abs(x:Interval):Double = functions.max(functions.abs(x.low), functions.abs(x.high))
def min(a:Interval,b:Interval):Interval = Interval(functions.min(a.low,b.low), functions.min(a.high, b.high))
def max(a:Interval,b:Interval):Interval = Interval(functions.max(a.low,b.low), functions.max(a.high, b.high))
def sqrt(i:Interval) = Interval(functions.sqrt(i.low), functions.sqrt(i.high))
def square(i:Interval) = {
	if(i.isPositive)
		Interval(i.low*i.low, i.high*i.high)
	else if( i.high < 0 )
		Interval(i.high*i.high, i.low*i.low)
	else
		Interval(0, functions.max(i.low*i.low, i.high*i.high))
}

def pow(i:Interval, n:Int) = {
	if( i.isPositive || (n & 1) == 1)
		Interval(functions.pow(i.low,n), functions.pow(i.high,n))
	else if( i.isNegative && (n & 1) == 0 )
		Interval(functions.pow(i.high,n), functions.pow(i.low,n))
	else
		Interval(0,functions.pow(functions.max(functions.abs(i.low), functions.abs(i.high)),n))
}

def exp( x:Interval ) = Interval(functions.exp(x.low), functions.exp(x.high))
def clamp( x:Interval, low:Double, high:Double ) = Interval(functions.max(x.low,low), functions.min(x.high,high))

case class Volume(x:Interval = Interval(), y:Interval = Interval(), z:Interval = Interval()) {
	def low  = Vec3(x.low , y.low , z.low )
	def high = Vec3(x.high, y.high, z.high)
	
	def isDegenerate = x.isDegenerate || y.isDegenerate || z.isDegenerate
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

	// Volume <op> Interval
	def + (that:Interval) = Volume(this.x + that, this.y + that, this.z + that)
	def - (that:Interval) = Volume(this.x - that, this.y - that, this.z - that)
	def * (that:Interval) = Volume(this.x * that, this.y * that, this.z * that)
	def / (that:Interval) = Volume(this.x / that, this.y / that, this.z / that)
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

def dot(a:Volume, b:Volume) = a.x*b.x + a.y*b.y + a.z*b.z
def length(v:Volume) = sqrt(square(v.x) + square(v.y) + square(v.z))


case class Interval4D(x:Interval = Interval(), y:Interval = Interval(), z:Interval = Interval(), w:Interval) {
	def low  = Vec4(x.low , y.low , z.low,  w.low )
	def high = Vec4(x.high, y.high, z.high, w.high)
	
	def isDegenerate = x.isDegenerate || y.isDegenerate || z.isDegenerate || w.isDegenerate
	def apply(v:Vec4) = x(v.x) && y(v.y) && z(v.z) && w(v.w)
	
	// -Interval4D
	def unary_- = Interval4D(-x, -y, -z, -w)

	// Interval4D <op> Interval4D
	def + (that:Interval4D) = Interval4D(this.x + that.x, this.y + that.y, this.z + that.z, this.w + that.w)
	def - (that:Interval4D) = Interval4D(this.x - that.x, this.y - that.y, this.z - that.z, this.w - that.w)
	def * (that:Interval4D) = Interval4D(this.x * that.x, this.y * that.y, this.z * that.z, this.w * that.w)
	def / (that:Interval4D) = Interval4D(this.x / that.x, this.y / that.y, this.z / that.z, this.w / that.w)

	// Interval4D <op> Scalar
	def + (that:Double) = Interval4D(this.x + that, this.y + that, this.z + that, this.w + that)
	def - (that:Double) = Interval4D(this.x - that, this.y - that, this.z - that, this.w - that)
	def * (that:Double) = Interval4D(this.x * that, this.y * that, this.z * that, this.w * that)
	def / (that:Double) = Interval4D(this.x / that, this.y / that, this.z / that, this.w / that)

	// Interval4D <op> Interval
	def + (that:Interval) = Interval4D(this.x + that, this.y + that, this.z + that, this.w + that)
	def - (that:Interval) = Interval4D(this.x - that, this.y - that, this.z - that, this.w - that)
	def * (that:Interval) = Interval4D(this.x * that, this.y * that, this.z * that, this.w * that)
	def / (that:Interval) = Interval4D(this.x / that, this.y / that, this.z / that, this.w / that)
}

object Interval4D {
	def apply(v1:Vec4, v2:Vec4):Interval4D = {
		Interval4D(
			Interval(v1.x, v2.x),
			Interval(v1.y, v2.y),
			Interval(v1.z, v2.z),
			Interval(v1.w, v2.w)
		)
	}
	def apply(v:Vec4):Interval4D = Interval4D(v,v)
	def apply(x:Double, y:Double, z:Double, w:Double):Interval4D = Interval4D(Interval(x), Interval(y), Interval(z), Interval(w))
	def apply(value:Double):Interval4D = Interval4D(value, value, value, value)
}

// Scalar <op> Interval
/*implicit def scalarplusinterval(value:Double)  = new { def + (i:Interval):Interval = i + value }
implicit def scalarminusinterval(value:Double) = new { def - (i:Interval):Interval = -i + value }
implicit def scalartimesinterval(value:Double) = new { def * (i:Interval):Interval = i * value }
implicit def scalaroverinterval(value:Double)  = new { def / (i:Interval):Interval = Interval(value) / i }

// Scalar <op> Volume
implicit def scalarplusvolume(value:Double)  = new { def + (i:Volume) = i + value }
implicit def scalarminusvolume(value:Double) = new { def - (i:Volume) = -i + value }
implicit def scalartimesvolume(value:Double) = new { def * (i:Volume) = i * value }
implicit def scalarovervolume(value:Double)  = new { def / (i:Volume) = Volume(value) / i }
*/

}
