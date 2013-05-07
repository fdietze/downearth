package interval

import simplex3d.math.double.{functions => scalar}

package object functions {
  def hull(x:Interval,y:Interval) = Interval(scalar.min(x.low, y.low), scalar.max(x.high, y.high))
  def abs(x:Interval):Double = scalar.max(scalar.abs(x.low), scalar.abs(x.high))
  def min(a:Interval,b:Interval):Interval = Interval(scalar.min(a.low,b.low), scalar.min(a.high, b.high))
  def max(a:Interval,b:Interval):Interval = Interval(scalar.max(a.low,b.low), scalar.max(a.high, b.high))
  def sqrt(i:Interval) = Interval(scalar.sqrt(i.low), scalar.sqrt(i.high))
  def square(i:Interval) = {
    if(i.isPositive)
      Interval(i.low*i.low, i.high*i.high)
    else if( i.high < 0 )
      Interval(i.high*i.high, i.low*i.low)
    else
      Interval(0, scalar.max(i.low*i.low, i.high*i.high))
  }

  def pow(i:Interval, n:Int) = {
    if( i.isPositive || (n & 1) == 1)
      Interval(scalar.pow(i.low,n), scalar.pow(i.high,n))
    else if( i.isNegative && (n & 1) == 0 )
      Interval(scalar.pow(i.high,n), scalar.pow(i.low,n))
    else
      Interval(0,scalar.pow(scalar.max(scalar.abs(i.low), scalar.abs(i.high)),n))
  }

  def exp( x:Interval ) = Interval(scalar.exp(x.low), scalar.exp(x.high))
  def clamp( x:Interval, low:Double, high:Double ) = Interval(scalar.max(x.low,low), scalar.min(x.high,high))



  def dot(a:Interval3, b:Interval3) = a.x*b.x + a.y*b.y + a.z*b.z
  def length(v:Interval3) = sqrt(square(v.x) + square(v.y) + square(v.z))
}
