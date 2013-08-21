package interval

object Interval {
  // Degenerate Interval
  def apply(value:Double):Interval = new Interval(value,value)
  def infinity = new Interval(scala.Double.NegativeInfinity, scala.Double.PositiveInfinity)
}

case class Interval(low:Double = 0.0, high:Double = 0.0) {
  assert(low <= high, "Invalid Interval: ["+low+", "+high+"], high < low")

  def isPositive = low > 0
  def isNegative = high < 0
  def isDegenerate = low == high

  def width = high - low
  def radius = 0.5*width
  def midpoint = 0.5*(low + high)

  // Does the Interval contain value?
  def apply(value:Double) = low <= value && value <= high

  // -Interval
  def unary_- = new Interval(-high, -low)

  // Interval <op> Interval
  def + (that:Interval) = new Interval(this.low + that.low,  this.high + that.high)
  def - (that:Interval) = new Interval(this.low - that.high, this.high - that.low )
  def * (that:Interval) = {
    if( this.isPositive && that.isPositive )
      new Interval(this.low * that.low, this.high * that.high)
    else {
      val S = Array(this.low*that.low, this.low*that.high, this.high*that.low, this.high*that.high)
      new Interval(S.min, S.max)
    }
  }
  def / (that:Interval) = {
    if( that(0) )
      Interval.infinity
    else
      this * (new Interval(1 / that.high, 1 / that.low))
  }

  // Interval <op> Scalar
  def + (that:Double) = Interval(this.low + that, this.high + that)
  def - (that:Double) = Interval(this.low - that, this.high - that)
  def * (that:Double) = if(that >= 0) Interval(this.low * that, this.high * that) else Interval(this.high * that, this.low * that)
  def / (that:Double) = if(that > 0) Interval(this.low / that, this.high / that) else Interval(this.high / that, this.low / that)

  override def toString = "["+low+","+high+"]"
}
