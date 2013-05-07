package noise.interval

import simplex3d.math.double._

object Interval3 {
  def apply(v1:Vec3, v2:Vec3):Interval3 = {
    Interval3(
      Interval(v1.x, v2.x),
      Interval(v1.y, v2.y),
      Interval(v1.z, v2.z)
    )
  }
  def apply(v:Vec3):Interval3 = Interval3(v,v)
  def apply(x:Double, y:Double, z:Double):Interval3 = Interval3(Interval(x), Interval(y), Interval(z))
  def apply(value:Double):Interval3 = Interval3(value, value, value)
}

case class Interval3(x:Interval = Interval(), y:Interval = Interval(), z:Interval = Interval()) {
  def low  = Vec3(x.low , y.low , z.low )
  def high = Vec3(x.high, y.high, z.high)

  def isDegenerate = x.isDegenerate || y.isDegenerate || z.isDegenerate
  def apply(v:Vec3) = x(v.x) && y(v.y) && z(v.z)

  // -Interval3
  def unary_- = Interval3(-x, -y, -z)

  // Interval3 <op> Interval3
  def + (that:Interval3) = Interval3(this.x + that.x, this.y + that.y, this.z + that.z)
  def - (that:Interval3) = Interval3(this.x - that.x, this.y - that.y, this.z - that.z)
  def * (that:Interval3) = Interval3(this.x * that.x, this.y * that.y, this.z * that.z)
  def / (that:Interval3) = Interval3(this.x / that.x, this.y / that.y, this.z / that.z)

  // Interval3 <op> Scalar
  def + (that:Double) = Interval3(this.x + that, this.y + that, this.z + that)
  def - (that:Double) = Interval3(this.x - that, this.y - that, this.z - that)
  def * (that:Double) = Interval3(this.x * that, this.y * that, this.z * that)
  def / (that:Double) = Interval3(this.x / that, this.y / that, this.z / that)

  // Interval3 <op> Interval
  def + (that:Interval) = Interval3(this.x + that, this.y + that, this.z + that)
  def - (that:Interval) = Interval3(this.x - that, this.y - that, this.z - that)
  def * (that:Interval) = Interval3(this.x * that, this.y * that, this.z * that)
  def / (that:Interval) = Interval3(this.x / that, this.y / that, this.z / that)
}
