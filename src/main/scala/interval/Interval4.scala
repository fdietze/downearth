package interval

import simplex3d.math.double._

object Interval4 {
  def apply(v1:ReadVec4, v2:ReadVec4):Interval4 = {
    new Interval4(
      Interval(v1.x, v2.x),
      Interval(v1.y, v2.y),
      Interval(v1.z, v2.z),
      Interval(v1.w, v2.w)
    )
  }
  def apply(v:ReadVec4):Interval4 = Interval4(v,v)
  def apply(x:Double, y:Double, z:Double, w:Double):Interval4 = new Interval4(Interval(x), Interval(y), Interval(z), Interval(w))
  def apply(value:Double):Interval4 = Interval4(value, value, value, value)
}

case class Interval4(x:Interval = Interval(), y:Interval = Interval(), z:Interval = Interval(), w:Interval) {
  def low  = Vec4(x.low , y.low , z.low,  w.low )
  def high = Vec4(x.high, y.high, z.high, w.high)

  def isDegenerate = x.isDegenerate || y.isDegenerate || z.isDegenerate || w.isDegenerate
  def apply(v:ReadVec4) = x(v.x) && y(v.y) && z(v.z) && w(v.w)

  // -Interval4
  def unary_- = Interval4(-x, -y, -z, -w)

  // Interval4 <op> Interval4
  def + (that:Interval4) = Interval4(this.x + that.x, this.y + that.y, this.z + that.z, this.w + that.w)
  def - (that:Interval4) = Interval4(this.x - that.x, this.y - that.y, this.z - that.z, this.w - that.w)
  def * (that:Interval4) = Interval4(this.x * that.x, this.y * that.y, this.z * that.z, this.w * that.w)
  def / (that:Interval4) = Interval4(this.x / that.x, this.y / that.y, this.z / that.z, this.w / that.w)

  // Interval4 <op> Scalar
  def + (that:Double) = Interval4(this.x + that, this.y + that, this.z + that, this.w + that)
  def - (that:Double) = Interval4(this.x - that, this.y - that, this.z - that, this.w - that)
  def * (that:Double) = Interval4(this.x * that, this.y * that, this.z * that, this.w * that)
  def / (that:Double) = Interval4(this.x / that, this.y / that, this.z / that, this.w / that)

  // Interval4 <op> Interval
  def + (that:Interval) = Interval4(this.x + that, this.y + that, this.z + that, this.w + that)
  def - (that:Interval) = Interval4(this.x - that, this.y - that, this.z - that, this.w - that)
  def * (that:Interval) = Interval4(this.x * that, this.y * that, this.z * that, this.w * that)
  def / (that:Interval) = Interval4(this.x / that, this.y / that, this.z / that, this.w / that)
}
