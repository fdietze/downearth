package noise.split.prediction

import interval.{Interval3, Interval4}
import simplex3d.math.double._


object bounds {
  val minvalue = Vec4(0.0)
  val maxvalue = Vec4(1.125) //TODO: tighter bounds
  def apply(range:Interval3) = Interval4(minvalue, maxvalue)
}
