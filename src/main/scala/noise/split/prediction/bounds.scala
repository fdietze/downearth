package noise.split.prediction

import interval.{Interval, Interval3, Interval4}
import simplex3d.math.double._


object bounds {
  val minvalue = 0.0
  val maxvalue = 1.125 //TODO: tighter bounds
  def apply(range:Interval3, n:Int, i:Int) = Interval(minvalue, maxvalue)
}
