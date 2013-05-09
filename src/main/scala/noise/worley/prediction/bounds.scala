package noise.worley.prediction

import interval.{Interval3, Interval4}
import simplex3d.math.double._


object bounds {
  val minvalue = Vec4(0)
  val maxvalue = Vec4(1.1242269084951875, 1.1555402454923787, 1.2139070974183257, 1.2354559268805023)
  def apply(range:Interval3) = Interval4(minvalue, maxvalue)
}
