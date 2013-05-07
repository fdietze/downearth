package noise.worley.prediction

import interval.{Interval3, Interval4}
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import noise.worley


object distance {
  val minvalue = Vec4(0)
  val maxvalue = Vec4(1.1242269084951875, 1.1555402454923787, 1.2139070974183257, 1.2354559268805023)

  def apply(v:Interval3):Interval4 = {
    val center = (v.high + v.low)*0.5
    val radius = length(center - v.low)
    val sample = worley.reference(center)
    Interval4(max(minvalue, sample-radius), min(maxvalue, sample+radius))
  }
}
