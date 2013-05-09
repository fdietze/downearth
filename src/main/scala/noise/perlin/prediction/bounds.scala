package noise.perlin.prediction

import interval.{Interval3, Interval}

// http://www.gamedev.net/topic/285533-2d-perlin-noise-gradient-noise-range--/

object boundsSimple {
  def apply(range:Interval3) = Interval(-1.036353811211803, 1.036353811211803)
}

object boundsImproved {
  def apply(range:Interval3) = Interval(-1.036353811211803, 1.036353811211803)
}

