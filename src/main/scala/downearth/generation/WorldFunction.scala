package downearth.generation

import simplex3d.math.double._
import interval.{Interval, Interval3}

abstract class WorldFunction {
  val perlinNoise3:(Vec3 => Double) = noise.perlin.simple _
  val perlinNoise3Prediction        = noise.perlin.prediction.bezierSimple
  val perlinNoise3Bounds            = noise.perlin.prediction.boundsSimple

  val worleyNoise3:(Vec3 => Vec4)   = noise.worley.reference _
  val worleyNoise3Prediction        = noise.worley.prediction.distance
  val worleyNoise3Bounds            = noise.worley.prediction.bounds

  def range(area:Interval3) = {
    val trivialBounds = bounds(area)
    // if trivial bounds contain surface, investigate further
    if( trivialBounds(0) )
      intervalExtension(area)
    else
      trivialBounds
  }

  def density(pos:Vec3):Double
  def material(pos:Vec3):Material

  def bounds(area:Interval3):Interval
  def intervalExtension(area:Interval3):Interval
}
