package downearth.generation

import simplex3d.math.double._
import interval.{Interval, Interval3}
import simplex3d.math.{ReadVec3i, Vec3i}
import downearth.resources.Material

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

  def density(pos:ReadVec3):Double
  def material(pos:ReadVec3):Material
  def materialAtBlock(pos:ReadVec3i) = material(Vec3(pos + 0.5))

  def bounds(area:Interval3):Interval
  def intervalExtension(area:Interval3):Interval
}
