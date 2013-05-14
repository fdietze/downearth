package downearth.generation

import simplex3d.math.double._
import interval.{Interval, Interval3}

object Material {
  def apply(r:Int, g:Int, b:Int):Material = new Material((r << 16) + (g << 8) + b,0)
  def apply(r:Double, g:Double, b:Double):Material = Material((r*255).toInt, (g*255).toInt, (b*255).toInt)
  def apply():Material = new Material(0xFF00FF,0)
}

case class Material(color:Int, id:Int) {
  def red   = color >> 16
  def green = (color & 0x00FF00) >> 8
  def blue  = color & 0xFF
  def vec4  = Vec4(red/255.0,green/255.0,blue/255.0,1)
}

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
