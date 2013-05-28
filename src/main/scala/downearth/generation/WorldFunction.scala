package downearth.generation

import simplex3d.math.double._
import interval.{Interval, Interval3}

object ColorMaterial {
  @deprecated("","") def apply(r:Int, g:Int, b:Int):ColorMaterial = new ColorMaterial(0, (r << 16) + (g << 8) + b)
  @deprecated("","") def apply(r:Double, g:Double, b:Double):ColorMaterial = ColorMaterial((r*255).toInt, (g*255).toInt, (b*255).toInt)
  @deprecated("","") def apply():ColorMaterial = new ColorMaterial(0, 0xFF00FF)
}

case class ColorMaterial(id:Int, color:Int) {
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
  def material(pos:Vec3):ColorMaterial

  def bounds(area:Interval3):Interval
  def intervalExtension(area:Interval3):Interval
}
