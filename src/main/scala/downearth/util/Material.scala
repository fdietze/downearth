package downearth.util

import simplex3d.math.double._

/**
 * User: arne
 * Date: 28.04.13
 * Time: 22:39
 */
case class Material(color:Int, id:Int) {
  def red   = color >> 16
  def green = (color & 0x00FF00) >> 8
  def blue  = color & 0xFF
  def vec4  = Vec4(red/255.0,green/255.0,blue/255.0,1)
}
