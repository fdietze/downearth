package downearth.worldoctree

import simplex3d.math.Vec3i

/**
 * User: arne
 * Date: 29.04.13
 * Time: 01:54
 */

class Selector(val pos:Vec3i) {
  def incX = move(true, 0)
  def decX = move(false, 0)
  def incY = move(true, 1)
  def decY = move(false, 1)
  def incZ = move(true, 2)
  def decZ = move(false, 2)

  def move(inc:Boolean, dim:Int) {

  }
}
