package downearth.util

import simplex3d.math.double._

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 5/27/13
 * Time: 6:24 PM
 * To change this template use File | Settings | File Templates.
 */
trait Ray {
  def pos:ReadVec3
  def dir:ReadVec3
}

object Ray {
  def apply(_pos:ReadVec3, _dir:ReadVec3) = new Ray {
    val pos = Vec3(_pos)
    val dir = Vec3(_dir)
  }
}
