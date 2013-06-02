package downearth.message

package object implicits {
  import downearth.message

  implicit def SimplexVec3iToMessage(v:simplex3d.math.Vec3i) = message.Vec3i(v.x, v.y, v.z)
}
