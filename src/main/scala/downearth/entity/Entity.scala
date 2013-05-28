package downearth.entity

import simplex3d.math.double._
import downearth.rendering.ObjMesh

trait Entity {
	def pos:Vec3
}

class SimpleEntity(val pos:Vec3, val mesh:ObjMesh) extends Entity {
	require(mesh.hasVbo)
}
