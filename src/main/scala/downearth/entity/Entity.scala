package downearth.entity

import downearth.ObjMesh
import simplex3d.math.double._

trait Entity {
	def pos:Vec3
}

class SimpleEntity(val pos:Vec3, val mesh:ObjMesh) extends Entity {
	require(mesh.hasVbo)
}
