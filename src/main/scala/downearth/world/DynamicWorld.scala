package downearth.world

import scala.collection.mutable.ArrayBuffer
import simplex3d.math.double._
import downearth.entity._
import downearth.rendering.{ObjManager, ObjLoader}
import java.io._

// this object should contain all objects of the scene that are not part of the static environment
object DynamicWorld {
	def testScene:DynamicWorld = {
		//val mesh = ObjManager.testMesh
		//mesh.genvbo()

		val world = new DynamicWorld

    /*
		world.entities += new SimpleEntity(Vec3(0), mesh)
		world.entities += new SimpleEntity(Vec3(1), mesh)
		world.entities += new SimpleEntity(Vec3(2), mesh)
		*/
		
		world
	}
}


class DynamicWorld {
	val entities = ArrayBuffer[Entity]()
}
