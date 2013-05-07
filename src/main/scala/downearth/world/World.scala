package downearth.world

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import downearth.util._
import downearth.entity._
import downearth.generation.WorldGenerator
import downearth.worldoctree.{Polyeder, Hexaeder, Leaf}
import downearth.rendering.ObjLoader
import downearth.{BulletPhysics, util, WorldSerializer}


import java.io._

object World {
	val octree = {
		WorldSerializer.load match {
			case Some(s) => s
			case None => WorldGenerator.genWorld
		}
	}
	
	def update(pos:Vec3i, l:Leaf) {
		octree(pos) = l
		BulletPhysics.worldChange(pos)
	}

	val dynamicWorld = new DynamicWorld
	val loader = new ObjLoader
	dynamicWorld.entities += new SimpleEntity(loader.load(new FileInputStream(new File("monkey.obj"))))


}
