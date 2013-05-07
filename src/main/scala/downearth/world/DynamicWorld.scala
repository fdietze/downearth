package downearth.world

import scala.collection.mutable.ArrayBuffer

import downearth.entity.Entity

// this object should contain all objects of the scene that are not part of the static environment
class DynamicWorld {
	val entities = ArrayBuffer[Entity]()
}