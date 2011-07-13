package xöpäx

import akka.actor.Actor
import Actor._
import akka.dispatch.Future

import simplex3d.math.Vec3i

case class GenerateNodeAt(nodepos:Vec3i,nodesize:Int)
case class GenerateSliceAt(slicepos:Vec3i,minMeshNodeSize:Int,slicesize:Vec3i)

object WorldNodeGenerator {
	
	val master = actorOf[Master].start
	
	def generateSliceAt(slicepos:Vec3i,minMeshNodeSize:Int,slicesize:Vec3i):Future[Array3D[Octant]] = {
		println("generateSlice")
		val answer = master !!! GenerateSliceAt(slicepos,minMeshNodeSize,slicesize)
		
		answer.asInstanceOf[Future[Array3D[Octant]]]
	}

	class Master extends Actor {
		def receive = {
			case GenerateSliceAt(slicepos,minMeshNodeSize,slicesize) =>
				self reply WorldGenerator.genSlice(slicepos, minMeshNodeSize, slicesize)
			
		}
	}
}

