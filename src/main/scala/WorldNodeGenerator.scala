package xöpäx

//import akka.actor.Actor
//import Actor._
//import akka.dispatch.Future

import scala.actors.{Actor,Future}

import simplex3d.math.Vec3i

case class GenerateNodeAt(nodepos:Vec3i,nodesize:Int)
case class GenerateSliceAt(slicepos:Vec3i,minMeshNodeSize:Int,slicesize:Vec3i)

object WorldNodeGenerator {
	
	Master.start
	
	def generateSliceAt(slicepos:Vec3i,minMeshNodeSize:Int,slicesize:Vec3i):Future[Array3D[Octant]] = {
		println("generateSlice")
		val answer = Master !! GenerateSliceAt(slicepos,minMeshNodeSize,slicesize)
		
		answer.asInstanceOf[Future[Array3D[Octant]]]
	}

	def generateNodeAt(nodepos:Vec3i,nodesize:Int):Future[Octant] = {
		val answer = Master !! GenerateNodeAt(nodepos,nodesize)
		answer.asInstanceOf[Future[Octant]]
	}

	object Master extends Actor {
		def act = {
			loop{ 
				react{
					case GenerateSliceAt(slicepos,minMeshNodeSize,slicesize) =>
						reply(WorldGenerator.genSlice(slicepos, minMeshNodeSize, slicesize))
					case GenerateNodeAt(nodepos,nodesize) =>
						val node = WorldGenerator.genWorldAt(nodepos,nodesize)
						Util.time("genmesh: ")(node.genMesh)
						reply(node.root)
				}
			}
		}
	}
}

