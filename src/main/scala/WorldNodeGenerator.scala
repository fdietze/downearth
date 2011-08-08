package xöpäx

//import akka.actor.Actor
//import Actor._
//import akka.dispatch.Future

import scala.actors.{Actor,Future}

import simplex3d.math.Vec3i

import collection.mutable.{Queue, SynchronizedQueue, SynchronizedSet, HashSet}

object WorldNodeGenerator {
	
	Master.start
	
	def generateFutureNodeAt(nodeinfo : NodeInfo):Future[Octant] = {
		val answer = Master !! nodeinfo
		answer.asInstanceOf[Future[Octant]]
	}
	
	object Master extends Actor {
		val jobqueue = new SynchronizedQueue[NodeInfo]
		val done  = new SynchronizedQueue[(NodeInfo,Octant)]
		val activeJobs = new HashSet[NodeInfo] with SynchronizedSet[NodeInfo]
		
		val workers = (1 to 6) map (new Worker)
		
		val activeWorkers = HashSet[Worker]()
		val idlingWorkers = Queue(workers:_*)
		
		def act = {
			loop{
				react{
					case nodeinfo:NodeInfo =>
						// TODO worker beauftragen falls verfügbar, sonst in die jobqueue
						if(idlingWorkers.isEmpty)
						jobqueue enqueue nodeinfo
					case tuple:Tuple2[NodeInfo,Octant] =>
						// TODO neuen Job vergeben falls verfügbar, sonst worker zu idlingWorkers hinzufügen
						done += tuple
						val job = jobqueue.dequeue
						activeJobs += job
				}
			}
		}
	}
	
	class Worker extends Actor {
		def act = {
			loop{
				react{
					case nodeinfo:NodeInfo =>
						val node = WorldGenerator genWorldAt nodeinfo
						node.genMesh
						Master ! Tuple2(nodeinfo,node)
				}
			}
		}
	}
}

