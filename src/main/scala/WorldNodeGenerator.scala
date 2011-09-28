package openworld

//import akka.actor.Actor
//import Actor._
//import akka.dispatch.Future

import scala.actors.{DaemonActor, OutputChannel, Actor, Future}

import simplex3d.math.Vec3i

import collection.mutable.{Queue, SynchronizedQueue, SynchronizedSet, HashSet}

case object PoisonPill

object WorldNodeGenerator {
	
	object Master extends DaemonActor {
		val jobqueue = new SynchronizedQueue[NodeInfo]
		val done  = new SynchronizedQueue[(NodeInfo,Octant)]
		val activeJobs = new HashSet[NodeInfo] with SynchronizedSet[NodeInfo]
		
		val workers = (1 to Config.numWorkingThreads) map (i => new Worker)
		workers.foreach(_.start)
		
		val idlingWorkers = Queue[OutputChannel[NodeInfo]](workers:_*)
		
		def act = {
			loop{
				react{
				case nodeinfo:NodeInfo =>
					activeJobs += nodeinfo
					// worker beauftragen falls verfügbar, sonst in die jobqueue
					if(idlingWorkers.isEmpty) {
						jobqueue enqueue nodeinfo
					}
					else {
						val worker = idlingWorkers.dequeue
						worker ! nodeinfo
					}
				case ( oldjob : NodeInfo, node : Octant ) =>
					// neuen Job vergeben falls verfügbar, sonst worker zu idlingWorkers hinzufügen
					done enqueue ( oldjob -> node )
					activeJobs -= oldjob
					
					if(jobqueue.isEmpty){
						idlingWorkers enqueue sender
					}
					else{
						val job = jobqueue.dequeue
						sender ! job
						activeJobs += job
					}
				case PoisonPill =>
					for( worker <- workers )
						worker ! PoisonPill
				}
			}
		}
		start
	}
	
	class Worker extends Actor {
		var alive = true
		def act = {
			while(alive){
				receive {
					case nodeinfo:NodeInfo =>
						val node = WorldGenerator genWorldAt nodeinfo
						Master ! Tuple2(nodeinfo,node.root)
					case PoisonPill => 
						alive = false
				}
			}
		}
	}
}

