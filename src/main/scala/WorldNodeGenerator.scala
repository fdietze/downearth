package openworld

//import akka.actor.Actor
//import Actor._
//import akka.dispatch.Future

import scala.actors.{DaemonActor, OutputChannel, Actor, Future}

import simplex3d.math.Vec3i
import simplex3d.math.float.Vec3

import collection.mutable.{Queue, SynchronizedQueue, SynchronizedSet, HashSet}

case object PoisonPill

// Verwaltung, um die Erzeugung der MeshNodes auf alle Prozesse aufzuteilen
object WorldNodeGenerator {
	object Master extends DaemonActor {
		val jobqueue = new SynchronizedQueue[NodeInfo]
		val done  = new SynchronizedQueue[(NodeInfo,OctantOverMesh)]
		val activeJobs = new HashSet[NodeInfo] with SynchronizedSet[NodeInfo]
		
		val workers = (1 to Config.numWorkingThreads) map (i => new Worker)
		workers.foreach(_.start)
		
		val idlingWorkers = Queue[OutputChannel[NodeInfo]](workers:_*)
		
		def act = {
			loop{
				react{
				case ( oldjob:NodeInfo, nodeinfoseq: Seq[_]) =>
					sender ! nodeinfoseq.head
					// done enqueue (oldjob, DeadNode)
					
					nodeinfoseq.tail foreach { 
					case ni:NodeInfo => 
						jobqueue enqueue ni
					}
					activeJobs -= oldjob
					
					val job = jobqueue.dequeue
					sender ! job
					activeJobs += job
					
					while( ! idlingWorkers.isEmpty ) {
						val job = jobqueue.dequeue
						val worker = idlingWorkers.dequeue
						worker ! job
						activeJobs += job
					}
					
				case nodeinfo:NodeInfo =>
					// println("Master: habe Nodeinfo " + nodeinfo + " empfangen")
					activeJobs += nodeinfo
					// worker beauftragen falls verfügbar, sonst in die jobqueue
					if(idlingWorkers.isEmpty) {
						jobqueue enqueue nodeinfo
					}
					else {
						val worker = idlingWorkers.dequeue
						worker ! nodeinfo
					}
				case ( oldjob : NodeInfo, node : OctantOverMesh ) =>
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
				
				case irgendwas =>
					println("Master: konnte " + irgendwas + " nicht erkennen")
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
					case nodeinfo @ NodeInfo(nodepos, nodesize) =>
						// println("Ich habe eine NodeInfo " + nodeinfo + " empfangen")
						val interval = Config.prediction(Vec3(nodepos),Vec3(nodepos+nodesize))	
						
						if(interval.isPositive) {
							// println(nodeinfo + ": Prediction war Positiv")
							Draw addPredictedNode nodeinfo  // Für DebugDraw
							
							val octree = new WorldOctree( nodesize, nodepos.clone )
							octree.root = new Leaf(FullHexaeder)
							octree.genMesh(x => FullHexaeder)
							
							Master ! Tuple2(nodeinfo, octree.root)

						}
						
						else if(interval.isNegative) {
							// println(nodeinfo + ": Prediction war Negativ")
							Draw addPredictedNode nodeinfo  // Für DebugDraw
							
							val octree = new WorldOctree( nodesize, nodepos.clone )
							octree.root = new Leaf(EmptyHexaeder)
							octree.genMesh(x => EmptyHexaeder)
							
							Master ! Tuple2(nodeinfo, octree.root)
						}

						else {
							// println(nodeinfo + ": Bereich könnte Oberfläche enthalten")
							// falls der Bereich groß genug ist splitten und Teile neu predicten
							if( (nodesize/2) >= Config.minPredictionSize ) {
								// println("Splitting " + nodeinfo)
								// für alle Kindknoten die Nodeinfo an Master senden
								
								// println("Sende nodeinfos an Master...")
								Master ! Tuple2(nodeinfo, Range(0,8).map( i => nodeinfo(i)))
							}
							// sonst samplen
							else {
								// println(nodeinfo + ": Bereich wird gesamplet")
								val node = WorldGenerator genWorldAt nodeinfo
								Master ! Tuple2(nodeinfo, node.root)
							}
						}
						
					case PoisonPill => 
						alive = false
				}
			}
		}
	}
}

