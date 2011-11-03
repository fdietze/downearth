package openworld

//import akka.actor.Actor
//import Actor._
//import akka.dispatch.Future

import scala.actors.{DaemonActor, OutputChannel, Actor, Future}

import simplex3d.math.Vec3i
import simplex3d.math.float.{Vec3, Vec4}

import collection.mutable.{Queue, SynchronizedQueue, SynchronizedSet, HashSet}

case object PoisonPill

// Verwaltung, um die Erzeugung der MeshNodes auf alle Prozesse aufzuteilen
object WorldNodeGenerator {
	object Master extends DaemonActor {
		val jobqueue = new SynchronizedQueue[NodeInfo]
		val done  = new SynchronizedQueue[(NodeInfo,OctantOverMesh)] //TODO: im worldoctree speichern
		// Alle im moment bearbeiteten jobs
		val activeJobs = new HashSet[NodeInfo] with SynchronizedSet[NodeInfo] //TODO: activejobs rauswerfen, hier rauswerfen und information im Octree speichern "generatingNode"
		
		val workers = (1 to Config.numWorkingThreads) map (i => new Worker(i))
		workers.foreach(_.start)
		
		val idleWorkers = Queue[OutputChannel[NodeInfo]](workers:_*)
		
		def distributeJobs {
			while( !idleWorkers.isEmpty && !jobqueue.isEmpty ){
				val job = jobqueue.dequeue
				val worker = idleWorkers.dequeue
				worker ! job
				
				// idleWorkers.dequeue ! jobqueue.dequeue
			}
		}
		
		def act = {
			loop {
				react{
				case ( oldjob:NodeInfo, nodeinfoseq: Seq[_]) =>
					idleWorkers += sender
					
					nodeinfoseq foreach { 
					case nodeInfo:NodeInfo => 
						jobqueue enqueue nodeInfo
						activeJobs += nodeInfo
					}
					
					distributeJobs
					activeJobs -= oldjob
					
				case nodeinfo:NodeInfo =>
					activeJobs += nodeinfo
					// println("Master: habe Nodeinfo " + nodeinfo + " empfangen")
					// worker beauftragen falls verfügbar, sonst in die jobqueue
					if( idleWorkers.isEmpty ) {
						jobqueue enqueue nodeinfo
					}
					else {
						val worker = idleWorkers.dequeue
						worker ! nodeinfo
					}

				// Worker meldet abgeschlossenen Job
				case ( oldjob : NodeInfo, node : OctantOverMesh ) =>
					// neuen Job vergeben falls verfügbar, sonst worker zu idleWorkers hinzufügen
					done enqueue ( oldjob -> node )
					activeJobs -= oldjob
					
					if( jobqueue.isEmpty ) {
						assert( !(idleWorkers contains sender), "IdleWorkers nicht Aktuell" )
						idleWorkers enqueue sender
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
		override def toString = "Master"
	}
	
	val emptyTextureMeshData = {
		val vertexArray = new Array[Vec3](0)
		val normalArray = new Array[Vec3](0)
		val colorArray  = new Array[Vec4](0)
		TextureMeshData(vertexArray,normalArray,colorArray)
	}
	
	class Worker (id:Int) extends Actor {
		var alive = true
		var isActive = false
		def act = {
			while(alive) {
				receive {
					case nodeinfo @ NodeInfo(nodepos, nodesize) =>
						isActive = true
						// println("Ich habe eine NodeInfo " + nodeinfo + " empfangen")
						val interval = Util.time("predicten"){ Config.prediction(Vec3(nodepos),Vec3(nodepos+nodesize))	}
						
						if(interval.isPositive) {
							// println(nodeinfo + ": Prediction war Positiv")
							Draw addPredictedNode nodeinfo  // Für DebugDraw
							
							val meshnode = new MeshNode(Leaf(FullHexaeder))
							meshnode.mesh = MutableTextureMesh( emptyTextureMeshData )
							
							isActive = false
							Master ! Tuple2(nodeinfo, meshnode)
						}
						
						else if(interval.isNegative) {
							// println(nodeinfo + ": Prediction war Negativ")
							Draw addPredictedNode nodeinfo  // Für DebugDraw
							
							val meshnode = new MeshNode(Leaf(EmptyHexaeder))
							meshnode.mesh = MutableTextureMesh( emptyTextureMeshData )
							
							isActive = false
							Master ! Tuple2(nodeinfo, meshnode)
						}

						else {
							// println(nodeinfo + ": Bereich könnte Oberfläche enthalten")
							// falls der Bereich groß genug ist splitten und Teile neu predicten
							if( (nodesize/2) >= Config.minPredictionSize ) {
								// println("Splitting " + nodeinfo)
								// für alle Kindknoten die Nodeinfo an Master senden
								
								// println("Sende nodeinfos an Master...")
								isActive = false
								Master ! Tuple2(nodeinfo, Range(0,8).map( i => nodeinfo(i)))
							}
							// sonst samplen
							else {
								// println(nodeinfo + ": Bereich wird gesamplet")
								val node = WorldGenerator genWorldAt nodeinfo
								isActive = false
								Master ! Tuple2(nodeinfo, node.root)
							}
						}
						
					case PoisonPill => 
						alive = false
				}
			}
		}
		override def toString = "Worker(%d %b)".format(id, isActive)
	}
}

