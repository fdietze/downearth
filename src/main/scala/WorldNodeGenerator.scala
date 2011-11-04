package openworld

//import akka.actor.Actor
//import Actor._
//import akka.dispatch.Future

import scala.actors.{DaemonActor, OutputChannel, Actor, Future}

import simplex3d.math.Vec3i
import simplex3d.math.float.{Vec2, Vec3, Vec4}

import collection.mutable.{Queue, SynchronizedQueue, SynchronizedSet, HashSet}

case object PoisonPill

// Verwaltung, um die Erzeugung der MeshNodes auf alle Prozesse aufzuteilen
object WorldNodeGenerator {
	object Master extends DaemonActor {
		val jobqueue = new SynchronizedQueue[Cuboid]
		val done  = new SynchronizedQueue[(NodeInfo, OctantOverMesh)] //TODO: im worldoctree speichern
		// Alle im moment bearbeiteten jobs
		val activeJobs = new HashSet[Cuboid] with SynchronizedSet[Cuboid] //TODO: activejobs rauswerfen, hier rauswerfen und information im Octree speichern "generatingNode"
		
		val workers = (1 to Config.numWorkingThreads) map (i => new Worker(i))
		workers.foreach(_.start)
		
		val idleWorkers = Queue[OutputChannel[Cuboid]](workers:_*)
		
		def act = {
			loop {
				react{
				
				// Master erhält neuen Job und verteilt ihn.
				case cuboid:Cuboid =>
					activeJobs += cuboid
					if( !idleWorkers.isEmpty )
						idleWorkers.dequeue ! cuboid //Verteilen
					else
						jobqueue enqueue cuboid //Warteschlange

				// Worker meldet abgeschlossenen Job (als nodeinfo)
				case ( oldjob:NodeInfo, node:OctantOverMesh ) =>
					done enqueue ( oldjob -> node )
				

				// Worker meldet abgeschlossenen Job (als cuboid)
				case (oldjob:Cuboid, 'done) =>
					activeJobs -= oldjob
					
					// Wenn es noch Jobs gibt, dem Worker einen neuen geben
					if( jobqueue.isEmpty )
						idleWorkers enqueue sender
					else {
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
		val texCoordArray = new Array[Vec2](0)
//		val colorArray  = new Array[Vec4](0)
		TextureMeshData(vertexArray,normalArray,texCoordArray)
	}
	
	class Worker (id:Int) extends Actor {
		var alive = true
		var isActive = false
		def act = {
			while(alive) {
				receive {
					case cuboid @ Cuboid(cuboidpos, cuboidsize) =>
						isActive = true
						val interval = Config.prediction(cuboid.volume)
						
						if(interval.isPositive) {
							Draw addPredictedCuboid cuboid  // Für DebugDraw
							
							isActive = false
							for( nodeinfo <- cuboid.nodeinfos ) {
								val meshnode = new MeshNode(Leaf(FullHexaeder))
								meshnode.mesh = MutableTextureMesh( emptyTextureMeshData )
								Master ! Tuple2(nodeinfo, meshnode)
							}
						}
						
						else if(interval.isNegative) {
							Draw addPredictedCuboid cuboid  // Für DebugDraw
							
							isActive = false
							for( nodeinfo <- cuboid.nodeinfos ) {
								val meshnode = new MeshNode(Leaf(EmptyHexaeder))
								meshnode.mesh = MutableTextureMesh( emptyTextureMeshData )
								Master ! Tuple2(nodeinfo, meshnode)
							}
						}

						else {
							// falls der Bereich groß genug ist splitten und Teile neu predicten
							if( cuboid.size(cuboid.longestedge)/2 >= Config.minPredictionSize ) {
								// für alle Kindknoten den Cuboid an Master senden
								
								isActive = false
								//Master ! Tuple2(nodeinfo, Range(0,8).map( i => nodeinfo(i)))
								if( Config.kdTreePrediction )
									for( child <- cuboid.splitlongest )
										Master ! child
								else
									for( child <- cuboid.octsplit )
										Master ! child
							}
							// sonst samplen
							else {
								assert( cuboid.isCube )
								val nodeinfo = cuboid.nodeinfo
								val node = WorldGenerator genWorldAt nodeinfo
								isActive = false
								Master ! Tuple2(nodeinfo, node.root)
							}
						}
						Master ! (cuboid, 'done)

						
					case PoisonPill => 
						alive = false
				}
			}
		}
		override def toString = "Worker(%d %b)".format(id, isActive)
	}
}

