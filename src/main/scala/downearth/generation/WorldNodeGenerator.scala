package downearth.generation

//import akka.actor.Actor
//import Actor._
//import akka.dispatch.Future

import akka.pattern.ask
import simplex3d.math.double.{Vec2, Vec3, Vec4}

import collection.mutable.{Queue, SynchronizedQueue, SynchronizedSet, HashSet}
import akka.actor._
import scala.Tuple2
import downearth.worldoctree._
import downearth.{MutableTextureMesh, TextureMeshData, Config}
import downearth.rendering.Draw
import downearth.worldoctree.NodeInfo
import downearth.worldoctree.Cuboid
import scala.Tuple2

// Verwaltung, um die Erzeugung der MeshNodes auf alle Prozesse aufzuteilen
object WorldNodeGenerator {
  val actorSystem = ActorSystem.create("worldNodeGenerator")
  val master = actorSystem.actorOf( Props(new Master) )
}

case object GetFinishedJobs

class Master extends Actor {
  val jobqueue = new SynchronizedQueue[Cuboid]
  val done  = new SynchronizedQueue[(NodeInfo, OctantOverMesh)] //TODO: im worldoctree speichern
  // Alle im moment bearbeiteten jobs
  val activeJobs = new HashSet[Cuboid] with SynchronizedSet[Cuboid] //TODO: activejobs rauswerfen, hier rauswerfen und information im Octree speichern "generatingNode"

  val workers = (1 to Config.numWorkingThreads) map ( i => context.actorOf( Props(new Worker(i)) ))

  val idleWorkers = Queue(workers:_*)

  def receive = {
    case GetFinishedJobs =>
      val result = done.dequeueAll( _ => true)
      sender ! result
    case nodeInfo:NodeInfo =>
      sender ! (activeJobs.find( _ indexInRange nodeInfo ).isDefined || done.find( _._1 indexInRange nodeInfo).isDefined)

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

  override def toString = "Master"
}

class Worker (id:Int) extends Actor {
  var alive = true
  var isActive = false

  val emptyTextureMeshData = {
    val vertexArray = new Array[Vec3](0)
    val normalArray = new Array[Vec3](0)
    val texCoordArray = new Array[Vec2](0)
    //		val colorArray  = new Array[Vec4](0)
    TextureMeshData(vertexArray,normalArray,texCoordArray)
  }

  def receive = {
    case cuboid @ Cuboid(cuboidpos, cuboidsize) =>
      isActive = true
      val interval = Config.prediction(cuboid.volume)

      if(interval.isPositive) {
        Draw addPredictedCuboid cuboid  // Für DebugDraw

        isActive = false
        for( nodeinfo <- cuboid.nodeinfos ) {
          val meshnode = new MeshNode(Leaf(FullHexaeder))
          meshnode.mesh = MutableTextureMesh( emptyTextureMeshData )
          sender ! Tuple2(nodeinfo, meshnode)
        }
      }

      else if(interval.isNegative) {
        Draw addPredictedCuboid cuboid  // Für DebugDraw

        isActive = false
        for( nodeinfo <- cuboid.nodeinfos ) {
          val meshnode = new MeshNode(Leaf(EmptyHexaeder))
          meshnode.mesh = MutableTextureMesh( emptyTextureMeshData )
          sender ! Tuple2(nodeinfo, meshnode)
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
              sender ! child  // Master
          else
            for( child <- cuboid.octsplit )
              sender ! child  // Master
        }
        // sonst samplen
        else {
          assert( cuboid.isCube )
          val nodeinfo = cuboid.nodeinfo
          val node = WorldGenerator genWorldAt nodeinfo
          isActive = false
          sender ! Tuple2(nodeinfo, node.root)  // Master
        }
      }
      sender ! (cuboid, 'done)  // Master
  }

  override def toString = "Worker(%d %b)".format(id, isActive)
}


