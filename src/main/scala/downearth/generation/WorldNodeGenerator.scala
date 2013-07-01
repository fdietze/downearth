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
import downearth.{Player, FrustumTestImpl, Config}
import downearth.rendering.{MutableTextureMesh, TextureMeshData, GlDraw, Draw}
import downearth.worldoctree.NodeInfo
import downearth.worldoctree.Cuboid
import scala.Tuple2
import downearth.server.LocalServer
import akka.util.Timeout
import scala.concurrent.Await

// Verwaltung, um die Erzeugung der MeshNodes auf alle Prozesse aufzuteilen
object WorldNodeGenerator {
  val actorSystem = ActorSystem.create("worldNodeGenerator")
  val master = actorSystem.actorOf( Props[Master] )
}

case object GetFinishedJobs
case object ActiveJobsEmpty
case object AllJobsEmpty

class Master extends Actor {
  val jobqueue = new Queue[NodeInfo]
  val done  = new Queue[(NodeInfo, NodeOverMesh)] //TODO: im worldoctree speichern
  val workers = (1 to Config.numWorkingThreads) map ( i => context.actorOf( Props(classOf[Worker],i) ))

  val idleWorkers = Queue(workers:_*)

  def receive = {
    case GetFinishedJobs =>
      val result = done.dequeueAll( _ => true)
      sender ! result

    case AllJobsEmpty =>
      sender ! (jobqueue.isEmpty && done.isEmpty)

    // Master erhält neuen Job und verteilt ihn.
    case area:NodeInfo =>

      if( idleWorkers.isEmpty )
        jobqueue enqueue area //Warteschlange
      else
        idleWorkers.dequeue ! area //Verteilen

    // Worker has finished Job
    case ( oldjob:NodeInfo, node:NodeOverMesh ) =>
      done enqueue Tuple2(oldjob,node)

      if( jobqueue.isEmpty )
        idleWorkers enqueue sender
      else {
        // if there is a job left assign it.
        val job = if(Config.prioritizeGenerationInFrustum) {
          val test = new FrustumTestImpl(Player.camera.projection, Player.camera.view)
          jobqueue.dequeueFirst(c => test(c)) getOrElse jobqueue.dequeue()
        }
        else
          jobqueue.dequeue()

        sender ! job
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
    case info:NodeInfo =>
      // Prediction:
      // Hier wird bis zur minMeshnodeSize geteilt
      // in genWorldAt wird dann mithilfe der Prediction schlauer gesampled

      val interval = WorldDefinition.range(info.toInterval3)
      val surfaceNotInArea = !interval(0)

      if( surfaceNotInArea ) {
        GlDraw addPredictedCuboid info.toCuboid  // Für DebugDraw
        val meshnode = new MeshNode(Leaf(
          if(interval.isPositive) FullHexaeder else EmptyHexaeder
        ))
        meshnode.mesh = MutableTextureMesh( emptyTextureMeshData )
        sender ! Tuple2(info, meshnode)
      }
      else if( info.size/2 >= Config.minMeshNodeSize ) {
        // if the area is too big, it will be splitted
        val data = Array.fill[NodeOverMesh](8)(UngeneratedInnerNode)
        val node = new InnerNodeOverMesh(data)
        sender ! Tuple2(info, node)
      }
      else { // sonst samplen
        val meshnode = WorldGenerator genWorldAt info
        sender ! Tuple2(info, meshnode)  // Master
      }
  }
}


