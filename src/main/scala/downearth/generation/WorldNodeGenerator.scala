package downearth.generation

import akka.pattern.ask
import simplex3d.math.double.{Vec2, Vec3, Mat4}

import collection.mutable.{Queue, SynchronizedQueue, SynchronizedSet, HashSet}
import akka.actor._
import scala.Tuple2
import downearth.worldoctree._
import downearth.{Player, FrustumTestImpl, Config}
import downearth.rendering.{TextureMesh, TextureMeshData, GlDraw, Draw}
import downearth.worldoctree.PowerOfTwoCube
import downearth.worldoctree.Cuboid
import scala.Tuple2
import downearth.server.LocalServer
import akka.util.Timeout
import scala.concurrent.Await

// Verwaltung, um die Erzeugung der MeshNodes auf alle Prozesse aufzuteilen
object WorldNodeGenerator {
  val actorSystem = ActorSystem.create("worldNodeGenerator")
  val master = actorSystem.actorOf( Props[Master] )
  object Messages {
    case object GetFinishedJobs
    case object ActiveJobsEmpty
    case object AllJobsEmpty
    case class FinishedJob(job:PowerOfTwoCube, node:Node)
  }
}

class Master extends Actor {
  import WorldNodeGenerator.Messages._

  val jobqueue = new Queue[PowerOfTwoCube]
  val done  = new Queue[(PowerOfTwoCube, NodeOverMesh)] //TODO: im worldoctree speichern
  val workers = (1 to Config.numWorkingThreads) map ( i => context.actorOf( Props(classOf[Worker],i) ))

  val idleWorkers = Queue(workers:_*)

  def receive = {
    case GetFinishedJobs =>
      val result = done.dequeueAll( _ => true)
      sender ! result


    case AllJobsEmpty =>
      sender ! (jobqueue.isEmpty && done.isEmpty)


    // Master erhält neuen Job und verteilt ihn.
    case area:PowerOfTwoCube =>
      if( idleWorkers.isEmpty )
        jobqueue enqueue area //Warteschlange
      else
        idleWorkers.dequeue ! area //Verteilen


    // Worker has finished Job
    case FinishedJob( oldjob:PowerOfTwoCube, node:NodeOverMesh ) =>
      done enqueue Tuple2(oldjob,node)

      if( jobqueue.isEmpty )
        idleWorkers enqueue sender
      else {
        // if there is a job left assign it.
        val job = if(Config.prioritizeGenerationInFrustum) {
          val test = new FrustumTestImpl(Mat4(Player.camera.projection), Mat4(Player.camera.view))
          jobqueue.dequeueFirst(c => test(c)) getOrElse jobqueue.dequeue()
        }
        else
          jobqueue.dequeue()

        sender ! job
      }


    case PoisonPill =>
      for( worker <- workers )
        worker ! PoisonPill


    case unknown =>
      println("Master: unknown message: " + unknown)
  }

  override def toString = "Master"
}

class Worker (id:Int) extends Actor {
  import WorldNodeGenerator.Messages._

  val emptyTextureMeshData = {
    val vertexArray = new Array[Vec3](0)
    val normalArray = new Array[Vec3](0)
    val texCoordArray = new Array[Vec2](0)
    //		val colorArray  = new Array[Vec4](0)
    TextureMeshData(vertexArray,normalArray,texCoordArray)
  }

  def receive = {
    case area:PowerOfTwoCube =>
      // Prediction:
      // Hier wird bis zur minMeshnodeSize geteilt

      val interval = WorldDefinition.range(area.toInterval3)
      val surfaceNotInArea = !interval(0)

      if( surfaceNotInArea ) {
        GlDraw addPredictedCuboid area  // Für DebugDraw
        val meshnode = new MeshNode(Leaf(
          if(interval.isPositive) FullHexaeder else EmptyHexaeder
        ))
        meshnode.mesh = TextureMesh( emptyTextureMeshData )
        sender ! FinishedJob(area, meshnode)
      }
      // if the area is too big, it will be splitted
      else if( area.size/2 >= Config.minMeshNodeSize ) {
        val data = Array.fill[NodeUnderMesh](8)(UngeneratedNode)
        val node = new InnerNodeUnderMesh(data)
        sender ! FinishedJob(area, node)
      }
      // sample the whole area
      else {
        val meshnode = WorldGenerator generateNode area
        sender ! FinishedJob(area, meshnode)  // Master
      }
  }
}


