package downearth.generation

import akka.pattern.ask
import simplex3d.math.double.{Vec2, Vec3, Mat4}

import collection.mutable.Queue
import akka.actor._
import downearth.worldoctree._
import downearth.Config
import downearth.rendering.{TextureMesh, GlDraw}
import downearth.worldoctree.PowerOfTwoCube
import akka.routing.SmallestMailboxRouter

// Verwaltung, um die Erzeugung der MeshNodes auf alle Prozesse aufzuteilen
object WorldNodeGenerator {
  val actorSystem = ActorSystem.create("worldNodeGenerator")
  val master = actorSystem.actorOf( Props[Master] )

  object Messages {
    case object GetFinishedJobs
    case class FinishedJob(job:PowerOfTwoCube, node:MeshNode)
  }
}

class Master extends Actor {
  import WorldNodeGenerator.Messages._

  val workers = context.actorOf(Props[Worker].
    withRouter(SmallestMailboxRouter(Config.numWorkingThreads)), "router")

  val done  = new Queue[FinishedJob] //TODO: im worldoctree speichern (warum?)

  def receive = {
    case GetFinishedJobs =>
      sender ! done.dequeueAll( _ => true)

    // Master erhält neuen Job und verteilt ihn.
    case area:PowerOfTwoCube =>
      workers ! area
    // TODO: Config.prioritizeGenerationInFrustum

    // Worker has finished Job
    case job:FinishedJob =>
      done enqueue job

    case PoisonPill =>
        workers ! PoisonPill

    case unknown =>
      println("Master: unknown message: " + unknown)
  }

  override def toString = "Master"
}

class Worker extends Actor {
  import WorldNodeGenerator.Messages._

  def receive = {
    case area:PowerOfTwoCube =>
      // Prediction:
      val interval = WorldDefinition.range(area.toInterval3)
      val surfaceNotInArea = !interval(0)

      if( surfaceNotInArea ) {
        //GlDraw addPredictedCuboid area  // Für DebugDraw
        val meshNode = new MeshNode(Leaf(
          if(interval.isPositive) FullHexaeder else EmptyHexaeder
        ))
        meshNode.mesh = TextureMesh.empty
        sender ! FinishedJob(area, meshNode)
      }
      // if the area is too big, it will be splitted
      else if( area.size/2 >= Config.minPredictionSize ) {
        val data = Array.fill[NodeUnderMesh](8)(UngeneratedNode)
        val meshNode = new MeshNode(new InnerNodeUnderMesh(data))
        meshNode.mesh = TextureMesh.empty
        sender ! FinishedJob(area, meshNode)
      }
      // sample the whole area
      else {
        val meshNode = WorldGenerator generateNode area
        sender ! FinishedJob(area, meshNode)
      }
  }
}


