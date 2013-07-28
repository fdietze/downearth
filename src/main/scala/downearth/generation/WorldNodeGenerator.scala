package downearth.generation

import akka.pattern.ask
import simplex3d.math.double.{Vec2, Vec3, Mat4}

import collection.mutable
import akka.actor._
import downearth.worldoctree._
import downearth.Config
import downearth.rendering.{TextureMesh, GlDraw}
import downearth.worldoctree.PowerOfTwoCube
import akka.routing.{RoundRobinRouter, SmallestMailboxRouter}
import downearth.AkkaMessages._

// Verwaltung, um die Erzeugung der MeshNodes auf alle Prozesse aufzuteilen
class Master extends Actor {
  val workers = context.actorOf(Props[Worker]
    .withRouter(RoundRobinRouter(Config.numWorkingThreads))
    .withDispatcher("akka.actor.worker-dispatcher")
    , "worker-router")

  def receive = {
    // Master erhält neuen Job und verteilt ihn.
    case area:PowerOfTwoCube =>
      workers ! area
    // TODO: Config.prioritizeGenerationInFrustum

    // Worker has finished Job
    case job:FinishedJob =>
      context.parent ! job
  }

  override def unhandled(message: Any) {
    println("Master: unknown message: " + message)
  }
}

class Worker extends Actor {
  def receive = {
    case area:PowerOfTwoCube =>
      // Prediction:
      val interval = WorldDefinition.range(area.toInterval3)
      val surfaceNotInArea = !interval(0)

      if( surfaceNotInArea ) {
        //debugLog ! Predicted(area)
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


