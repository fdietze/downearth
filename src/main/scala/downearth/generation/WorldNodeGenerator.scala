package downearth.generation

import akka.pattern.ask
import simplex3d.math.double.{Vec2, Vec3, Mat4}
import simplex3d.math.double.functions._

import collection.mutable
import akka.actor._
import downearth.worldoctree._
import downearth.Config
import downearth.rendering.{TextureMesh, GlDraw}
import downearth.worldoctree.PowerOfTwoCube
import akka.routing.{RoundRobinRouter, SmallestMailboxRouter}
import downearth.AkkaMessages._
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}

class WorkerMailbox(settings: ActorSystem.Settings, config: com.typesafe.config.Config)
  extends UnboundedPriorityMailbox(
    // Create a new PriorityGenerator, lower prio means more important
    PriorityGenerator {
      case PoisonPill      => 0
      case GeneratingJob(area,playerPos) => if( Config.prioritizeCloseGenerationJobs )
            2 + length(playerPos - area.center).toInt
        else 2

      case otherwise       => 1
    })


class Worker(gameLoop:ActorRef, debugLog:ActorRef) extends Actor {

  def receive = {
    case GeneratingJob(area,_) =>
      // Prediction:
      val interval = WorldDefinition.range(area.toInterval3)
      val surfaceNotInArea = !interval(0)

      if( surfaceNotInArea ) {
        if( Config.predictionDebug )
          debugLog ! Predicted(area)
        val meshNode = new MeshNode(Leaf(
          if(interval.isPositive) FullHexaeder else EmptyHexaeder
        ))
        meshNode.mesh = TextureMesh.empty
        gameLoop ! FinishedGeneratingJob(area, meshNode)
      }
      // if the area is too big, it will be splitted
      else if( area.size/2 >= Config.minPredictionSize ) {
        val data = Array.fill[NodeUnderMesh](8)(UngeneratedNode)
        val meshNode = new MeshNode(new InnerNodeUnderMesh(data))
        meshNode.mesh = TextureMesh.empty
        gameLoop ! FinishedGeneratingJob(area, meshNode)
      }
      // sample the whole area
      else {
        val meshNode = WorldGenerator generateNode area
        gameLoop ! FinishedGeneratingJob(area, meshNode)
      }
  }
}


