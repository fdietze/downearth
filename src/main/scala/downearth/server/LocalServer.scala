package downearth.server

import akka.actor.{Actor, Props, ActorSystem}
import simplex3d.math.Vec3i
import collection.mutable
import downearth.util._
import downearth.worldoctree.{Polyeder, EmptyHexaeder}

case class Block(shape:Polyeder, mat:Int)

package object message {
  case class GetDeltaInRange(pos:Vec3i, size:Int)
  case class DeltaSet(delta:Map[Vec3i,Block])
  case class Delta(pos:Vec3i, block:Block)
}

object LocalServer {
  val actorSystem = ActorSystem.create("network")
  val server = actorSystem.actorOf( Props[LocalServer] )
}

class LocalServer extends Actor {
  import message._

  val worldDelta = new mutable.HashMap[Vec3i,Block]
  //TODO: read Data from File


  //TODO: Distribute World Definition
  def receive = {
    case GetDeltaInRange(rangePos, rangeSize) =>
      sender ! DeltaSet(worldDelta.filter {
        case (pos, _) => indexInRange(pos, rangePos, rangeSize)
      }.toMap)
      /*
      Test Data:
      sender ! DeltaSet((rangePos until (rangePos+rangeSize)).collect{
        case v@Vec3i(x,y,z) if (x+y+z) % 2 == 0 => (v,Block(EmptyHexaeder,0))
      }.toMap)*/

    case Delta(pos,block) => worldDelta += (pos -> block)

    case m => println("Server: Unknown Message: " + m)
  }

  override def postStop {
    println("Server: Stopped")
  }
}
