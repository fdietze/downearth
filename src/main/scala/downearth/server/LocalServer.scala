package downearth.server

import akka.actor.{Actor, Props, ActorSystem}
import simplex3d.math.Vec3i
import collection.mutable
import downearth.util._
import downearth.worldoctree.{FullHexaeder, Polyeder, EmptyHexaeder}
import java.io.{FileInputStream, File, FileOutputStream}

package object message {
  case class Block(shape:Polyeder, mat:Int)
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

  val file = new File("worlddelta.save")
  val worldDelta = new mutable.HashMap[Vec3i,Block]

  override def preStart() {
    load()
  }

  //TODO: Distribute World Definition
  def receive = {
    // client queries for a set of changes
    case GetDeltaInRange(rangePos, rangeSize) =>
      sender ! DeltaSet(worldDelta.filter {
        case (pos, _) => indexInRange(pos, rangePos, rangeSize)
      }.toMap)

      /* //Test Data:
      sender ! DeltaSet((rangePos until (rangePos+rangeSize)).collect{
        case v@Vec3i(x,y,z) if (x+y+z) % 2 == 0 => (v,Block(EmptyHexaeder,0))
      }.toMap)*/

    // client notifies about changes in the world
    case Delta(pos,block) => worldDelta += (pos -> block)

    case m => println("Server: Unknown Message: " + m)
  }

  override def postStop {
    save()
    println("Server: Stopped")
  }

  def serializeDelta(deltas:mutable.HashMap[Vec3i,Block]) = {
    import downearth.message.implicits._
    import downearth.{message => m}

    m.WorldDelta(deltas.map{
      case (pos, Block(shape,material)) =>
        m.Delta(pos, m.Polyeder(shape != EmptyHexaeder), material)
    }.toVector)
  }

  def deserializeDelta(worldDelta:downearth.message.WorldDelta) = {
    import downearth.message.implicits._
    import downearth.{message => m}
    worldDelta.set map {
      case m.Delta(pos,shape,material) =>
        ( messageToSimplexVec3i(pos),
          Block(if(shape.full) FullHexaeder else EmptyHexaeder, material)
          )
    }
  }

  def load() {
    import downearth.{message => m}
    val deltas= try{
      val fis = new FileInputStream(file)
      deserializeDelta(m.WorldDelta().mergeFrom(fis))
    } catch {
      case e:Exception =>
        println(e.getMessage)
        println("couldn't open file: " + file)
        Vector()
    }

    worldDelta.clear()
    worldDelta ++= deltas
  }

  def save() {
    println("Server: Saving World")

    try{
      val fos = new FileOutputStream(file)
      fos.write(serializeDelta(worldDelta).toByteArray)
    } catch {
      case e:Exception =>
        println(e.getMessage)
        println("couldn't save file: " + file)
    }
  }
}
