package downearth.server

import akka.actor.{Actor, Props, ActorSystem}
import simplex3d.math.Vec3i
import collection.mutable
import downearth.util._
import java.io.{FileInputStream, File, FileOutputStream}
import downearth.message._
import downearth.message.implicits._

object LocalServer {
  val actorSystem = ActorSystem.create("network")
  val server = actorSystem.actorOf( Props[LocalServer] )
}

class LocalServer extends Actor {
  val file = new File("worlddelta.save")
  val deltaSet = new mutable.HashMap[Vec3i,Block] {
    def +=(d:Delta) {
      update(d.pos, d.block)
    }
    def apply(range:NodeInfo) = DeltaSet(this.collect {
      case (pos, block) if indexInRange(pos, range.pos, range.size) => Delta(pos,block)
    }.toVector)

    def toMessage = {
      DeltaSet(this.map{ case (pos, block) => Delta(pos, block) }.toVector)
    }

    def fromMessage(m:DeltaSet) {
      this.clear()
      this ++= m.set map { case Delta(pos, block) => (messageToSimplexVec3i(pos) -> block) }
    }
  }

  override def preStart() {
    load()
  }

  //TODO: Distribute World Definition
  def receive = {
    // client asks for a set of changes
    case range:NodeInfo =>
      sender ! deltaSet(range)

    // client notifies about changes in the world
    //TODO: check if change represents density function, if yes, delete it
    case d:Delta => deltaSet += d

    // unknown message
    case m => println("Server: Unknown Message: " + m)
  }

  override def postStop() {
    save()
    println("Server: Stopped")
  }

  def load() {
    println("Server: Loading World Deltas")
    try{
      if( file.exists() ) {
        val fis = new FileInputStream(file)
        val message = DeltaSet().mergeFrom(fis)
        //println(message)
        deltaSet.fromMessage(message)
      }
    } catch {
      case e:Exception =>
        e.printStackTrace()
        println("couldn't open file: " + file)
    }
  }

  def save() {
    println("Server: Saving World Deltas")
    try{
      val fos = new FileOutputStream(file)
      fos.write(deltaSet.toMessage.toByteArray)
    } catch {
      case e:Exception =>
        e.printStackTrace()
        println("couldn't save file: " + file)
    }
  }
}
