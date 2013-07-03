package downearth.benchmark

import downearth.util.Timer
import simplex3d.math._
import simplex3d.math.double._
import downearth.worldoctree.NodeInfo
import downearth.generation.WorldFunction
import downearth.generation.WorldGenerator.genWorldAt

object Generation {

  def FullGeneration() {
    println("Full Generation")
    val timer = new Timer
    timer.start()
    /*genWorldAt(NodeInfo(pos=Vec3i(0),size=16),
               worldFunction = WorldDefinition)*/
    timer.stop()
    println(s"${timer.read}s")
  }
}
