package downearth.benchmark

import downearth.util.Timer
import simplex3d.math._
import simplex3d.math.double._
import downearth.worldoctree.PowerOfTwoCube

object Octree {
  def nodeInfoTraversal() {
    println("Cube Traversal")
    val timer = new Timer
    
    val nodeInfo = PowerOfTwoCube(Vec3i(-1),2)
    val camera = Vec3(10,20,30)

    timer.benchmark(1000000) {
      nodeInfo.traversalOrder(camera)
    }
    
    println(s"${timer.read}s")
  }
}
