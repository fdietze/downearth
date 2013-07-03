package downearth.benchmark

import downearth.util.Timer
import simplex3d.math._
import simplex3d.math.double._
import downearth.worldoctree.NodeInfo

object MicroBenchmarks {

  def benchmark(n:Int)(code: => Unit) = {
    val timer = new Timer
    var i = 0
    timer.start()
    while( i < n ) {
      code
      i += 1
    }
    timer.stop()
    timer.read / n
  }


  def nodeInfoTraversal() {
    println("NodeInfo Traversal")
    
    val nodeInfo = NodeInfo(Vec3i(-1),2)
    val camera = Vec3(10,20,30)
    
    val duration = benchmark(100000000) {
      nodeInfo.traversalOrder(camera)
    }
    
    println(s"${duration}s")
  }
}
