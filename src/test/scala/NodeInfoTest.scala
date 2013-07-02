import downearth.worldoctree.NodeInfo
import org.scalatest.FunSuite
import simplex3d.math._
import simplex3d.math.double._

class NodeInfoTest extends FunSuite {

  test("child creation by index"){
    val nodeInfo = NodeInfo(pos = Vec3i(0), size = 2)
    assert( nodeInfo(0) === NodeInfo(Vec3i(0, 0, 0), 1) )
    assert( nodeInfo(1) === NodeInfo(Vec3i(1, 0, 0), 1) )
    assert( nodeInfo(2) === NodeInfo(Vec3i(0, 1, 0), 1) )
    assert( nodeInfo(3) === NodeInfo(Vec3i(1, 1, 0), 1) )
    assert( nodeInfo(4) === NodeInfo(Vec3i(0, 0, 1), 1) )
    assert( nodeInfo(5) === NodeInfo(Vec3i(1, 0, 1), 1) )
    assert( nodeInfo(6) === NodeInfo(Vec3i(0, 1, 1), 1) )
    assert( nodeInfo(7) === NodeInfo(Vec3i(1, 1, 1), 1) )
  }

  test("traversalOrder 1"){
    val nodeInfo = NodeInfo(pos = Vec3i(0), size = 2)
    val camera = Vec3(-0.5, 0.5, -0.5)
    assert( nodeInfo.traversalOrder(camera) === Array(2,6,0,4, 3,7,1,5) )
  }

  test("traversalOrder 2"){
    val nodeInfo = NodeInfo(pos = Vec3i(0), size = 2)
    val camera = Vec3(1.5, -10, 1.5)
    assert( nodeInfo.traversalOrder(camera) === Array(5,1,7,3, 4,0,6,2) )
  }
}
