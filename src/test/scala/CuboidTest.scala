import downearth.worldoctree.{Cuboid, Cube, PowerOfTwoCube}
import org.scalatest.FunSuite
import simplex3d.math._
import simplex3d.math.double._
import downearth.util._

class CuboidTest extends FunSuite {
  val positions = Seq(Vec3i(0),Vec3i(-3,4,12))

  test("PowerOfTwoCube: child creation by index") {
    val sizes = Seq(2,4,8,16,32)
    for( size <- sizes; pos <- positions ) {
      val cube = PowerOfTwoCube(pos, size)
      val a = size/2
      assert( cube(0) === PowerOfTwoCube(pos+Vec3i(0, 0, 0), a))
      assert( cube(1) === PowerOfTwoCube(pos+Vec3i(a, 0, 0), a))
      assert( cube(2) === PowerOfTwoCube(pos+Vec3i(0, a, 0), a))
      assert( cube(3) === PowerOfTwoCube(pos+Vec3i(a, a, 0), a))
      assert( cube(4) === PowerOfTwoCube(pos+Vec3i(0, 0, a), a))
      assert( cube(5) === PowerOfTwoCube(pos+Vec3i(a, 0, a), a))
      assert( cube(6) === PowerOfTwoCube(pos+Vec3i(0, a, a), a))
      assert( cube(7) === PowerOfTwoCube(pos+Vec3i(a, a, a), a))
    }
  }

  test("Cube: child creation by index"){
    val sizes = 2 until 32
    for( size <- sizes; pos <- positions ) {
      val cube = Cube(pos, size)
      val (a,b) = halves(size)
      assert( cube(0) === Cuboid(pos+Vec3i(0, 0, 0), Vec3i(a,a,a)))
      assert( cube(1) === Cuboid(pos+Vec3i(a, 0, 0), Vec3i(b,a,a)))
      assert( cube(2) === Cuboid(pos+Vec3i(0, a, 0), Vec3i(a,b,a)))
      assert( cube(3) === Cuboid(pos+Vec3i(a, a, 0), Vec3i(b,b,a)))
      assert( cube(4) === Cuboid(pos+Vec3i(0, 0, a), Vec3i(a,a,b)))
      assert( cube(5) === Cuboid(pos+Vec3i(a, 0, a), Vec3i(b,a,b)))
      assert( cube(6) === Cuboid(pos+Vec3i(0, a, a), Vec3i(a,b,b)))
      assert( cube(7) === Cuboid(pos+Vec3i(a, a, a), Vec3i(b,b,b)))
    }
  }

  test("Cuboid: child creation by index"){
    val sizes = Vec3i(2) until Vec3i(32)
    for( size <- sizes; pos <- positions ) {
      val cuboid = Cuboid(pos, size)
      val (a,b) = halves(size)
      assert( cuboid(0) === Cuboid(pos+Vec3i(0  , 0  , 0  ), Vec3i(a.x,a.y,a.z)))
      assert( cuboid(1) === Cuboid(pos+Vec3i(a.x, 0  , 0  ), Vec3i(b.x,a.y,a.z)))
      assert( cuboid(2) === Cuboid(pos+Vec3i(0  , a.y, 0  ), Vec3i(a.x,b.y,a.z)))
      assert( cuboid(3) === Cuboid(pos+Vec3i(a.x, a.y, 0  ), Vec3i(b.x,b.y,a.z)))
      assert( cuboid(4) === Cuboid(pos+Vec3i(0  , 0  , a.z), Vec3i(a.x,a.y,b.z)))
      assert( cuboid(5) === Cuboid(pos+Vec3i(a.x, 0  , a.z), Vec3i(b.x,a.y,b.z)))
      assert( cuboid(6) === Cuboid(pos+Vec3i(0  , a.y, a.z), Vec3i(a.x,b.y,b.z)))
      assert( cuboid(7) === Cuboid(pos+Vec3i(a.x, a.y, a.z), Vec3i(b.x,b.y,b.z)))
    }
  }

  test("split (power of 2, volume)"){
    val nodeInfo = PowerOfTwoCube(pos = Vec3i(-5), size = 4)
    assert( nodeInfo.splitOct.map(_.volume).sum === nodeInfo.volume )
  }

  test("split (power of 2, coverage)"){
    val nodeInfo = PowerOfTwoCube(pos = Vec3i(-5), size = 4)
    assert( nodeInfo.splitOct.flatMap(_.coordinates).toSet === nodeInfo.coordinates.toSet )
  }

  /*test("split (not power of 2, volume)"){
    val nodeInfo = Cube(pos = Vec3i(-5), size = 7)
    assert( nodeInfo.split.map(_.volume).sum === nodeInfo.volume )

    val nodeInfo2 = Cube(pos = Vec3i(-5), size = 17)
    assert( nodeInfo2.split.map(_.volume).sum === nodeInfo2.volume )
  }

  test("split (not power of 2, coverage)"){
    val nodeInfo = Cube(pos = Vec3i(-5), size = 7)
    assert( nodeInfo.split.flatMap(_.coordinates).toSet === nodeInfo.coordinates.toSet )

    val nodeInfo2 = Cube(pos = Vec3i(-5), size = 17)
    assert( nodeInfo2.split.flatMap(_.coordinates).toSet === nodeInfo2.coordinates.toSet )
  }*/

  test("traversalOrder 1"){
    val cube = PowerOfTwoCube(pos = Vec3i(0), size = 2)
    val camera = Vec3(-0.5, 0.5, -0.5)
    assert( cube.traversalOrder(camera) === Array(2,6,0,4, 3,7,1,5) )
  }

  test("traversalOrder 2"){
    val cube = PowerOfTwoCube(pos = Vec3i(0), size = 2)
    val camera = Vec3(1.5, -10, 1.5)
    assert( cube.traversalOrder(camera) === Array(5,1,7,3, 4,0,6,2) )
  }

}
