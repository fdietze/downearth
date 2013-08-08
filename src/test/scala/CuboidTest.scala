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

  test("PowerOfTwoCube: splitOct (volume, coverage)") {
    val sizes = Seq(2,4,8,16,32)
    for( size <- sizes; pos <- positions ) {
      val cube = PowerOfTwoCube(pos, size)
      assert( cube.splitOct.map(_.volume).sum === cube.volume )
      assert( cube.splitOct.flatMap(_.coordinates).toSet === cube.coordinates.toSet )
    }
  }

  test("Cube: splitOct (volume, coverage)") {
    val sizes = 2 until 17
    for( size <- sizes; pos <- positions ) {
      val cube = Cube(pos, size)
      assert( cube.splitOct.map(_.volume).sum === cube.volume )
      assert( cube.splitOct.flatMap(_.coordinates).toSet === cube.coordinates.toSet )
    }
  }

  test("Cuboid: splitOct (volume, coverage)") {
    val sizes = Vec3i(2) until Vec3i(10)
    for( size <- sizes; pos <- positions ) {
      val cuboid = Cuboid(pos, size)
      assert( cuboid.splitOct.map(_.volume).sum === cuboid.volume )
      assert( cuboid.splitOct.flatMap(_.coordinates).toSet === cuboid.coordinates.toSet )
    }
  }

  test("traversalOrder 1"){
    val cube = PowerOfTwoCube(pos = Vec3i(0), size = 2)
    val camera = Vec3(-0.5, 0.5, -0.5)
    assert( cube.traversalOrder(camera) === Array(0,4,2,6, 1,5,3,7) )
  }

  test("traversalOrder 2"){
    val cube = PowerOfTwoCube(pos = Vec3i(0), size = 2)
    val camera = Vec3(1.5, -10, 1.5)
    assert( cube.traversalOrder(camera) === Array(5,1,7,3, 4,0,6,2) )
  }

  test("intersection"){
    assert( Cuboid(Vec3i(-2), Vec3i(4)).intersection(Cuboid(Vec3i(-1), Vec3i(1))) === Cuboid(Vec3i(-1), Vec3i(1)) )
    assert( Cuboid(Vec3i(-1), Vec3i(2)).intersection(Cuboid(Vec3i(0), Vec3i(2))) === Cuboid(Vec3i(0), Vec3i(1)) )
    assert( Cuboid(Vec3i(0), Vec3i(2)).intersection(Cuboid(Vec3i(-1), Vec3i(2))) === Cuboid(Vec3i(0), Vec3i(1)) )
  }

  test("overlap") {
    assert( Cube(Vec3i(1,1,1),2) overlaps Cube(Vec3i(1,1,1),2) )

    assert( Cube(Vec3i(1,1,1),2) overlaps Cube(Vec3i(1,1,2),2) )
    assert( Cube(Vec3i(1,1,1),2) overlaps Cube(Vec3i(1,2,1),2) )
    assert( Cube(Vec3i(1,1,1),2) overlaps Cube(Vec3i(2,1,1),2) )

    assert( Cube(Vec3i(1,1,1),2) overlaps Cube(Vec3i(1,1,0),2) )
    assert( Cube(Vec3i(1,1,1),2) overlaps Cube(Vec3i(1,0,1),2) )
    assert( Cube(Vec3i(1,1,1),2) overlaps Cube(Vec3i(0,1,1),2) )

    assert( Cube(Vec3i(1,1,1),2) overlaps Cube(Vec3i(0,0,0),2) )
    assert( Cube(Vec3i(1,1,1),2) overlaps Cube(Vec3i(2,2,2),2) )

    assert( Cube(Vec3i(-2,-2,-2),4) overlaps Cube(Vec3i(-1,-1,-1),2) )
    assert( Cube(Vec3i(-1,-1,-1),2) overlaps Cube(Vec3i(-2,-2,-2),4) )

    assert( !( Cube(Vec3i(1,1,1),2) overlaps Cube(Vec3i(4,4,4),2) ))
    assert( !( Cube(Vec3i(4,4,4),2) overlaps Cube(Vec3i(1,1,1),2) ))

    assert( !(Cube(Vec3i(-32,-32,32),32) overlaps Cube(Vec3i(-5, -5, 0),10)) )
    assert( !(PowerOfTwoCube(0,-32,32,32) overlaps Cube(Vec3i(-5, -5, 0),10)) )
    assert( !(PowerOfTwoCube(-32,0,32,32) overlaps Cube(Vec3i(-5, -5, 0),10)) )
    assert( !(PowerOfTwoCube(0,0,32,32) overlaps Cube(Vec3i(-5, -5, 0),10)) )
    assert( !(PowerOfTwoCube(-64,-64,64,64) overlaps Cube(Vec3i(-5, -5, 0),10)) )  }
}
