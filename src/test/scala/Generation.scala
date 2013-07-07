import downearth.benchmark.TestingWorldDefinition
import downearth.generation.WorldGenerator._
import downearth.worldoctree.{Cuboid, Array3D, PowerOfTwoCube}
import org.scalatest.FunSuite
import simplex3d.math._
import simplex3d.math.double._
import downearth.util._

class Generation extends FunSuite {

  val positions = Seq(Vec3i(0),Vec3i(-3,4,12))
  val sizes = Vec3i(1) until Vec3i(6)

  test("find nodes to sample (volume, coverage)"){
    for( size <- sizes; pos <- positions ) {
      val area = Cuboid(pos, size)
      val (toSample, positive, negative) = findNodesToSample(area, TestingWorldDefinition, 1)
      //println("sampling ratio: " + (toSample.map(_.volume).sum.toDouble/area.volume))
      assert((toSample ++ positive ++ negative).map(_.volume).sum === area.volume)
      assert((toSample ++ positive ++ negative).flatMap(_.coordinates).toSet === area.coordinates.toSet)
    }
  }

  test("find nodes to sample (data3d fill coverage)"){
    for( size <- sizes; pos <- positions ) {
      val area = Cuboid(pos, size)
      val (toSample, positive, negative) = findNodesToSample(area, TestingWorldDefinition, 1)
      val data = new Array3D[Int](area.vsize)
      data.fill(v => 1)
      data.fill(v => 2, toSample ++ positive ++ negative, offset = -area.pos)
      assert( data.data === Array.fill(area.volume)(2) )
    }
  }
}
