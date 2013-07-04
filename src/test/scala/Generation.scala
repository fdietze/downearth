import downearth.benchmark.TestingWorldDefinition
import downearth.generation.WorldGenerator._
import downearth.worldoctree.{Array3D, NodeInfo}
import org.scalatest.FunSuite
import simplex3d.math._
import simplex3d.math.double._

class Generation extends FunSuite {

  test("find nodes to sample (volume)"){
    val area = NodeInfo(Vec3i(140),32)
    val (toSample, pos, neg) = findNodesToSample(area, TestingWorldDefinition, 1)
    //println("sampling ratio: " + (toSample.map(_.volume).sum.toDouble/area.volume))
    assert((toSample ++ pos ++ neg).map(_.volume).sum === area.volume)
  }

  test("find nodes to sample (coverage)"){
    val area = NodeInfo(Vec3i(6745),32)
    val (toSample, pos, neg) = findNodesToSample(area, TestingWorldDefinition, 1)
    assert((toSample ++ pos ++ neg).flatMap(_.coordinates).toSet === area.coordinates.toSet)
  }

  test("find nodes to sample (data3d fill coverage)"){
    val area = NodeInfo(Vec3i(-345),32)
    val (toSample, pos, neg) = findNodesToSample(area, TestingWorldDefinition, 1)
    assert((toSample ++ pos ++ neg).map(_.volume).sum === area.volume)
    val data = new Array3D[Int](area.vsize)
    data.fill(v => 1)
    data.fill(v => 2, toSample ++ pos ++ neg, offset = -area.pos)
    assert( data.data === Array.fill(area.volume)(2) )
  }

  test("predicted fill (sign)"){
    val area = NodeInfo(Vec3i(20),1)
    val data1 = sample(area, TestingWorldDefinition)
    val data2 = samplePredicted(area, TestingWorldDefinition)
    /*println(data1.toStringRounded(1))
    println("-"*20)
    val (toSample, pos, neg) = findNodesToSample(area, TestingWorldDefinition)
    println(toSample, pos, neg)
    println(data2.toStringRounded(1).replace("0.0","   "))
    //assert(data1.data.map(_.signum) === data2.data.map(_.signum))*/
  }
}
