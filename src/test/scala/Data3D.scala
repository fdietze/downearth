import downearth.worldoctree.{NodeInfo, Array3D}
import org.scalatest.FunSuite
import simplex3d.math._
import simplex3d.math.double._

class Data3D extends FunSuite {

  test("fill"){
    val a = new Array3D[Int](Vec3i(4))
    a.fill((v) => 1)
    assert( a.data === Array.fill(4*4*4)(1) )
  }

  test("fill (areas)"){
    val a = new Array3D[Int](Vec3i(4))
    a.fill((v) => 1, Seq(NodeInfo(Vec3i(5),4)), offset = Vec3i(-5))
    assert( a.data === Array.fill(4*4*4)(1) )
  }

  test("fill border"){
    val a = new Array3D[Int](Vec3i(3))
    a.fill((v) => 1)
    a.fillBorder(a.index)
    assert( a.data === Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,  1,  14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26) )
  }

  test("fill without border (areas)"){
    val area = NodeInfo(Vec3i(1234),1)
    val a = new Array3D[Int](Vec3i(3))
    a.fill((v) => 2)
    a.fill(a.index, Seq(area), offset=Vec3i(-area.pos + 1))
    assert( a.data === Array(2,2,2,2,2,2,2,2,2, 2,2,2,2, 13 ,2,2,2,2, 2,2,2,2,2,2,2,2,2) )
  }

  test("extract"){
    val a = new Array3D[Int](Vec3i(3), Array.range(0,3*3*3))
    assert(a.extract(Vec3i(0)).toSeq === Seq(0, 1, 3, 4, 9, 10, 12, 13))
    assert(a.extract(Vec3i(1)).toSeq === Seq(13, 14, 16, 17, 22, 23, 25, 26))
  }

  test("toString"){
    val a = new Array3D[Int](Vec3i(2))
    a.fill(v => v.x*100+v.y*10+v.z+9000)
    assert(a.toString ===
      """9000, 9100
        |9010, 9110
        |
        |9001, 9101
        |9011, 9111""".stripMargin)
  }
}
