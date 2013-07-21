import downearth.rendering.Update
import java.nio.ByteBuffer
import org.lwjgl.BufferUtils
import org.scalatest.FunSuite
import scala._
import simplex3d.math._
import downearth.util._

class Mesh extends FunSuite {
  test("buffer split"){
    val a = BufferUtils.createByteBuffer(15)
    a.position(5)
    a.limit(10)

    val (b,c) = a.split(2)

    assert(b.position === 5)
    assert(b.limit === 7)

    assert(c.position === 7)
    assert(c.limit === 10)

    assert(a.position === 5)
    assert(a.limit === 10)
  }

  test("buffer list split (one element)"){
    val a = BufferUtils.createByteBuffer(15)
    assert(a.position === 0)
    assert(a.limit === 15)
    val l:List[ByteBuffer] = List(a)

    val (left,right) = l.split(10)

    assert(left.head.position === 0)
    assert(left.head.limit === 10)

    assert(right.head.position === 10)
    assert(right.head.limit === 15)

    assert(a.position === 0)
    assert(a.limit === 15)
  }

  test("buffer list split (second element)"){
    val data = BufferUtils.createByteBuffer(20)
    val a = data.duplicate
    val b = data.duplicate
    a.position(0); a.limit(5)
    b.position(10); b.limit(20)
    val l:List[ByteBuffer] = List(a,b)

    val (left,right) = l.split(10)

    assert(left.size === 2)
    assert(right.size === 1)

    assert(left(0).position === 0)
    assert(left(0).limit === 5)
    assert(left(1).position === 10)
    assert(left(1).limit === 15)

    assert(right(0).position === 15)
    assert(right(0).limit === 20)


    assert(a.position === 0)
    assert(a.limit === 5)
    assert(b.position === 10)
    assert(b.limit === 20)
  }

  implicit class ByteBufferToArray(buffer:ByteBuffer) {
    def toArray = {
      val a = new Array[Byte](buffer.width)
      buffer.get(a)
      a
    }
  }

  test("make updates dependent") {
    val u1 = Update(5, 5, BufferUtils.createByteBuffer(0)) //DELETE
    val u2d = BufferUtils.createByteBuffer(10)
    u2d.put(Array[Byte](0,1,2,3,4,5,6,7,8,9))
    u2d.flip()
    val u2 = Update(10, 5, u2d) //REPLACE
    val u3 = Update(20, 0, u2d) //INSERT

    val updates = List(u1,u2,u3)
    val dep = Update.makeDependent(updates)

    assert( dep(0) === updates(0) )
    assert( dep(1) === updates(1).copy(byteOffset = 5) )
    assert( dep(2) === updates(2) )
  }

  test("apply updates (simple replace)"){
    val data = BufferUtils.createByteBuffer(15)
    data.put(Array[Byte](0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))
    data.flip()

    val u1d = BufferUtils.createByteBuffer(10)
    u1d.put(Array[Byte](0,1,2,3,4,5,6,7,8,9))
    u1d.flip()
    val u1 = Update(5, 5, u1d)

    val newData = data.applyUpdates(List(u1))

    assert(
      newData.toArray.toList.map(_.toInt) ===
        List(0,1,2,3,4,  0,1,2,3,4,5,6,7,8,9,  10,11,12,13,14)
    )
  }

  test("apply updates (delete then replace)"){
    val data = BufferUtils.createByteBuffer(15)
    data.put(Array[Byte](0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))
    data.flip()


    val u1 = Update(5, 5, BufferUtils.createByteBuffer(0)) //DELETE
    val u2d = BufferUtils.createByteBuffer(10)
    u2d.put(Array[Byte](0,1,2,3,4,5,6,7,8,9))
    u2d.flip()
    val u2 = Update(10, 5, u2d) //REPLACE

    val newData = data.applyUpdates(List(u1, u2))

    assert(
      newData.toArray.toList.map(_.toInt) ===
        List(0,1,2,3,4,  0,1,2,3,4,5,6,7,8,9)
    )
  }
}
