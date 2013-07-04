package downearth

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import simplex3d.data._
import simplex3d.data.double._

import Config._
import downearth.worldoctree.PowerOfTwoCube
import org.lwjgl.opengl.Display
import org.lwjgl.BufferUtils
import simplex3d.math.floatx.{Mat4f, ReadMat4f}
import java.nio.FloatBuffer

abstract class Camera {
	var position:ReadVec3
  var directionQuat:ReadQuat4

  def direction:Vec3 = directionQuat.rotateVector( -Vec3.UnitZ )

  def projection:Mat4
  def view:Mat4
  def noTranslate:Mat4

//  protected val m_viewBuffer = DataBuffer[Mat4,RFloat](1)
//  protected val m_projectionBuffer = DataBuffer[Mat4,RFloat](1)

  val m_viewBuffer = BufferUtils.createFloatBuffer(16)
  val m_projectionBuffer = BufferUtils.createFloatBuffer(16)

  def putMat(buffer:FloatBuffer, m:Mat4) {
    buffer.put(m.m00.toFloat)
    buffer.put(m.m01.toFloat)
    buffer.put(m.m02.toFloat)
    buffer.put(m.m03.toFloat)

    buffer.put(m.m10.toFloat)
    buffer.put(m.m11.toFloat)
    buffer.put(m.m12.toFloat)
    buffer.put(m.m13.toFloat)

    buffer.put(m.m20.toFloat)
    buffer.put(m.m21.toFloat)
    buffer.put(m.m22.toFloat)
    buffer.put(m.m23.toFloat)

    buffer.put(m.m30.toFloat)
    buffer.put(m.m31.toFloat)
    buffer.put(m.m32.toFloat)
    buffer.put(m.m33.toFloat)
  }

  def viewBuffer = {
    putMat(m_viewBuffer, view)
    m_viewBuffer.flip()
    m_viewBuffer
  }

  def projectionBuffer = {
    putMat(m_projectionBuffer, projection)
    m_projectionBuffer.flip()
    assert(m_projectionBuffer.position() == 0 && m_projectionBuffer.limit() == 16, m_projectionBuffer)
    m_projectionBuffer
  }
}

// Eine Kamera, die frei im Raum bewegt werden kann
class Camera3D(val _position:ReadVec3,val _directionQuat:ReadQuat4) extends Camera {
  val position = Vec3(_position)
  val directionQuat = Quat4(_directionQuat)

  def position_=(newPos:ReadVec3) {
    position := newPos
  }

  def directionQuat_=(newDir:ReadQuat4) {
    directionQuat := newDir
  }

	def this (positionVec:Vec3,directionVec:Vec3) = this(positionVec,quaternion(lookAt(-directionVec,worldUpVector)))
	
	def camera = this

	def rotateVector(v:Vec3) = directionQuat.rotateVector(v)
	
	def move(delta:Vec3) {
		position += rotateVector(delta)
	}
	
	def rotate(rot:Vec3) {
		directionQuat *= Quat4 rotateX(rot.x) rotateY(rot.y) rotateZ(rot.z)
	}

	// rotiert die Kamera, damit der worldUpVector auch für die Kamera oben ist
	def lerpUp(factor:Double) {
		val up = inverse(directionQuat) rotateVector worldUpVector
		val α = atan(up.y, up.x) - Pi/2
		
		directionQuat *= Quat4 rotateZ(α*pow(factor, 1.5))
	}

  object State extends Enumeration {
    val RightEye, LeftEye, Center = Value
  }

  var eyeState = State.Center

  def rightEye() {
    eyeState = State.RightEye
  }
  def leftEye() {
    eyeState = State.LeftEye
  }

	override def projection:Mat4 = {
    // TODO implement viewport

    // aspect Ratio
    val v = Display.getWidth.toDouble / Display.getHeight.toDouble

    import Config.test

    eyeState match {
      case State.RightEye =>
        downearth.util.projection(l = -v - test, r = v - test, b = -1, t = 1, n = 1, f = 1000)
      case State.LeftEye  =>
        downearth.util.projection(l = -v + test, r = v + test, b = -1, t = 1, n = 1, f = 1000)
      case State.Center   =>
        downearth.util.projection(l = -v,       r = v,       b = -1, t = 1, n = 1, f = 1000)
    }

	}

	override def view:Mat4 = Mat4(inverse(Mat4x3 rotate(directionQuat) translate(position)))
  override def noTranslate:Mat4 = Mat4(inverse(Mat4x3 rotate(directionQuat)))
}

trait FrustumTest extends Function1[PowerOfTwoCube,Boolean] {
	def testNode( info:PowerOfTwoCube ):Boolean
  def apply( info:PowerOfTwoCube ) = testNode(info)
}

// Frustum Culling
// Idea by Mark Morley, http://web.archive.org/web/20030601123911/http://www.markmorley.com/opengl/frustumculling.html
class FrustumTestImpl(projection:Mat4, view:Mat4) extends FrustumTest {

	val planes = Array(Vec4(0),Vec4(0),Vec4(0),Vec4(0),Vec4(0),Vec4(0))
	val rows = transpose(projection * view)
	planes(0) = normalize(rows(3) - rows(0)) //right plane
	planes(1) = normalize(rows(3) + rows(0)) //left plane
	planes(2) = normalize(rows(3) + rows(1)) //bottom plane
	planes(3) = normalize(rows(3) - rows(1)) //top plane
	planes(4) = normalize(rows(3) - rows(2)) //far plane
	planes(5) = normalize(rows(3) + rows(2)) //near plane

	def testNode( info:PowerOfTwoCube ):Boolean = {
		val inside = testCube(Vec3(info.pos + info.size / 2), info.size / 2)
		return inside
	}
	
	private def testCube( pos:Vec3, radius:Double ):Boolean = {
		// TODO: Give the information, if a cube is completely in the frustum
		var p = 0
		import pos.{x,y,z}
		while( p < 6 )
		{
			if( planes(p).x * (x - radius) + planes(p).y * (y - radius) + planes(p).z * (z - radius) + planes(p).w <= 0 )
			if( planes(p).x * (x + radius) + planes(p).y * (y - radius) + planes(p).z * (z - radius) + planes(p).w <= 0 )
			if( planes(p).x * (x - radius) + planes(p).y * (y + radius) + planes(p).z * (z - radius) + planes(p).w <= 0 )
			if( planes(p).x * (x + radius) + planes(p).y * (y + radius) + planes(p).z * (z - radius) + planes(p).w <= 0 )
			if( planes(p).x * (x - radius) + planes(p).y * (y - radius) + planes(p).z * (z + radius) + planes(p).w <= 0 )
			if( planes(p).x * (x + radius) + planes(p).y * (y - radius) + planes(p).z * (z + radius) + planes(p).w <= 0 )
			if( planes(p).x * (x - radius) + planes(p).y * (y + radius) + planes(p).z * (z + radius) + planes(p).w <= 0 )
			if( planes(p).x * (x + radius) + planes(p).y * (y + radius) + planes(p).z * (z + radius) + planes(p).w <= 0 )
				return false
			p += 1
		}
		true
	}
}
