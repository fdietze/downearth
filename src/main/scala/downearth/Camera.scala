package downearth

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import Config._
import downearth.worldoctree.{Frustum, Cone, PowerOfTwoCube}
import org.lwjgl.opengl.Display
import org.lwjgl.BufferUtils
import simplex3d.math.floatx.{Mat4f, ReadMat4f}
import glwrapper.util.putMat4f
import java.nio.FloatBuffer

abstract class Camera {
	var position:ReadVec3
  var directionQuat:ReadQuat4

  def direction:Vec3 = directionQuat.rotateVector( -Vec3.UnitZ )

  def projection:Mat4f
  def view:Mat4f
  def noTranslate:Mat4f

//  protected val m_viewBuffer = DataBuffer[Mat4,RFloat](1)
//  protected val m_projectionBuffer = DataBuffer[Mat4,RFloat](1)

  val m_viewBuffer = BufferUtils.createFloatBuffer(16)
  val m_projectionBuffer = BufferUtils.createFloatBuffer(16)

  def viewBuffer = {
    putMat4f(m_viewBuffer, view)
    m_viewBuffer.flip()
    m_viewBuffer
  }

  def projectionBuffer = {
    putMat4f(m_projectionBuffer, projection)
    m_projectionBuffer.flip()
    assert(m_projectionBuffer.position() == 0 && m_projectionBuffer.limit() == 16, m_projectionBuffer)
    m_projectionBuffer
  }

  def cone = {
    val screenRatio = Display.getWidth / Display.getHeight
    val near  = Config.nearPlane
    val right = screenRatio * near
    val top   = near
    val halfDiagonal = sqrt((right * right) + (top * top))
    val angle = atan(halfDiagonal/near)

    Cone(position, direction, angle)
  }

  def frustum = {
    val planes = new Array[Vec4](6)
    val rows = transpose(Mat4(projection) * Mat4(view))
    planes(0) = normalize(rows(3) - rows(0)) //right plane
    planes(1) = normalize(rows(3) + rows(0)) //left plane
    planes(2) = normalize(rows(3) + rows(1)) //bottom plane
    planes(3) = normalize(rows(3) - rows(1)) //top plane
    planes(4) = normalize(rows(3) - rows(2)) //far plane
    planes(5) = normalize(rows(3) + rows(2)) //near plane

    Frustum(planes)
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

	override val projection :Mat4f = glwrapper.util.simpleProjectionF(f = Config.farPlane.toFloat)

	override def view:Mat4f = Mat4f(inverse(Mat4x3 rotate(directionQuat) translate(position)))
  override def noTranslate:Mat4f = Mat4f(inverse(Mat4x3 rotate(directionQuat)))
}
