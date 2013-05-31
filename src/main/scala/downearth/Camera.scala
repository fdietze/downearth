package downearth

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import simplex3d.data._
import simplex3d.data.double._

import Config._
import downearth.worldoctree.NodeInfo
import org.lwjgl.opengl.Display

abstract class Camera {
	var position:ReadVec3
  var directionQuat:ReadQuat4

  def direction:Vec3 = directionQuat.rotateVector( -Vec3.UnitZ )

  def projection:Mat4
  def view:Mat4

  protected val m_viewBuffer = DataBuffer[Mat4,RFloat](1)
  protected val m_projectionBuffer = DataBuffer[Mat4,RFloat](1)

  def viewBuffer = {
    m_viewBuffer(0) = view
    m_viewBuffer.buffer
  }

  def projectionBuffer = {
    m_projectionBuffer(0) = projection
    m_projectionBuffer.buffer()
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
	

	override def projection:Mat4 = {
    // TODO implement viewport
		val v = Display.getWidth.toDouble / Display.getHeight.toDouble
		
		val n = 0.05     //near
		val f = 1000.0   //far
		val r = v*n      //right
		val t = n        //top
		
		Mat4(n/r,0,0,0, 0,n/t,0,0, 0,0,(f+n)/(n-f),-1, 0,0,2*f*n/(n-f),0)
	}

	override def view:Mat4 = Mat4(inverse(Mat4x3 rotate(directionQuat) translate(position)))
}

trait FrustumTest extends Function1[NodeInfo,Boolean] {
	def testNode( info:NodeInfo ):Boolean
  def apply( info:NodeInfo ) = testNode(info)
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

	def testNode( info:NodeInfo ):Boolean = {
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
		return true
	}
}
