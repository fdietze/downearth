package xöpäx

import com.bulletphysics.collision._
import broadphase.DbvtBroadphase
import com.bulletphysics.dynamics._
import com.bulletphysics.util.ObjectArrayList
import constraintsolver.SequentialImpulseConstraintSolver
import dispatch.{CollisionFlags, CollisionDispatcher, DefaultCollisionConfiguration}
import javax.vecmath.Vector3f
import shapes._
import simplex3d.math.float.functions._
import simplex3d.math.float._
import simplex3d.math._
import java.nio.{ByteOrder,ByteBuffer}

import Util._
import WorldGenerator.cubesize
import com.bulletphysics.linearmath.{DefaultMotionState, Transform}
import org.lwjgl.opengl.GL11._
import com.bulletphysics.util.ObjectArrayList._

import simplex3d.data._
import simplex3d.data.float._

object BulletPhysics{
	val broadPhase = new DbvtBroadphase
	val collisionConfig = new DefaultCollisionConfiguration()
	val dispatcher = new  CollisionDispatcher(collisionConfig)
	val sol = new SequentialImpulseConstraintSolver
	val dynamicsWorld = new DiscreteDynamicsWorld(dispatcher,broadPhase,sol,collisionConfig)

	dynamicsWorld.setGravity(new Vector3f(0,0,-1))
	dynamicsWorld.setDebugDrawer(DirectDrawer)
	
	val startTransform = new Transform
	startTransform.setIdentity
	startTransform.origin.set(0f, 0f, 0f)
		
	
	def makeStaticMesh(triangleverts:Seq[ConstVec3]) = {
	
		val vertices = DataBuffer[Vec3, RFloat](triangleverts.size)
		val indices = DataBuffer[SInt,SInt](triangleverts.size)
		
		for(i <- 0 until triangleverts.size){
			vertices(i) = triangleverts(i)
			indices(i) = i
		}
		
		val indexVertexArray = new TriangleIndexVertexArray(
			triangleverts.size/3, //numTriangles
			indices.bindingBuffer, //triangleIndexBase
			indices.stride, //triangleIndexStride
			triangleverts.size, //numVertices
			vertices.bindingBuffer, //vertexBase
			vertices.stride //vertexStride
		)
		
		val groundShape = new BvhTriangleMeshShape(indexVertexArray, false);
		
		val myMotionState = new DefaultMotionState(startTransform);
		
		val cInfo = new RigidBodyConstructionInfo(0, myMotionState, groundShape, new Vector3f(0,0,0) );
		
		val staticBody = new RigidBody(cInfo);
		
		staticBody.setCollisionFlags( staticBody.getCollisionFlags | CollisionFlags.STATIC_OBJECT )
		
		staticBody
	}
	
	case class Ball(body:RigidBody,radius:Float)
	
	var balls:List[Ball] = Nil
	var groundBody:RigidBody = null
	
	def addBall(pos:Vec3,radius:Float) = {
		val mass = 1f;
		val colShape = new SphereShape(radius)
		val startTransform = new Transform();
		startTransform.setIdentity
		startTransform.origin.set(pos.x,pos.y,pos.z)

		val localInertia = new Vector3f(0, 0, 1)
		if (mass != 0f)
			colShape.calculateLocalInertia(mass, localInertia)

		val myMotionState = new DefaultMotionState(startTransform)
		val rbInfo = new RigidBodyConstructionInfo(mass, myMotionState, colShape, localInertia)
		val body = new RigidBody(rbInfo)
		dynamicsWorld addRigidBody body
		
		balls ::= Ball(body,radius)
		
		body
	}
	
	def prepareGroundMesh{
		//TODO Überlappende polygone
		val triangleVertices =
		for( Ball(body,radius) <- balls ) yield {
			val tmp = new Vector3f
			body getCenterOfMassPosition tmp
			val pos = Vec3(tmp.x,tmp.y,tmp.z)
			val lower = Vec3i(floor(pos - radius))
			val upper = Vec3i(ceil(pos + radius))
			
			(lower until upper) flatMap World.octree.getPolygons
		}
		groundBody = makeStaticMesh(triangleVertices.flatten)
		dynamicsWorld addRigidBody groundBody
	}
	
	def removeGroundMesh{
		dynamicsWorld removeRigidBody groundBody
	}

	def getTime = System.nanoTime / 1000000000.0
	var simtime = getTime
	val timestep = 1f/Main.FRAMERATE

	def update{
		val currentTime = getTime
		while(simtime < currentTime){
			//prepareGroundMesh
			dynamicsWorld stepSimulation timestep
			//removeGroundMesh
			simtime += timestep
		}
	}

	def debugDrawWorld{
		glDisable(GL_LIGHTING)
		dynamicsWorld.debugDrawWorld
		glColor3b(127,127,127)
		glEnable(GL_LIGHTING)
	}
}



/*
package xöpäx

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import org.lwjgl.opengl.GL11._

import Util._

object Physics{
	val starttime = System.currentTimeMillis / 1000.0
	var simulationTime = starttime
	
	val timestep = 1 / 60.0
	val gravity = Vec3(0,0,-0.2f)
	
	def update{
		val currentTime = System.currentTimeMillis / 1000.0
		// solange die simulation hinter der aktuellen Zeit ist, 
		// einen Schritt simulieren
		while( simulationTime < currentTime ){
			for(o <- objects)
				o.simStep
			simulationTime += timestep
		}
	}
	
	// TODO object manager
	var objects:List[Dynamic] = Nil
	
	def addBall(pos:Vec3, radius:Float) = {
		val ball = new Ball(pos,radius)
		objects ::= ball
		ball
	}
}

import Physics._

trait Dynamic{
	def simStep
	def multMatrix
}

class Ball(var pos:Vec3,var radius:Float) extends Dynamic{
	import Collision.sphereworldintersect
	
	val mass = radius*Pi*(4f/3f)
	var velocity = Vec3(0)
	var force = Vec3(0)
	
	def simStep{
		velocity = (velocity + timestep.toFloat * (gravity + force / mass ) ) * 0.99f
		val npos = pos + velocity
		sphereworldintersect(npos,radius) match {
		case Some(n) =>
			velocity = reflect(velocity,n)*0.75f
			pos += velocity
		case None =>
			pos.xyz = npos
		}
	}
	
	def multMatrix{
		glTranslatef(pos.x,pos.y,pos.z)
	}
}

object Collision{
	def hexaedersperetest(hexaederpos:Vec3i,hexaeder:Hexaeder,sperepos:Vec3,radius:Float):Option[Vec3] = {
		val vertices = hexaeder.vertices
		// umrechnung in Weltkoordinaten
		for(v <- vertices)
			v += hexaederpos
			
		val normals = hexaeder.normals
		
		//val alldirections = ((vertices map ( v => sperepos - v )) ++ normals) map normalize
		val alldirections = new Array[Vec3](vertices.size + normals.size)
		var i = 0
		for(v <- vertices){
			alldirections(i) = normalize(sperepos-v)
			i += 1
		}
		for(n <- normals){
			alldirections(i) = n
			i += 1
		}
		
		var dir = Vec3.Zero
		var diff = scala.Float.MaxValue
		
		for(direction <- alldirections){
			val projectedvertices = new Array[Float](vertices.size)
			var i = 0
			for(v <- vertices){
				projectedvertices(i) = dot(v,direction)
				i += 1
			}
			val projectedspere = Array(dot(direction,sperepos)+radius,dot(direction,sperepos)-radius)
			
			val smin = projectedspere.min
			val smax = projectedspere.max
			val vmin = projectedvertices.min
			val vmax = projectedvertices.max
			
			if(vmax-smin < diff){
				dir = direction
				diff = vmax-smin
			}
			else if(smax-vmin < diff){
				dir = -direction
				diff = smax-vmin
			}
		}
		
		if( diff > 0 )
			Some( dir*diff )
		else
			None
	}
	
	def intersectioncoords(sperepos:Vec3,radius:Float) = {
		val start = Vec3i(floor(sperepos-radius))
		val end = Vec3i(ceil(sperepos+radius))
		start until end
	}
	
	def sphereworldintersect(sperepos:Vec3,radius:Float):Option[Vec3] = {
		val testresults = 
		for(vi <- intersectioncoords(sperepos,radius)) yield {
			val h = World(vi)
			if(h eq EmptyHexaeder)
				None
			else
				hexaedersperetest(vi,h,sperepos,radius)
		}
		
		// aufaddieren aller kollisionen
		var sum = 0
		val force =
		(Vec3(0) /: testresults) {
			case (v,None) => v
			case (v,Some(s)) => sum += 1;v+s
		}
		if(sum == 0)
			None
		else
			Some(normalize(force))
	}
	
	case class AABB(pos:Vec3,size:Vec3)
	
	def AABBtest(box1:AABB,box2:AABB) = {
		( box1.pos.x < box1.pos.x + box1.size.x && box2.pos.x > box2.pos.x + box2.size.x ) &&
		( box1.pos.y < box1.pos.y + box1.size.y && box2.pos.y > box2.pos.y + box2.size.y ) &&
		( box1.pos.z < box1.pos.z + box1.size.z && box2.pos.z > box2.pos.z + box2.size.z )
	}
}

*/
