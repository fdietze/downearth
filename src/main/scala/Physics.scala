package xöpäx

import com.bulletphysics.{BulletGlobals,collision,dynamics,util}
import collision._
import dynamics._
import util.ObjectArrayList
import broadphase.DbvtBroadphase
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
import com.bulletphysics.collision.dispatch.DefaultNearCallback
import com.bulletphysics.collision.broadphase.BroadphasePair;
import com.bulletphysics.collision.broadphase.DispatcherInfo;

import simplex3d.data._
import simplex3d.data.float._

object BulletPhysics{
	val broadPhase = new DbvtBroadphase
	val collisionConfig = new DefaultCollisionConfiguration()
	val dispatcher = new  CollisionDispatcher(collisionConfig)
	
	dispatcher.setNearCallback( new DefaultNearCallback{
		override def handleCollision(collisionPair:BroadphasePair, dispatcher:CollisionDispatcher, dispatchInfo:DispatcherInfo) = {
			println("yay collision callback")
			super.handleCollision(collisionPair,dispatcher,dispatchInfo)
		}
	})
	

	val sol = new SequentialImpulseConstraintSolver
	val dynamicsWorld = new DiscreteDynamicsWorld(dispatcher,broadPhase,sol,collisionConfig)
	
	val tickCallback = new InternalTickCallback{
		override def internalTick( world:DynamicsWorld, timeStep:Float){
			prepareGroundMesh
		}
	}
	
	dynamicsWorld.setInternalTickCallback(tickCallback, null)
	
	var pause = false
	
	def togglePause{
		if(pause){
			pause = false
			simtime = getTime
		}
		else
			pause = true
	}
	
	dynamicsWorld.setGravity(new Vector3f(0,0,-1))
	dynamicsWorld.setDebugDrawer(DirectDrawer)
	
	// TODO disable deactivation !!!!! funktioniert nicht !!!!
	BulletGlobals.setDeactivationDisabled(true);
	
	val startTransform = new Transform
	startTransform.setIdentity
	startTransform.origin.set(0f, 0f, 0f)
		
	
	addWall
	
	def makeStaticMesh(triangleverts:Seq[ConstVec3]) = {
		
		val vertices = DataBuffer[Vec3, RFloat](triangleverts.size)
		val indices = DataBuffer[SInt,SInt](triangleverts.size)
		
		val indexVertexArray = new TriangleIndexVertexArray(
			triangleverts.size/3,   //numTriangles
			indices.bindingBuffer,  //triangleIndexBase
			indices.stride,         //triangleIndexStride
			triangleverts.size,     //numVertices
			vertices.bindingBuffer, //vertexBase
			vertices.stride         //vertexStride
		)
		
		val groundShape = new BvhTriangleMeshShape(indexVertexArray, false)
		
		
		val myMotionState = new DefaultMotionState(startTransform);
		val cInfo = new RigidBodyConstructionInfo(0, myMotionState, groundShape, new Vector3f(0,0,0) );
		val staticBody = new RigidBody(cInfo);
		staticBody.setCollisionFlags( staticBody.getCollisionFlags | CollisionFlags.STATIC_OBJECT )
		staticBody
	}
	
	case class Ball(body:RigidBody,radius:Float)
	
	var balls:List[Ball] = Nil
	//var groundBody:Option[RigidBody] = None
	var groundBodies:Seq[RigidBody] = Nil
	
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
		dynamicsWorld.addRigidBody(body,1,-1)
		
		balls ::= Ball(body,radius)
		
		body
	}
	
	def addWall {
		val mass = 0;
		val colShape = new BoxShape(new Vector3f(20,20,20))
		val startTransform = new Transform();
		startTransform.setIdentity
		startTransform.origin.set(-10,-10,-30)

		val localInertia = new Vector3f(0, 0, 1)
		if (mass != 0f)
			colShape.calculateLocalInertia(mass, localInertia)

		val myMotionState = new DefaultMotionState(startTransform)
		val rbInfo = new RigidBodyConstructionInfo(mass, myMotionState, colShape, localInertia)
		val body = new RigidBody(rbInfo)
		dynamicsWorld.addRigidBody(body,1,-1)
	}
	
	def addShape(pos:Vec3, colShape:CollisionShape) = {
		val mass = 0;
		val startTransform = new Transform();
		startTransform.setIdentity
		startTransform.origin.set(pos.x,pos.y,pos.z)

		val localInertia = new Vector3f(0, 0, 1)
		if (mass != 0f)
			colShape.calculateLocalInertia(mass, localInertia)

		val myMotionState = new DefaultMotionState(startTransform)
		val rbInfo = new RigidBodyConstructionInfo(mass, myMotionState, colShape, localInertia)
		val body = new RigidBody(rbInfo)
		dynamicsWorld.addRigidBody(body,1,-1)
		
		body
	}
	
	
	def prepareGroundMesh{
		//if(groundBody != None){
		//	dynamicsWorld removeRigidBody groundBody.get
		//	groundBody = None
		//}
		
		groundBodies foreach dynamicsWorld.removeRigidBody
		
		//TODO Überlappende polygone
		val triangleVertices =
		for( Ball(body,radius) <- balls ) yield {
			val tmp = new Vector3f
			body getCenterOfMassPosition tmp
			val pos = Vec3(tmp.x,tmp.y,tmp.z)
			val lower = Vec3i(floor(pos - radius - 0.1f))
			val upper = Vec3i(ceil(pos + radius + 0.1f))
			
			val vertices = (lower until upper) flatMap World.octree.getPolygons
			
			glDisable(GL_LIGHTING)
			glPushMatrix
			
			glBegin(GL_TRIANGLES)
			for( Vec3(x,y,z) <- vertices ){
				glVertex3f(x,y,z)
			}
			glEnd
			
			
			glTranslatef(lower.x,lower.y,lower.z)
			val Vec3i(sx,sy,sz) = upper - lower
			glScalef(sx,sy,sz)
			Draw.renderCube(1)
			
			
			glPopMatrix
			
			vertices
		}
		
		val vertices = triangleVertices.flatten
		if( ! vertices.isEmpty ){		
			// groundBody = Some( makeStaticMesh(vertices) )
			// dynamicsWorld.addRigidBody(groundBody.get,1,-1)
			
			implicit def v2v(in:Vec3) = new Vector3f(in.x,in.y,in.z) 
			
			val bodyIterator =
			for( Seq(pt0,pt1,pt2) <- vertices.grouped(3) ) yield {
				val center = (pt0+pt1+pt2)/3
				addShape(center, new BU_Simplex1to4(pt0-center, pt1-center, pt2-center))
			}
			
			groundBodies = bodyIterator.toSeq
		}
	}
	
	def getTime = System.nanoTime / 1000000000.0
	var simtime = getTime
	
	def simStep(timestep:Float){
		
		val vector = new Vector3f
//		if(groundBody != None){
//			groundBody.get.getCenterOfMassPosition(vector) 
//			println("ground Transform: "+vector)
//		}
		if(!balls.isEmpty){
			balls.head.body.getCenterOfMassPosition(vector) //getWorldTransform(transform)
			println("ballTransform: "+vector)
		}
		
		println(dynamicsWorld.getNumCollisionObjects)
		dynamicsWorld stepSimulation timestep
	}
	
	def update{
		if(!pause){
			val currentTime = getTime
			while(simtime < currentTime){
				dynamicsWorld stepSimulation (currentTime - simtime).toFloat
				simtime = currentTime
			}
		}
	}
	
	def debugDrawWorld{
		glPushMatrix
		glDisable(GL_LIGHTING)
		dynamicsWorld.debugDrawWorld
		glColor3b(127,127,127)
		glEnable(GL_LIGHTING)
		glPopMatrix
	}
}
