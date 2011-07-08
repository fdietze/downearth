package xöpäx

import com.bulletphysics.collision._
import broadphase.DbvtBroadphase
import com.bulletphysics.dynamics._
import com.bulletphysics.util.ObjectArrayList
import constraintsolver.SequentialImpulseConstraintSolver
import dispatch.{CollisionDispatcher, DefaultCollisionConfiguration}
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

object BulletPhysics{
	val broadPhase = new DbvtBroadphase
	val collisionConfig = new DefaultCollisionConfiguration()
	val dispatcher = new  CollisionDispatcher(collisionConfig)
	val sol = new SequentialImpulseConstraintSolver
	val dynamicsWorld = new DiscreteDynamicsWorld(dispatcher,broadPhase,sol,collisionConfig)

	dynamicsWorld.setGravity(new Vector3f(0,0,-1))
	dynamicsWorld.setDebugDrawer(DirectDrawer)

	def addSector(worldpos:Vec3i, sector:WorldOctree){

		println("Adding sector to Physics")

		val transform = new Transform
		transform.setIdentity
		val vertices = new ObjectArrayList[Vector3f]
		for(i <- 0 until 6)
			vertices add new Vector3f(0,0,0)

		for( WorldNodeInfo(pos,size,hexa) <- sector if(hexa ne EmptyHexaeder)) {
			for(i <- 0 until 6){
				val w = hexa(i) * size
				vertices.get(i).set(w.x,w.y,w.z)
			}

			val shape = new ConvexHullShape(vertices)
			transform.origin.set(pos.x,pos.y,pos.z)
			val myMotionState = new DefaultMotionState(transform)
			val rbInfo = new RigidBodyConstructionInfo(0,myMotionState,shape,new Vector3f(0,0,0))
			val body = new RigidBody(rbInfo)

			dynamicsWorld addRigidBody body
		}
	}
	/*
	def addSector(pos:Vec3i, sector:Data3D[Hexaeder]){
		val transform = new Transform

		val vertices = new ObjectArrayList[Vector3f](6)
		transform.setIdentity
		var bodycounter = 0
		for(i <- 0 until 6)
			vertices add new Vector3f(0,0,0)

		for(v <- Vec3i(0) until Vec3i(cubesize)){
			val h = sector(v)
			if( (h ne EmptyHexaeder) /* && (h.vertexCount != 0) */ ){
				bodycounter += 1
				for(i <- 0 until 6){
					val w = h(i)
					vertices.get(i).set(w.x, w.y, w.z)
				}

				val shape = new ConvexHullShape(vertices)
				transform.origin.set(v.x,v.y,v.z)

				val myMotionState = new DefaultMotionState(transform);
				val rbInfo = new RigidBodyConstructionInfo(0, myMotionState, shape, new Vector3f(0,0,0));
				val body = new RigidBody(rbInfo)

				// add the body to the dynamics world
				dynamicsWorld addRigidBody body
			}
		}

		println("added " + bodycounter + " bodies")
	}
	*/

	def addPolygonSector(sector:WorldOctree){
		val shape = new BvhTriangleMeshShape()
		val vertices = sector.mesh.vertices
		val gVertices = vertices.bindingBuffer
		val totalVerts = vertices.size
		val totalTriangles = totalVerts / 3
		val vertStride = vertices.stride
		val indexStride = 3*4
		
		val transform = new Transform
		transform.setIdentity
		val myMotionState = new DefaultMotionState(transform)

		val indices = ByteBuffer.allocateDirect(totalTriangles * 3 * 4).order(ByteOrder.nativeOrder)

		(0 until totalVerts).foreach( indices.putInt _ )
		indices.flip();

		val indexVertexArrays = new TriangleIndexVertexArray(totalTriangles,
				indices,
				indexStride,
				totalVerts, gVertices, vertStride)
		
		val useQuantizedAabbCompression = false
		val trimeshShape = new BvhTriangleMeshShape(indexVertexArrays, useQuantizedAabbCompression)

		val rbInfo = new RigidBodyConstructionInfo(0,myMotionState,trimeshShape,new Vector3f(0,0,0))
		val body = new RigidBody(rbInfo)

		dynamicsWorld addRigidBody body
	}

	def addBall(pos:Vec3,radius:Float) = {
		val mass = 1f;
		val colShape = new SphereShape(radius)
		val startTransform = new Transform();
		startTransform.setIdentity();
		startTransform.origin.set(pos.x,pos.y,pos.z)

		val localInertia = new Vector3f(0, 0, 1)
		if (mass != 0f)
			colShape.calculateLocalInertia(mass, localInertia)

		val myMotionState = new DefaultMotionState(startTransform)
		val rbInfo = new RigidBodyConstructionInfo(mass, myMotionState, colShape, localInertia)
		val body = new RigidBody(rbInfo)
		dynamicsWorld addRigidBody body
		body
	}

	def getTime = System.nanoTime / 1000000000.0
	var simtime = getTime
	val timestep = 1f/Worldgen.FRAMERATE

	def update{
		val currentTime = getTime
		while(simtime < currentTime){
			dynamicsWorld stepSimulation timestep
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


