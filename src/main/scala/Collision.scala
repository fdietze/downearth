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
	
	
	/*
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
	*/

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
	val timestep = 1f/Main.FRAMERATE

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
