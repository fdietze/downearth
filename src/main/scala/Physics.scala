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
	implicit def v2v(in:Vec3) = new Vector3f(in.x,in.y,in.z) 
	
	val broadPhase = new DbvtBroadphase
	val collisionConfig = new DefaultCollisionConfiguration()
	val dispatcher = new  CollisionDispatcher(collisionConfig)
	
	// dispatcher.setNearCallback( new MyNearCallback )
	

	val sol = new SequentialImpulseConstraintSolver
	val dynamicsWorld = new DiscreteDynamicsWorld(dispatcher,broadPhase,sol,collisionConfig)
	
	val tickCallback = new InternalTickCallback{
		override def internalTick( world:DynamicsWorld, timeStep:Float){
			prepareGroundMesh2
		}
	}
	
	dynamicsWorld.setInternalTickCallback(tickCallback, null)
	dynamicsWorld.setGravity(new Vector3f(0,0,-1))
	dynamicsWorld.setDebugDrawer(DirectDrawer)
	
	var pause = false
	
	def togglePause{
		if(pause){
			pause = false
			simtime = getTime
		}
		else
			pause = true
	}
	
	
	
	// TODO disable deactivation !!!!! funktioniert nicht !!!!
	// BulletGlobals.setDeactivationDisabled(true);
	
	val startTransform = new Transform
	startTransform.setIdentity
	startTransform.origin.set(0f, 0f, 0f)
	
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
	
	case class Ball(body:RigidBody,radius:Float,stream : StreamingBox)
	
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
		
		balls ::= Ball(body,radius,new StreamingHexaederBox(Vec3i(round(pos)),ceil(radius).toInt*2))
		
		body.setCcdMotionThreshold(radius*0.1f)
		body.setCcdSweptSphereRadius(radius*0.9f) 
		
		body
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
	
	trait StreamingBox{
		def moveTo(dstpos:Vec3)
	}
	
	class StreamingHexaederBox(val pos:Vec3i,val size:Int) extends StreamingBox {
		assert(size%2 == 0, " noch nicht implementiert ")
		// argument is center position, this is the lower left corner of the Area
		pos -= (size/2)
		
		val bodies = new Array3D[Option[RigidBody]](Vec3i(size))
		
		def fillfunc(v:Vec3i) = {
			
			val vertexdata = World(pos+v).vertices.distinct
			
			if(!vertexdata.isEmpty){
				val center = (vertexdata.reduce(_+_))/vertexdata.size
			
				val points = new ObjectArrayList[Vector3f]
				vertexdata foreach ( w => points.add(w-center) )
				val shape = new ConvexHullShape(points)
			
				val body = addShape(pos+v+center, shape)
			
				Some(body)
			}
			else
				None
		}
		
		bodies fill fillfunc
		
		
		def move(dirvec:Vec3i){
			assert( length(dirvec) == 1 )
			
			// remove old bodies
			val (in,out) = (Vec3i(0) until Vec3i(size)) partition ( x => Util.indexInRange(x-dirvec,Vec3i(0),size) )
			
			for( v <- out ){
				val body = bodies(v)
				if(body != None)
					dynamicsWorld.removeRigidBody(body.get)
			}
				
			// move the rest of the bodies inside of the Array
			
			val (dir,axis) = dirvec match{
				case Vec3i( 1,0,0) =>  ( 1,0)
				case Vec3i(-1,0,0) =>  (-1,0)
				case Vec3i(0, 1,0) =>  ( 1,1)
				case Vec3i(0,-1,0) =>  (-1,1)
				case Vec3i(0,0, 1) =>  ( 1,2)
				case Vec3i(0,0,-1) =>  (-1,2)
			}
			
			for(v <- in.toSeq.sortBy( v => dir * v(axis) ) ){
				bodies(v-dirvec) = bodies(v)
			}
			
			// load the new bodies
			pos += dirvec
			
			
			val newarea = (Vec3i(0) until Vec3i(size)) filterNot ( x => Util.indexInRange(x+dirvec,Vec3i(0),size) )
			
			for( v <- newarea ){
				bodies(v) = fillfunc(v)
			}
		}
		
		def moveTo(dstpos:Vec3){
			val movingdistance = Vec3i(round(dstpos))-(pos + size/2)
			for(i <- 0 until 3) {
				for( _ <- 0 until movingdistance(i).abs ){
					val dir = Vec3i(0)
					dir(i) = sign( movingdistance(i) )
					move( dir )
				}
			}
		}
		
		def draw{
			glPushMatrix
			glTranslatef(pos.x,pos.y,pos.z)
			Draw.renderCube(size)
			glPopMatrix
		}
		
	}
	
	class StreamingTriangleBox(val pos:Vec3i,val size:Int) extends StreamingBox{
		assert(size%2 == 0, " noch nicht implementiert ")
		// argument is center position, this is the lower left corner of the Area
		pos -= (size/2)
		
		val bodies = new Array3D[Seq[RigidBody]](Vec3i(size))
		
		
		def fillfunc(v:Vec3i) = {
			val builder = collection.mutable.ArrayBuilder.make[RigidBody]
			
			val polygondata = World.octree.getPolygons(pos+v)
			
			
			val polygonIterator =
			for( Seq(pt0,pt1,pt2) <- polygondata.grouped(3) ) yield {
					val center = (pt0+pt1+pt2)/3
					addShape(center, new BU_Simplex1to4(pt0-center, pt1-center, pt2-center))
			}
			val polygons = polygonIterator.toSeq
			
			if(polygondata.size != 0){
				println("vertices: "+polygondata.size+" at "+(pos+v)+" polygons: " + polygons.size)
			}
			polygons
		}
		
		bodies fill fillfunc
		
		
		def move(dirvec:Vec3i){
			assert( length(dirvec) == 1 )
			
			// remove old bodies
			val (in,out) = (Vec3i(0) until Vec3i(size)) partition ( x => Util.indexInRange(x-dirvec,Vec3i(0),size) )
			
			for( v <- out )
				bodies(v) foreach dynamicsWorld.removeRigidBody 
				
			// move the rest of the bodies inside of the Array
			
			val (dir,axis) = dirvec match{
				case Vec3i( 1,0,0) =>  ( 1,0)
				case Vec3i(-1,0,0) =>  (-1,0)
				case Vec3i(0, 1,0) =>  ( 1,1)
				case Vec3i(0,-1,0) =>  (-1,1)
				case Vec3i(0,0, 1) =>  ( 1,2)
				case Vec3i(0,0,-1) =>  (-1,2)
			}
			
			for(v <- in.toSeq.sortBy( v => dir * v(axis) ) ){
				bodies(v-dirvec) = bodies(v)
			}
			
			// load the new bodies
			pos += dirvec
			
			
			val newarea = (Vec3i(0) until Vec3i(size)) filterNot ( x => Util.indexInRange(x+dirvec,Vec3i(0),size) )
			
			for( v <- newarea ){
				bodies(v) = fillfunc(v)
			}
		}
		
		def moveTo(dstpos:Vec3){
			val movingdistance = Vec3i(round(dstpos))-(pos + size/2)
			for(i <- 0 until 3) {
				for( _ <- 0 until movingdistance(i).abs ){
					val dir = Vec3i(0)
					dir(i) = sign( movingdistance(i) )
					move( dir )
				}
			}
		}
		
		def draw{
			glPushMatrix
			glTranslatef(pos.x,pos.y,pos.z)
			Draw.renderCube(size)
			glPopMatrix
		}
		
	}
	
	def prepareGroundMesh2{
		for( Ball(body, _ , stream) <- balls ){
			val tmp = new Vector3f
			body getCenterOfMassPosition tmp
			val pos = Vec3(tmp.x,tmp.y,tmp.z)
			stream moveTo pos
		}
	}
	
	def prepareGroundMesh{
		//if(groundBody != None){
		//	dynamicsWorld removeRigidBody groundBody.get
		//	groundBody = None
		//}
		
		groundBodies foreach dynamicsWorld.removeRigidBody
		
		//TODO Überlappende polygone
		val triangleVertices =
		for( Ball(body,radius, _ ) <- balls ) yield {
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
