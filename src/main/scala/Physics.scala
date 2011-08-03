package xöpäx

import com.bulletphysics.{BulletGlobals,collision,dynamics,util}
import collision._
import dynamics._
import util.ObjectArrayList
import broadphase.DbvtBroadphase
import constraintsolver.SequentialImpulseConstraintSolver
import dispatch.{CollisionObject, CollisionFlags, CollisionDispatcher, DefaultCollisionConfiguration}
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

	val sol = new SequentialImpulseConstraintSolver
	val dynamicsWorld = new DiscreteDynamicsWorld(dispatcher,broadPhase,sol,collisionConfig)

	val tickCallback = new InternalTickCallback{
		override def internalTick( world:DynamicsWorld, timeStep:Float){
			prepareGroundMesh2
		}
	}
	
	dynamicsWorld.setInternalTickCallback(tickCallback, null)
	dynamicsWorld.setGravity(new Vector3f(0,0,-9.81f))
	dynamicsWorld.setDebugDrawer(DirectDrawer)
	
	var pause = false
	var debugDraw = false
	
	def togglePause{
		if(pause){
			pause = false
			simtime = getTime
		}
		else
			pause = true
	}
	
	def toggleDebugDraw{
		debugDraw = !debugDraw
	}
	
	case class Body[T](body:RigidBody,radius:Float,stream:StreamingBox[T])
	
	var bodies:List[Body[Option[RigidBody]]] = Nil
	//var groundBody:Option[RigidBody] = None
	var groundBodies:Seq[RigidBody] = Nil
	
	def worldChange(pos:Vec3i){
		for( Body( _ , _ , stream ) <- bodies ){
			stream reloadAt pos
		}
	}
	
	def addShape(mass:Float, pos:Vec3, colShape:CollisionShape):RigidBody = {
		val startTransform = new Transform();
		startTransform.setIdentity
		startTransform.origin.set(pos.x,pos.y,pos.z)

		val localInertia = new Vector3f(0, 0, 1)
		if (mass != 0f){
			colShape.calculateLocalInertia(mass, localInertia)
			startTransform.setRotation( Quat4 rotateZ Pi/2 )
		}

		val myMotionState = new DefaultMotionState(startTransform)
		val rbInfo = new RigidBodyConstructionInfo(mass, myMotionState, colShape, localInertia)
		
		//rbInfo.friction = 0.5f
		rbInfo.linearDamping = 0.9f

		val body = new RigidBody(rbInfo)
		dynamicsWorld.addRigidBody(body,1,-1)
		
		if (mass != 0f){
			val tmp = Array(0f)
			colShape.getBoundingSphere(new Vector3f,tmp)
			val radius = tmp(0)
			bodies ::= Body(body,radius,new StreamingHexaederBox(Vec3i(round(pos)),ceil(radius).toInt))
			body.setActivationState(CollisionObject.DISABLE_DEACTIVATION)
		}
		
		body
	}
	
	abstract class StreamingBox[T:Manifest](centerpos:Vec3i,radius:Int){
		val pos = centerpos - radius
		val size = 2*radius
		
		val bodies = new Array3D[T](Vec3i(size))
		bodies fill fillfunc
		
		def indexInRange(p:Vec3i) = Util.indexInRange(p,pos,size)
		def reloadAt(p:Vec3i){
			if(indexInRange(p)){
				val rpos = p-pos
				println("in Range "+rpos)
				removeBody( bodies(rpos) )
				bodies(rpos) = fillfunc(rpos)
			}
		}
		
		def fillfunc(v:Vec3i):T
		def removeBody(bodies:T)
		
		def move(dirvec:Vec3i){
			assert( length(dirvec) == 1 )
			
			// remove old bodies
			val (in,out) = (Vec3i(0) until Vec3i(size)) partition ( x => Util.indexInRange(x-dirvec,Vec3i(0),size) )
			
			for( v <- out )
				removeBody(bodies(v))
			
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
	
	class StreamingHexaederBox(centerpos:Vec3i,radius:Int) extends StreamingBox[Option[RigidBody]](centerpos,radius) {
		
		def fillfunc(v:Vec3i) = {
			
			val vertexdata = World(pos+v).vertices.distinct
			
			if(!vertexdata.isEmpty){
				val center = (vertexdata.reduce(_+_))/vertexdata.size
			
				val points = new ObjectArrayList[Vector3f]
				vertexdata foreach ( w => points.add(w-center) )
				val shape = new ConvexHullShape(points)
			
				val body = addShape(0,pos+v+center, shape)
			
				Some(body)
			}
			else
				None
		}
		
		def removeBody(bodies:Option[RigidBody]){
			if(bodies != None)
				dynamicsWorld.removeRigidBody(bodies.get)
		}
	}
	
	class StreamingTriangleBox(centerpos:Vec3i,radius:Int) extends StreamingBox[Seq[RigidBody]](centerpos,radius){
		
		def fillfunc(v:Vec3i) = {
			val builder = collection.mutable.ArrayBuilder.make[RigidBody]
			
			val polygondata = World.octree.getPolygons(pos+v)
			
			
			val polygonIterator =
			for( Seq(pt0,pt1,pt2) <- polygondata.grouped(3) ) yield {
					val center = (pt0+pt1+pt2)/3
					addShape(0,center, new BU_Simplex1to4(pt0-center, pt1-center, pt2-center))
			}
			val polygons = polygonIterator.toSeq
			
			polygons
		}
		
		def removeBody(bodies:Seq[RigidBody]){
			bodies foreach dynamicsWorld.removeRigidBody
		}
	}
	
	def prepareGroundMesh2{
		for( Body(body, _ , stream) <- bodies ){
			val tmp = new Vector3f
			body getCenterOfMassPosition tmp
			val pos = Vec3(tmp.x,tmp.y,tmp.z)
			stream moveTo pos
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
		if(debugDraw){
			glPushMatrix
			glDisable(GL_LIGHTING)
			dynamicsWorld.debugDrawWorld
			glColor3b(127,127,127)
			glEnable(GL_LIGHTING)
			glPopMatrix
		}
	}
}
