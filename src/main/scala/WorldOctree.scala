package openworld

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import Util._
import akka.pattern.ask

import Config.minMeshNodeSize
import scala.util.{Success, Failure}
import akka.util.Timeout
import concurrent.Await
import concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

// Kapselung für die OctreeNodes
class WorldOctree(var rootNodeSize:Int,var rootNodePos:Vec3i = Vec3i(0)) extends Data3D[Leaf] with Serializable{
	var worldWindowPos:Vec3i = rootNodePos.clone
	val worldWindowSize:Int = rootNodeSize
	
	def worldWindowCenter = worldWindowPos + worldWindowSize/2
	
	val vsize = Vec3i(worldWindowSize)
	
	// TODO dies ist noch etwas unsauber gelöst.
	var root:Octant = new Leaf(EmptyHexaeder) // DeadInnerNode
	def rootA = root.asInstanceOf[OctantUnderMesh]
	def rootB = root.asInstanceOf[OctantOverMesh]
	
	var meshGenerated = false
	
	override def indexInRange(pos:Vec3i) = Util.indexInRange(pos,rootNodePos,rootNodeSize)
	
	def rootNodeInfo = NodeInfo(rootNodePos,rootNodeSize)
	
	def apply(p:Vec3i) = {
		if(rootNodeInfo.indexInRange(p))
			root(rootNodeInfo,p)
		else
			Config.ungeneratedDefault
	}

	def update(p:Vec3i,l:Leaf) {
		if(rootNodeInfo.indexInRange(p)) {
			// TODO, kann auf updated zurückgeführt werden
			if(meshGenerated)
				root = rootB.updated(rootNodeInfo, p,l)
			else 
				root = rootA.updated(rootNodeInfo, p,l)
		}
		else{
			printf("update out of world at %s, %s\n",p.toString,rootNodeInfo.toString)
		}
	}
	
	override def toString = "Octree("+root.toString+")"
	
	def draw(test:FrustumTest) {
		makeUpdates
	
		import org.lwjgl.opengl.GL11._
		glColor3f(1,1,1)
		rootB.draw(rootNodeInfo,test)
		
		if(Config.debugDraw) {
			glDisable(GL_LIGHTING)
			glDisable(GL_TEXTURE_2D)
		
			glPushMatrix
			glTranslate3dv(Vec3(rootNodePos))
			glColor3f(1,0,0)
			Draw.renderCube(rootNodeSize)
			glPopMatrix
		
			glPushMatrix
			glTranslate3dv(worldWindowPos + 0.05)
			glColor3f(0,1,0)
			Draw.renderCube(worldWindowSize - 0.1)
			glPopMatrix
		}
	}
	
	def genMesh(f:(Vec3i => Polyeder) = v => World.apply(v).h ) {
		assert(! meshGenerated)
		root = rootA.genMesh(rootNodeInfo,minMeshNodeSize,(x => {if(indexInRange(x)) apply(x).h else f(x) }) )
		meshGenerated = true
	}

	def generateStartArea {
		root = DeadInnerNode // TODO GeneratingNode
		WorldNodeGenerator.master ! rootNodeInfo.cuboid
		meshGenerated = true
	}
	
	def generateNode(info:NodeInfo) {
		if(!isSet(info)) {
			WorldNodeGenerator.master ! info.cuboid
		}
	}
	
	def makeUpdates = {

    implicit val timeout = Timeout(1000 seconds)
    val future = (WorldNodeGenerator.master ? GetFinishedJobs).mapTo[Seq[(NodeInfo, OctantOverMesh)]]
    val s = Await.result(future, 1000 seconds)

    for( (nodeinfo, node) <- s ) {
      insert( nodeinfo, node )
      BulletPhysics.worldChange(nodeinfo)
    }
	}

	def move(dir:Vec3i){
		// checkrange
		worldWindowPos += dir * minMeshNodeSize

		val slicepos = worldWindowPos + (dir+1)/2*worldWindowSize - (dir+1)/2 * minMeshNodeSize
		val slicesize = (Vec3i(1) - abs(dir)) * (worldWindowSize / minMeshNodeSize) + abs(dir)
		
		for(vi <- Vec3i(0) until slicesize){
			val nodeinfo = NodeInfo(slicepos + vi*minMeshNodeSize,minMeshNodeSize)
			generateNode(nodeinfo)
		}
	}
	
	def stream(pos:Vec3){
		val wpos = Vec3(worldWindowPos)
		val wsize = worldWindowSize.toDouble
		val msize = minMeshNodeSize.toDouble

		//while(Util.indexInRange(worldWindowPos,worldWindowSize,Vec3i(pos))
		
		val lowerVertex = wpos + wsize/2 - msize/2
		val upperVertex = wpos + wsize/2 + msize/2
		
		if( pos.x < lowerVertex.x )
			move( Vec3i(-1,0,0) )
		if( pos.y < lowerVertex.y )
			move( Vec3i(0,-1,0) )
		if( pos.z < lowerVertex.z )
			move( Vec3i(0,0,-1) )
		if( pos.x > upperVertex.x )
			move( Vec3i(1,0,0) )
		if( pos.y > upperVertex.y )
			move( Vec3i(0,1,0) )
		if( pos.z > upperVertex.z )
			move( Vec3i(0,0,1) )
	}
	
	def insert( nodeinfo:NodeInfo, that:OctantOverMesh ) {
		val NodeInfo(nodepos,nodesize) = nodeinfo
		
		if(any(lessThan(nodepos, rootNodePos))) {
			// Welt wird in (+1,+1,+1) vergrößert
			val newdata = Array.fill[OctantOverMesh](8)(DeadInnerNode)
			newdata(7) = rootB
			root = new InnerNodeOverMesh(newdata)
			
			rootNodePos -= rootNodeSize
			rootNodeSize *= 2
		}

		else if(any(greaterThan(nodepos+nodesize,rootNodePos+rootNodeSize))) {
			// Welt wird in (-1,-1,-1) vergrößert
			val newdata = Array.fill[OctantOverMesh](8)(DeadInnerNode)
			newdata(0) = rootB
			root = new InnerNodeOverMesh(newdata)
			
			rootNodeSize *= 2
		}
		
		root = rootB.insertNode(rootNodeInfo, nodeinfo, that)
	}

	override def fill( foo: Vec3i => Leaf ) {
		for( v <- rootNodePos until rootNodePos + rootNodeSize ) {
			this(v) = foo(v)
		}
	}
	

	def isSet(info:NodeInfo):Boolean = {
		//TODO: generating Node einführen und linearzeitabfrage rauswerfen
    import akka.pattern.ask
    implicit val timeout = akka.util.Timeout(1000)

    val future = WorldNodeGenerator.master ? info

    // asks weather the Area is currently processed
    val answer = Await.result( future, Duration(1, TimeUnit.SECONDS) ).asInstanceOf[Boolean]

    if ( answer )
      return true

    if(rootNodeInfo indexInRange info)
			return rootB.isSet(rootNodeInfo,info)
		else
			return false
	}
	
	def getPolygons(pos:Vec3i) = {
		if(rootNodeInfo indexInRange pos)
			root.asInstanceOf[OctantOverMesh].getPolygons(rootNodeInfo,pos)
		else
			Nil
	}
}
