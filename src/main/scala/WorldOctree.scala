package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import Util._
import collection.Map

import Config.minMeshNodeSize

// Kapselung für die OctreeNodes
class WorldOctree(var rootNodeSize:Int,var rootNodePos:Vec3i = Vec3i(0)) extends Data3D[Hexaeder] with Serializable{
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

	def update(p:Vec3i,h:Hexaeder) {
		if(rootNodeInfo.indexInRange(p)) {
			// TODO, kann auf updated zurückgeführt werden
			if(meshGenerated)
				root = rootB.updated(rootNodeInfo, p,h)
			else 
				root = rootA.updated(rootNodeInfo, p,h)
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
			glTranslatef(rootNodePos.x,rootNodePos.y,rootNodePos.z)
			glColor3f(1,0,0)
			Draw.renderCube(rootNodeSize)
			glPopMatrix
		
			glPushMatrix
			glTranslatef(worldWindowPos.x,worldWindowPos.y,worldWindowPos.z)
			glColor3f(0,1,0)
			Draw.renderCube(worldWindowSize)
			glPopMatrix
		}
	}
	
	def genMesh(f:(Vec3i => Hexaeder) = World.apply _){
		assert(! meshGenerated)
		root = rootA.genMesh(rootNodeInfo,minMeshNodeSize,(x => {if(indexInRange(x)) apply(x) else f(x) }) )
		meshGenerated = true
	}
	
	import scala.actors.Future

	def jumpTo(pos:Vec3){
		val newcenter = (Vec3i(pos)/minMeshNodeSize)*minMeshNodeSize
		worldWindowPos = newcenter-worldWindowSize/2
		
		for(vi <- (Vec3i(0) until Vec3i(worldWindowSize/minMeshNodeSize)).toSeq.sortBy(v => length(v-worldWindowSize/minMeshNodeSize/2)) ){
			val nodeinfo = NodeInfo(worldWindowPos + vi*minMeshNodeSize,minMeshNodeSize)
			generateNode(nodeinfo)
		}
	}
	
	def generateNode(info:NodeInfo){
		if(!isSet(info)) {
			WorldNodeGenerator.Master ! info
		}
	}
	
	def makeUpdates = {
		while( ! WorldNodeGenerator.Master.done.isEmpty ){
			val ( nodeinfo, node) = WorldNodeGenerator.Master.done.dequeue
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
		val wsize = worldWindowSize.toFloat
		val msize = minMeshNodeSize.toFloat

		//while(Util.indexInRange(worldWindowPos,worldWindowSize,Vec3i(pos))
		
		assert((worldWindowSize / minMeshNodeSize) % 2 == 0)
		
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

	override def fill( foo: Vec3i => Hexaeder ){
		for( v <- rootNodePos until rootNodePos + rootNodeSize ) {
			this(v) = foo(v)
		}
	}
	
	def isSet(info:NodeInfo):Boolean = {
		for( job <- WorldNodeGenerator.Master.activeJobs ){
			if(job indexInRange info)
				return true
		}

		try{
			for( job <- WorldNodeGenerator.Master.done ){
				if(job._1 indexInRange info)
					return true
			}
		}
		catch{
			case x => println(x)
				return true
		}
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
