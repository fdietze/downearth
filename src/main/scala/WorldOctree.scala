package xöpäx

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import Util._
import collection.Map

import Config.minMeshNodeSize

class WorldOctree(var rootNodeSize:Int,var rootNodePos:Vec3i = Vec3i(0)) extends Data3D[Hexaeder] with Serializable{

	var worldWindowPos:Vec3i = rootNodePos.clone
	val worldWindowSize:Int = rootNodeSize
	
	def worldWindowCenter = worldWindowPos + worldWindowSize/2
	
	val vsize = Vec3i(worldWindowSize)
	var root:Octant = new Leaf(EmptyHexaeder) // DeadInnderNode

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
			if(meshGenerated)
				root = root.patchWorld(rootNodeInfo, p,h,-1,-1)._1
			else 
				root = root.updated(rootNodeInfo, p,h)
		}
		else{
			printf("update out of world at %s, %s\n",p.toString,rootNodeInfo.toString)
		}
	}
	
	override def toString = "Octree("+root.toString+")"
	
	// removes all ungenerated Futures
	def cleanFutures{
		root = root.cleanFutures(rootNodeInfo)
	}
	
	def draw{
		makeUpdates
	
		import org.lwjgl.opengl.GL11._
		glColor3f(1,1,1)
		root.draw
		
		if(Config.debugDraw){
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
	
	def genMesh {
		root = root.genMesh(rootNodeInfo,minMeshNodeSize,(x => {if(indexInRange(x)) apply(x) else World(x)}) )
		meshGenerated = true
	}
	
	import scala.actors.Future

	def jumpTo(pos:Vec3){
		val newcenter = (Vec3i(pos)/minMeshNodeSize)*minMeshNodeSize
		worldWindowPos = newcenter-worldWindowSize/2
		
		for(vi <- Vec3i(0) until Vec3i(worldWindowSize/minMeshNodeSize) ){
			val nodepos = worldWindowPos + vi*minMeshNodeSize
			generateNode(nodepos,minMeshNodeSize)
		}
	}
	
	var generatingNodes:List[(NodeInfo,FutureNode)] = Nil
	
	def generateNode(nodepos:Vec3i,nodesize:Int){
		generatingNodes ::= ( NodeInfo(nodepos, nodesize), WorldNodeGenerator.generateFutureNodeAt(nodepos,nodesize) )
	}
	
	def makeUpdates = {
		val (ready,notReady) = generatingNodes.partition( _._2.node.isSet )
		for( ( NodeInfo(nodepos,nodesize), futureNode) <- ready )
			insert( nodepos,nodesize, futureNode.node.apply )
		generatingNodes = notReady
	}

	def move(dir:Vec3i){
		// checkrange
		worldWindowPos += dir * minMeshNodeSize

		val slicepos = worldWindowPos + (dir+1)/2*worldWindowSize - (dir+1)/2 * minMeshNodeSize
		val slicesize = (Vec3i(1) - abs(dir)) * (worldWindowSize / minMeshNodeSize) + abs(dir)
		
		for(vi <- Vec3i(0) until slicesize){
			val spos = slicepos + vi*minMeshNodeSize
			if(!isSet(spos,minMeshNodeSize)){
				generateNode(spos,minMeshNodeSize)
			}
		}
	}
	
	def stream(pos:Vec3){
		val wpos = Vec3(worldWindowPos)
		val wsize = worldWindowSize.toFloat
		val msize = minMeshNodeSize.toFloat

		//while(Util.indexInRange(worldWindowPos,worldWindowSize,Vec3i(pos))
		
		assert((worldWindowSize / minMeshNodeSize) % 2 == 0  , "da ist noch was nicht implementiert")
		
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
	
	def insert( nodepos:Vec3i, nodesize:Int, that:Octant ) {
		printf("insert node at pos: %s size: %s, rootpos: %s, rootsize: %s\n",nodepos,nodesize,rootNodePos,rootNodeSize)
		if(any(lessThan(nodepos, rootNodePos))) {
			println("World gets bigger-")
			val newroot = new InnerNodeOverVertexArray(EmptyHexaeder)
			for(i <- 0 until 7)
				newroot.data(i) = DeadInnderNode

			newroot.data(7) = root
			root = newroot
			
			rootNodePos -= rootNodeSize
			rootNodeSize *= 2
		}

		else if(any(greaterThan(nodepos+nodesize,rootNodePos+rootNodeSize))) {
			println("World gets bigger+")
			val newroot = new InnerNodeOverVertexArray(EmptyHexaeder)
			for(i <- 1 until 8)
				newroot.data(i) = DeadInnderNode

			newroot.data(0) = root
			root = newroot

			rootNodeSize *= 2
		}
		
		root = root.insertNode(rootNodeInfo,NodeInfo(nodepos,nodesize), that)
	}

	override def fill( foo: Vec3i => Hexaeder ){
		for( v <- rootNodePos until rootNodePos + rootNodeSize ) {
			this(v) = foo(v)
		}
	}
	
	def isSet(nodepos:Vec3i,nodesize:Int):Boolean = {
		val info = NodeInfo(nodepos,nodesize)
		
		var isGenerating = false
		for( (nodeinfo,node) <- generatingNodes ){
			if(nodeinfo indexInRange info)
				return true
		}
		
		if(rootNodeInfo indexInRange info)
			return root.isSet(rootNodeInfo,info)
		else
			return false
	}
	
	def getPolygons(pos:Vec3i) = {
		if(rootNodeInfo indexInRange pos)
			root.getPolygonsOverVertexArray(rootNodeInfo,pos)
		else
			Nil
	}
}
