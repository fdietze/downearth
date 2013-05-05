package downearth.worldoctree

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import akka.pattern.ask
import akka.util.Timeout

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._

import downearth.util._
import downearth.generation.{GetFinishedJobs, WorldNodeGenerator}
import downearth.{BulletPhysics, Config, util}
import downearth.Config._


object WorldOctree {
  def frontToBackOrder(dir:Vec3):Array[Int] = {
    val  x = if( dir.x < 0 ) 1 else 0
    val  y = if( dir.y < 0 ) 2 else 0
    val  z = if( dir.z < 0 ) 4 else 0

    val nx = if( dir.x < 0 ) 0 else 1
    val ny = if( dir.y < 0 ) 0 else 2
    val nz = if( dir.z < 0 ) 0 else 4

    val v1 =  x |  y |  z
    val v2 = nx |  y |  z
    val v3 =  x | ny |  z
    val v4 = nx | ny |  z
    val v5 =  x |  y | nz
    val v6 = nx |  y | nz
    val v7 =  x | ny | nz
    val v8 = nx | ny | nz

    Array(v1,v2,v3,v4,v5,v6,v7,v8)
  }
}

// Kapselung für die OctreeNodes
class WorldOctree(var rootNodeSize:Int,var rootNodePos:Vec3i = Vec3i(0)) extends Data3D[Leaf] with Serializable{
	var worldWindowPos:Vec3i = rootNodePos.clone
	val worldWindowSize:Int = rootNodeSize
	
	def worldWindowCenter = worldWindowPos + worldWindowSize/2
	
	val vsize = Vec3i(worldWindowSize)
	

	var root:OctantOverMesh = GeneratingNode // UngeneratedInnerNode
  WorldNodeGenerator.master ! rootNodeInfo.toCuboid
	
	override def indexInRange(pos:Vec3i) = util.indexInRange(pos,rootNodePos,rootNodeSize)
	
	def rootNodeInfo = NodeInfo(rootNodePos,rootNodeSize)

  def queryRegion(test:(NodeInfo) => Boolean)(order:Array[Int])(function: (NodeInfo,Octant) => Boolean) {
    require(order.length == 8)

    val infoQueue = collection.mutable.Queue[NodeInfo](rootNodeInfo)
    val nodeQueue = collection.mutable.Queue[Octant](root)

    while( !nodeQueue.isEmpty ) {
      val currentInfo = infoQueue.dequeue()
      val currentNode = nodeQueue.dequeue()
      if( test(currentInfo) && function(currentInfo,currentNode) && currentNode.hasChildren ) {
        var i = 0
        while(i < 8) {
          nodeQueue += currentNode.getChild(order(i))
          infoQueue += currentInfo(order(i))
          i += 1
        }
      }
    }
  }
	
	def apply(p:Vec3i) = {
		if( rootNodeInfo.indexInRange(p) )
			root(rootNodeInfo,p)
		else
			Config.ungeneratedDefault
	}

	def update(p:Vec3i,l:Leaf) {
		if(rootNodeInfo.indexInRange(p)) {
      root = root.updated(rootNodeInfo, p, l)
		}
		else{
			printf("update out of world at %s, %s\n",p.toString,rootNodeInfo.toString)
		}
	}

	override def toString = "Octree("+root.toString+")"
	
	def generateNode(info:NodeInfo) {
    require(!isSet(info))
    insert( info, GeneratingNode )
    WorldNodeGenerator.master ! info.toCuboid
	}
	
	def makeUpdates() {
    implicit val timeout = Timeout(1000 seconds)
    val future = (WorldNodeGenerator.master ? GetFinishedJobs).mapTo[Seq[(NodeInfo, OctantOverMesh)]]
    val s = Await.result(future, 1000 seconds)

    for( (nodeinfo, node) <- s ) {
      insert( nodeinfo, node )
      BulletPhysics.worldChange(nodeinfo)
    }
	}

	def move(dir:Vec3i) {
		// checkrange
		worldWindowPos += dir * minMeshNodeSize

		val slicepos = worldWindowPos + (dir+1)/2*worldWindowSize - (dir+1)/2 * minMeshNodeSize
		val slicesize = (Vec3i(1) - abs(dir)) * (worldWindowSize / minMeshNodeSize) + abs(dir)
		
		for(vi <- Vec3i(0) until slicesize) {
			val nodeinfo = NodeInfo(slicepos + vi*minMeshNodeSize,minMeshNodeSize)
			generateNode(nodeinfo)
		}
	}
	
	def stream(pos:Vec3) {
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
		
//		if(any(lessThan(nodepos, rootNodePos)) || any(greaterThan(nodepos+nodesize,rootNodePos+rootNodeSize))) {
//			// Welt wird in (+1,+1,+1) vergrößert
//			val newdata = Array.fill[OctantOverMesh](8)(UngeneratedInnerNode)
//			newdata(7) = root
//			root = new InnerNodeOverMesh(newdata)
//
//			rootNodePos -= rootNodeSize
//			rootNodeSize *= 2
//		}
//
//		else if() {
//			// Welt wird in (-1,-1,-1) vergrößert
//			val newdata = Array.fill[OctantOverMesh](8)(UngeneratedInnerNode)
//			newdata(0) = root
//			root = new InnerNodeOverMesh(newdata)
//
//			rootNodeSize *= 2
//		}
    if( any(lessThan(nodepos, rootNodePos)) || any(greaterThan(nodepos+nodesize,rootNodePos+rootNodeSize)) )
      incDepth()
		
		root = root.insertNode(rootNodeInfo, nodeinfo, that)
	}

  def incDepth() {
    // TODO add test for correct subdivision of the world. Depending on where the root is, it should be extended differently.

    var newRoot:OctantOverMesh = new InnerNodeOverMesh(Array.fill[OctantOverMesh](8)(UngeneratedInnerNode))
    val newRootNodeInfo = NodeInfo(rootNodePos - rootNodeSize/2, rootNodeSize*2)

    for(i <- 0 until 8) {
      println(root.getChild(i).getClass.getName)
    }

    if( root.isInstanceOf[MeshNode] )
      root = root.asInstanceOf[MeshNode].split

    for(i <- 0 until 8) {
      newRoot = newRoot.insertNode(newRootNodeInfo, rootNodeInfo(i), root.getChild(i).asInstanceOf[OctantOverMesh] )
    }

    root = newRoot
    rootNodePos = newRootNodeInfo.pos
    rootNodeSize = newRootNodeInfo.size
//
//    val data0 = Array.fill[OctantOverMesh](8)(UngeneratedInnerNode)
//    data0(7) = root
//    val data1 = Array.fill[OctantOverMesh](8)(UngeneratedInnerNode)
//    data1(0) = new InnerNodeOverMesh(data0)
//    root = new InnerNodeOverMesh(data1)
//
//    rootNodePos -= rootNodeSize
//    rootNodeSize *= 4
  }

	override def fill( foo: Vec3i => Leaf ) {
		for( v <- rootNodePos until rootNodePos + rootNodeSize ) {
			this(v) = foo(v)
		}
	}

	def isSet(info:NodeInfo):Boolean = {
		if(rootNodeInfo indexInRange info)
			return root.isSet(rootNodeInfo,info)
		else
			return false
	}
	
	def getPolygons(pos:Vec3i) = {
		if(rootNodeInfo indexInRange pos)
			root.getPolygons(rootNodeInfo,pos)
		else
			Nil
	}
}
