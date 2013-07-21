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
import downearth.generation.WorldNodeGenerator
import WorldNodeGenerator.Messages.GetFinishedJobs
import downearth.{BulletPhysics, Config, util}
import downearth.Config._
import downearth.message
import collection.mutable
import downearth.world.World

// Kapselung für die OctreeNodes
class WorldOctree(var rootNodeInfo:PowerOfTwoCube, var root:NodeOverMesh = new MeshNode) extends Data3D[Leaf] {
  def rootNodePos = rootNodeInfo.pos
  def rootNodeSize = rootNodeInfo.size

  var worldWindowPos:Vec3i = rootNodePos.clone
  val worldWindowSize:Int = rootNodeSize
  def worldWindowCenter = worldWindowPos + worldWindowSize/2

  // for Data3D interface
  val vsize = Vec3i(worldWindowSize)
  override def indexInRange(pos:Vec3i) = util.indexInRange(pos,rootNodePos,rootNodeSize)

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
      printf(s"update out of area at $p, $rootNodeInfo\n")
    }
  }

  // traverse the octree in a front to back order from view of point camera
  // filter the nodes by predicate (for example frustum test)
  // and apply action to every found node, skip subtree if action returns false
  def queryRegion(predicate:(PowerOfTwoCube) => Boolean = (_) => true, camera:ReadVec3 = null)(action: (PowerOfTwoCube,Node) => Boolean ) {

    val infoQueue = mutable.Queue[PowerOfTwoCube](rootNodeInfo)
    val nodeQueue = mutable.Queue[Node](root)
    val dummyOrder = if(camera == null) Array.range(0,8) else null

    while( nodeQueue.nonEmpty ) {
      val currentInfo = infoQueue.dequeue()
      val currentNode = nodeQueue.dequeue()
      if( predicate(currentInfo) && action(currentInfo, currentNode) && currentNode.hasChildren ) {
        val order = if(camera != null) currentInfo.traversalOrder(camera) else dummyOrder
        var i = 0
        while(i < 8) {
          nodeQueue += currentNode.getChild(order(i))
          infoQueue += currentInfo(order(i))
          i += 1
        }
      }
    }
  }

  // insert Node "that" at position and size of nodeinfo
  def insert( nodeInfo:PowerOfTwoCube, that:NodeOverMesh ) {
    if( !(nodeInfo inside rootNodeInfo) )
      incDepth()

    root = root.insertNode(rootNodeInfo, nodeInfo, that)
  }

  override def toString = "Octree("+root.toString+")"

  def getNextUngenerated:Option[PowerOfTwoCube] = {
    var node:Option[PowerOfTwoCube] = None
    queryRegion(){
      case (area, UngeneratedNode) =>
        node = Some(area)
        false
      case _ =>
        !node.isDefined
    }
    node
  }

  // mark area as ungenerated and ask
  // Worldgenerator to generate it
  def generateArea(area:PowerOfTwoCube) {
    // require(!isSet(info)) // TODO this fails sometimes

    // if area is outside the current Octree,
    // increase the size until it is inside
    while(!(rootNodeInfo indexInRange area)) {
      incDepth()
    }

    val meshNode = new MeshNode(GeneratingNode).genMesh(area,-1,null)
    println(meshNode.mesh.byteSize)
    insert( area, meshNode )
    WorldNodeGenerator.master ! area
  }

  // ask WorldNodeGenerator for generated nodes and insert them into the octree
  def makeUpdates() {
    implicit val timeout = Timeout(1000 seconds)
    val future = (WorldNodeGenerator.master ? GetFinishedJobs).mapTo[Seq[(PowerOfTwoCube, NodeOverMesh)]]
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
			val nodeinfo = PowerOfTwoCube(slicepos + vi*minMeshNodeSize,minMeshNodeSize)
			generateArea(nodeinfo)
		}
	}
	
	def stream(pos:ReadVec3) {
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

  // increase Octree size
  // the root becomes
  def incDepth() {
    // TODO add test for correct subdivision of the area. Depending on where the root is, it should be extended differently.
    assert( rootNodeInfo.center == Vec3i.Zero )

    // +---+---+---+---+            ^
    // |   |   |   |   |            |
    // +---+---+---+---+   ^        |
    // |   |###|###|   |   |        |
    // +---+---+---+---+  root   new root
    // |   |###|###|   |   |        |
    // +---+---+---+---+   v        |
    // |   |   |   |   |            |
    // +---+---+---+---+            v

    // create the new root
    var newRoot:NodeOverMesh = new InnerNodeOverMesh(Array.fill[NodeOverMesh](8)(new MeshNode))
    val newRootNodeInfo = PowerOfTwoCube(rootNodePos - rootNodeSize/2, rootNodeSize*2)

    newRoot = new InnerNodeOverMesh(
          (for(i <- 0 until 8) yield {
            // 8 meshnodes with Ungenerated nodes, calls genmesh
            val children = Array.tabulate[NodeOverMesh](8){ j =>
              new MeshNode(UngeneratedNode).genMesh(newRootNodeInfo(i)(j),-1,null)
            }

            // n:InnerNodeOverMesh containing MeshNodes
            val n = root match {
              case n:InnerNodeOverMesh => n
              case n:MeshNode => n.split(rootNodeInfo)
            }
            children(~i & 7) = n.getChild(i)
            (new InnerNodeOverMesh(children)).joinChildren
          }).toArray
        )

    root = newRoot
    rootNodeInfo = newRootNodeInfo
  }

	override def fill( foo: Vec3i => Leaf ) {
		for( v <- rootNodePos until rootNodePos + rootNodeSize ) {
			this(v) = foo(v)
		}
	}

	def isSet(info:PowerOfTwoCube):Boolean = {
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

	def raytracer( ray:Ray, top:Boolean, distance:Double):Option[Vec3i] = {
		// der raytracer ist fehlerhaft falls die startposition ganzzahling ist
    val from = Vec3( ray.pos )

		for(i <- 0 until 3) {
			if(from(i) == floor(from(i)))
				from(i) += 0.000001
		}
	
		val t = ray.dir
		
		val pos = Vec3i(floor(from))
		val step = Vec3i(sign(ray.dir))
		val tMax = Vec3(0)
		val tDelta = Vec3(0)
		
		tMax.x = if(step.x == 1) (ceil(from.x)-from.x)/abs(t.x) else (from.x-floor(from.x))/abs(t.x)
		tMax.y = if(step.y == 1) (ceil(from.y)-from.y)/abs(t.y) else (from.y-floor(from.y))/abs(t.y)
		tMax.z = if(step.z == 1) (ceil(from.z)-from.z)/abs(t.z) else (from.z-floor(from.z))/abs(t.z)
		
		tDelta.x = 1/abs(t.x)
		tDelta.y = 1/abs(t.y)
		tDelta.z = 1/abs(t.z)
		
		var h:Polyeder = apply(pos).h
		if(!util.rayPolyederIntersect( Ray(from-pos,ray.dir), h ))
			h = null
		var i = 0
		
		// todo octreeoptimierung
		var axis = 0
		
		while(h == null && i < distance) {
			if(tMax.x < tMax.y) {
				if(tMax.x < tMax.z) {
					axis = 0
					pos.x += step.x
					tMax.x += tDelta.x
				} else {
					axis = 2
					pos.z += step.z;
					tMax.z += tDelta.z;
				}
			} else {
				if(tMax.y < tMax.z) {
					axis = 1
					pos.y += step.y;
					tMax.y+= tDelta.y;
				} else {
					axis = 2
					pos.z += step.z;
					tMax.z += tDelta.z;
				}
			}

			h = apply(pos).h

			if(!util.rayPolyederIntersect( Ray(from-pos,ray.dir), h))
				h = null
			
			i += 1
		}
		
		val prepos = pos.clone
		prepos(axis) -= step(axis)
		
		if(h != null){
			if(top && rayCellTest( Ray(from-pos,ray.dir), h.asInstanceOf[Hexaeder]))
				Some(prepos)
			else
				Some(pos)
		}
		else
			None
	}

  /*def toMessage = {
    import downearth.message.implicits._
    message.Octree(
      pos  = rootNodePos,
      size = rootNodeSize,
      root = root.toMessage
    )
  }}*/
}
