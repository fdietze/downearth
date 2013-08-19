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
import downearth._
import downearth.Config._
import collection.mutable
import downearth.rendering.TextureMesh
import downearth.AkkaMessages._
import akka.actor.ActorRef
import downearth.util
import scala.Some
import downearth.generation.WorldGenerator
import downearth.worldoctree.Node.{TraverseDetail, Traverse}

// Kapselung für die OctreeNodes
class WorldOctree(var rootArea:PowerOfTwoCube, var root:NodeOverMesh = MeshNode.ungenerated, gameState:GameState) extends Data3D[Leaf] {
  import gameState._

  // for Data3D interface
  def vsize = rootArea.vsize
  override def indexInRange(pos:ReadVec3i) = rootArea indexInRange pos

  def apply(p:ReadVec3i) = {
    if( rootArea.indexInRange(p) )
      root(rootArea,p)
    else
      Config.ungeneratedDefault
  }

  def update(p:ReadVec3i,l:Leaf) {
    physics.worldChange(p)

    if(rootArea.indexInRange(p))
      root = root.updated(rootArea, this, p, l)
    else
      println(s"update at $p out of range: $rootArea")
  }

  def generateInitialAreaAroundPlayer() {
    println("generating initial area...")
    // creata an octree aligned node around the player and generate it
    val nodeSize = nextPowerOfTwo((playerRadius * 2).toInt)
    val start = Vec3i(startPos)
    val startMin = start - playerRadius.toInt
    val startMax = start + playerRadius.toInt
    val lowerIndex = divFloor(startMin, nodeSize)
    val higherIndex = divCeil(startMax, nodeSize)
    for( pos <- lowerIndex until higherIndex ) {
      val area = PowerOfTwoCube(pos*nodeSize, nodeSize)
      val node = WorldGenerator.generateNode(area)
      octree.insert(area, node)
    }
  }

  def traverse(culling:Culling = CullNothing, cameraPos:ReadVec3 = null)(action: TraverseDetail => Boolean ) =
    root.traverse(rootArea, culling, cameraPos)(action)

  // insert Node "that" at position and size of nodeinfo
  def insert( nodeInfo:PowerOfTwoCube, that:NodeOverMesh ) {
    if( !(nodeInfo inside rootArea) )
      incDepth()

    root = root.insertNode(rootArea, nodeInfo, that)
  }

  override def toString = "Octree("+root.toString+")"

  def getNextUngenerated:Option[PowerOfTwoCube] = {
    var node:Option[PowerOfTwoCube] = None
    traverse(){
      case Traverse(area, UngeneratedNode) =>
        node = Some(area)
        false
      case _ =>
        !node.isDefined
    }
    node
  }

  def ungeneratedAreasIn(area:CubeLike):IndexedSeq[PowerOfTwoCube] = {
    val ungenerated = new mutable.ArrayBuffer[PowerOfTwoCube]
    traverse(culling = new CubeCulling(area)) {
      case Traverse(area, UngeneratedNode) =>
        ungenerated += area
        false
      case Traverse(area, GeneratingNode) =>
        false
      case Traverse(area, node:MeshNode) =>
        true
      case Traverse(area, node:NodeUnderMesh) =>
        !node.finishedGeneration
      case _ =>
        true
    }
    ungenerated
  }

  // mark area as ungenerated and ask
  // Worldgenerator to generate it
  def generateArea(area:PowerOfTwoCube) {
    if(!Config.generation) return;
    // TODO: warning if generating already generated node
    // TODO: warning if area size/position does not match a node in octree
    workers ! GeneratingJob(area, player.pos)
    insert( area, MeshNode.generating )
    frameState.generationQueueSize += 1
  }

	def stream(player:Player) {
    val outside = !(player.generationWindow inside rootArea)
    if(outside)
      incDepth()

    ungeneratedAreasIn (player.window) foreach generateArea //TODO: higher priority
	}

  def freeOldMeshNodes(){
    var old = new mutable.ArrayBuffer[MeshNode]
    val culling = new SphereCulling(player.sightSphere).inverted
    octree.traverse(culling) {
      case TraverseDetail(area, node:MeshNode, cullResult) =>
        if( node.mesh.nonEmpty && cullResult == Culling.totallyInside ) // actually outside, because inverted
          old += node
        false
      case _ => true
    }
    val meshCount = old.size
    old = old.sortBy(_.mesh.lastDraw).dropRight(Config.maxMeshCount)
    old.foreach(_.free())
    if(old.size > 0) println(s"freed ${old.size} of ${meshCount} meshes.")
  }

  // increase Octree size
  // the root becomes
  def incDepth() {
    // TODO add test for correct subdivision of the area. Depending on where the root is, it should be extended differently.
    assert( rootArea.center == Vec3i.Zero )

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
    val newRootNodeInfo = PowerOfTwoCube(rootArea.pos - rootArea.size/2, rootArea.size*2)
    val newRoot = new InnerNodeOverMesh(
      (for(i <- 0 until 8) yield {
        // 8 meshnodes with Ungenerated nodes, calls genmesh
        val children = Array.fill[NodeOverMesh](8)(MeshNode.ungenerated)

        // n:InnerNodeOverMesh containing MeshNodes
        val n = root match {
          case n:InnerNodeOverMesh => n
          case n:MeshNode => n.split(rootArea)
        }
        children(~i & 7) = n.getChild(i)
        (new InnerNodeOverMesh(children)).joinChildren
      }).toArray
    )

    rootArea = newRootNodeInfo
    root = newRoot
  }

	override def fill[V3 >: ReadVec3i]( foo: V3 => Leaf ) {
		for( v <- rootArea.pos until rootArea.pos + rootArea.size ) {
			this(v) = foo(v)
		}
	}

	def getPolygons(pos:Vec3i) = {
		if(rootArea indexInRange pos)
			root.getPolygons(rootArea,pos)
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
