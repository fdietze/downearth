package xöpäx

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import Util._
import collection.Map

class WorldOctree(var rootNodeSize:Int,var rootNodePos:Vec3i = Vec3i(0)) extends Data3D[Hexaeder] with Serializable with Iterable[WorldNodeInfo]{

	var worldWindowPos:Vec3i = rootNodePos.clone
	val worldWindowSize:Int = rootNodeSize
	val minMeshNodeSize = 32

	val vsize = Vec3i(worldWindowSize)
	var root:Octant = new Leaf(EmptyHexaeder)

	var meshGenerated = false

	override def indexInRange(pos:Vec3i) = Util.indexInRange(pos,rootNodePos,rootNodeSize)

	def apply(p:Vec3i) = {
		if(Util.indexInRange(p,rootNodePos,rootNodeSize))
			root(p,rootNodePos,rootNodeSize)
		else
			UndefHexaeder
	}

	def update(p:Vec3i,h:Hexaeder) {
		if(Util.indexInRange(p,rootNodePos,rootNodeSize)) {
			if(meshGenerated)
				root = root.patchWorld(p,h,-1,-1, rootNodePos, rootNodeSize)._1
			else 
				root = root.updated(p,h,rootNodePos,rootNodeSize)
		}
		else{
			printf("update out of world at %s, rootNodePos: %s, rootNodeSize: %s \n",p.toString,rootNodePos.toString,rootNodeSize.toString)
		}
	}
	
	override def toString = "Octree("+root.toString+")"

	def iterator = new Iterator[WorldNodeInfo] {
		case class InnerNodeInfo(pos:Vec3i,size:Int,node:Octant)

		var history = List( InnerNodeInfo(Vec3i(0),vsize.x, root) )
		var height = Util.log2(rootNodeSize)

		def hasNext = history != Nil
		def next = history.head match{
			case InnerNodeInfo(pos,size, n:Leaf) =>
				history = history.tail
				WorldNodeInfo(pos, size, n.h)

			case InnerNodeInfo(pos, size, n:InnerNode) =>
				val hsize = size >> 1
				// Vec3i(0) until Vec3i(2)
				val ndata =
				for(i <- 0 until 8) yield {
					val offset = Vec3i(i&1,(i&2)>>1,(i&4)>>2)
					InnerNodeInfo( pos + offset*hsize, hsize, n.data(i) )
				}

				history = ndata ++: history.tail
				next
		}
	}
	
	def draw{
		import org.lwjgl.opengl.GL11._
		
		root.draw
		
		glDisable(GL_LIGHTING)
		glDisable(GL_TEXTURE_2D)
		glLineWidth(3)
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
		glLineWidth(1)
	}
	
	def genMesh {
		root = root.genMesh(rootNodePos,rootNodeSize,minMeshNodeSize,(x => {if(indexInRange(x)) apply(x) else World(x)}) )
		meshGenerated = true
	}
	
	import akka.dispatch.Future
	var queue:List[Pair[() => Boolean,() => Unit]] = Nil

	def move(dir:Vec3i){
		// checkrange
		worldWindowPos += dir * minMeshNodeSize

		val slicepos = worldWindowPos + (dir+1)/2*worldWindowSize - abs(dir) * minMeshNodeSize
		val slicesize = (Vec3i(1) - abs(dir)) * (worldWindowSize / minMeshNodeSize) + abs(dir)
		
		for(vi <- Vec3i(0) until slicesize){
			val spos = slicepos + vi*minMeshNodeSize
			
			val nodefuture = WorldNodeGenerator.generateNodeAt(spos, minMeshNodeSize)
			val ready:() => Boolean = nodefuture.isCompleted _
			val run:() => Unit = ( () => {
				val node = nodefuture.get.root
				val nodepos = nodefuture.get.rootNodePos
				val nodesize = nodefuture.get.rootNodeSize
				insert(nodepos,nodesize,node)
			})
			
			queue ::= Pair(ready,run)
		}
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
		
		root = root.insertNode(rootNodePos,rootNodeSize,that,nodepos,nodesize)
	}
	
	// fügt die im intergrund berechneten Nodes in den Baum ein
	def makeNodeUpdates{
		val (set,unset) = queue.partition( _._1() )
		
		for( (_,run) <- set )
			run()
		
		queue = unset
	}

	override def fill( foo: Vec3i => Hexaeder ){
		for( v <- rootNodePos until rootNodePos + rootNodeSize ) {
			this(v) = foo(v)
		}
	}
}
