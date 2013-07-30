package downearth.worldoctree

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import downearth._
import downearth.util._
import downearth.generation.WorldDefinition
import scala.collection.mutable.{ArrayBuffer, Queue}
import downearth.rendering.{UpdateInfo, Update, TextureMeshBuilder}

sealed trait Node {
  // im Oktant wird nicht Position und Größe gespeichert, da sie sich vom
	// Elternknoten ableiten lässt. Beim Traversieren durch den baum wird diese
	// Information in Form einer Instanz von Cube weitergereicht.
	
	// Greift mit absoluten Koordinaten auf den Oktant zu
	def apply(info:PowerOfTwoCube, p:ReadVec3i) : Leaf

  def updated(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, newLeaf:Leaf):Node

  def hasChildren:Boolean

  def getChild(i:Int):Node

  /*def toMessage:message.Octant = if(hasChildren)
    message.Octant(
      children = Vector.tabulate(8)( getChild(_).toMessage )
    )
  else
    this match {
      case leaf:UserLeaf => message.Octant(
        material = Some(leaf.material)
      )
      case _ => message.Octant()
    }*/
}

// im Octree wird unterschieden, ob sich der Node oberhalb oder unterhalb des
// Meshes befindet
trait NodeOverMesh extends Node {
	// liefert einen Knoten zurück, bei dem der Hexaeder eingefügt wurde.
	def updated(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, newLeaf:Leaf):NodeOverMesh
	
	// Diese Methode ist ähnlich wie patchWorld, nur ohne einen Hexaeder 
	// einzufügen, wird verwendet, um bei patchWorld an den Nachbarn den 
	// Polygonverdeckungstest aufzufrischen.
	def repolyWorld(area:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i):Unit
	
	// extrahiert alle Polygone an einer Position, extrahiert sie also aus dem 
	// Mesh heraus
	def getPolygons( info:PowerOfTwoCube, pos:ReadVec3i):Seq[ConstVec3]
	
	// Ähnlich wie updated, nur dass nicht ein einzelner Hexaeder eingefügt wird,
	// sonden ein ganzer Teilbaum. Funktioniert zur Zeit nur mit MeshNodes, und 
	// Elternknoten von MeshNodes
	def insertNode(info:PowerOfTwoCube, insertinfo:PowerOfTwoCube, insertnode:NodeOverMesh): NodeOverMesh
}

trait NodeUnderMesh extends Node {
	// Ersetzt im Baum an einigen stellen die Knoten durch MeshNodes, und 
	// aktiviert die Polygongenerierung. Der Baum mit InnerNodes (erbt von 
	// NodeUnderMesh) initialisiert, und anschließend werden im genMesh
	// Schritt die Knoten ersetzt, die tatsächlich höher liegen.
	def genMesh(info:PowerOfTwoCube, worldaccess:(Vec3i => Polyeder)): MeshNode
	// Generiert die Polygone des gesamten Knotens, und fügt sie zum meshBuilder 
	// hinzu, worldaccess wird für den Verdeckungstest mit den Nachbarn gebraucht.
	def genPolygons(info:PowerOfTwoCube, meshBuilder:TextureMeshBuilder, worldaccess:(Vec3i => Polyeder)):Int
	
	// liefert einen Knoten zurück, bei dem der Hexaeder eingefügt wurde.
	def updated(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, newLeaf:Leaf):NodeUnderMesh
	
	// Diese Methode ist ähnlich wie patchWorld, nur ohne einen Hexaeder 
	// einzufügen, wird verwendet, um bei patchWorld an den Nachbarn den 
	// Polygonverdeckungstest aufzufrischen.
	def repolyWorld(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, vertpos:Int, vertcount:Int) : Update
	
	// extrahiert alle Polygone an einer Position in form des Bereichs von 
	// Polygonen aus dem Mesh
	def getPolygons( info:PowerOfTwoCube, pos:ReadVec3i, from:Int, to:Int):(Int,Int)

  // Ähnlich zu updated, aber diese Funktion generiert auch Updates für das
  // Mesh, welches sich verändert.
  def patchWorld(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, newLeaf:Leaf, vertpos:Int, vertcount:Int) : (NodeUnderMesh, Update)

  def patchWorld(info:PowerOfTwoCube, insertInfo:PowerOfTwoCube, newNode:NodeUnderMesh, insertByteSize:Int, currentByteOffset:Int, currentByteSize:Int) : UpdateInfo

  // TODO: finishedState in all nodes
  // true: No generating or ungenerated nodes as children
  def finishedGeneration:Boolean
  def refreshFinishedState() = {}
}

class InnerNodeOverMesh(val data:Array[NodeOverMesh]) extends NodeOverMesh {
  override def hasChildren = true

  override def getChild(i:Int) = data(i)
	
	def apply(info:PowerOfTwoCube, p:ReadVec3i) = {
		val (index,nodeinfo) = info(p)
		data(index)(nodeinfo,p)
	}
	
	override def updated(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, newLeaf:Leaf):NodeOverMesh = {
		val (index,childinfo) = info(p)
		data(index) = data(index).updated(childinfo, octree, p, newLeaf)
		
		// TODO refactor diesen teil so weit es geht nach Cube auslagern
		import info.{pos => nodepos, size => nodesize}
		// nachbarn von p, die in verschidenen kindknoten sitzen
		val neigbours = ((0 until  6) map (i => {val v = Vec3i(0); v(i/2) = 2*(i&1)-1; p+v} )
			filter ( n => info.indexInRange(n) )
			map ( n => Pair(n, info.indexVec(n) ) )
			distinctBy( _._2 )
		)
		
		val v = info.indexVec(p)
		val hsize = info.size >> 1
		for( (n,nv) <- neigbours if(nv != v) ){
			val index = info.flat(nv)
			data(index).repolyWorld(PowerOfTwoCube(nodepos+nv*hsize,hsize), octree, n)
		}

		joinChildren
	}

	override def repolyWorld(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i) = {
		val (index,childinfo) = info(p)
		data(index).repolyWorld(childinfo, octree, p)
	}
	
	override def toString = data.mkString("InnerNodeOverMesh(",",",")")
	
	override def insertNode(info:PowerOfTwoCube, insertinfo:PowerOfTwoCube, insertnode:NodeOverMesh) = {
		if(info == insertinfo)
			insertnode
		else {
			val (index,childinfo) = info(insertinfo.pos)
			data(index) = data(index).insertNode(childinfo, insertinfo, insertnode)
			joinChildren
		}
	}
	
	override def getPolygons( info:PowerOfTwoCube, pos:ReadVec3i) = {
		val (index,nodeinfo) = info(pos)
		data(index).getPolygons( nodeinfo,pos )
	}
	
	// falls alle Kindknoten MeshNodes sind, und zusammen weniger Vertices Haben,
	// als Vorgeschrieben, so werden sie hier zu einem einzigen Mesh zusammengefügt
  //TODO: automatic join of small meshes?
  def joinChildren:NodeOverMesh = {
		def allChildrenAreMeshNodes = data.forall( _.isInstanceOf[MeshNode] )
    if( allChildrenAreMeshNodes ) {
			// println("starting Join.")
			val meshNodes = data map (_.asInstanceOf[MeshNode])
			val sum = (0 /: meshNodes) ( _ + _.mesh.byteSize )

			if(sum < Config.maxMeshByteSize)
        meshNodes.join
			else
				this
		}
		else
			this
	}
}

//TODO: merge even if prediction fails
class InnerNodeUnderMesh(val data:Array[NodeUnderMesh]) extends NodeUnderMesh {
  def allChildrenFinished = (true /: data)(_ && _.finishedGeneration)
  var finishedGeneration = allChildrenFinished
  override def refreshFinishedState() {
    data.foreach{ _.refreshFinishedState() } //TODO: smarter way than recursion?
    finishedGeneration = allChildrenFinished
  }

  override def toString = s"InnerNodeUnderMesh(finished=$finishedGeneration, geometry=${geometryByteCount.mkString("(",",","")}, sum=${geometryByteCount.sum}))"

  override def hasChildren = true

  override def getChild(i:Int) = data(i)

	def this(l:Leaf) = this( Array.fill[NodeUnderMesh](8)(l) )

	// count of bytes of the vertices for each child
  val geometryByteCount = new Array[Int](8)

	// ist nur dann wahr, wenn alle Kindknoten das selbe Blatt sind.
	def hasEqualChildren = {
		val first = data(0)
		( data(1) == first ) && ( data(2) == first ) &&
		( data(3) == first ) && ( data(4) == first ) &&
		( data(5) == first ) && ( data(6) == first ) &&
		( data(7) == first )
	}
	
	// wenn alle kinder das selbe Blatt sind, dann werden sie hier zu einem.
	def merge = {
		if( hasEqualChildren )
			data(0)
		else
			this
	}
	
	def apply(info:PowerOfTwoCube, p:ReadVec3i) = {
		val (index,nodeinfo) = info(p)
		data(index)(nodeinfo,p)
	}
	
	def updated(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, newLeaf:Leaf) = {
		val (index,childinfo) = info(p)
		data(index) = data(index).updated(childinfo, octree, p,newLeaf)
		merge
	}
	
	override def genPolygons(info:PowerOfTwoCube, meshBuilder:TextureMeshBuilder,worldaccess:(Vec3i => Polyeder)) = {
		for(i <- 0 until 8) {
      geometryByteCount(i) = data(i).genPolygons(info(i),meshBuilder,worldaccess)
      assert(geometryByteCount(i) >= 0)
    }
    geometryByteCount.sum
	}
	
	override def patchWorld(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, newLeaf:Leaf, bytePos:Int, byteCount:Int) = {
    val (index,childinfo) = info(p)
		
		val newvertpos = bytePos + geometryByteCount.view(0,index).sum
		val newvertcount = geometryByteCount(index)

		val (newNode,patch) = data(index).patchWorld(childinfo, octree, p,newLeaf,newvertpos,newvertcount)

		data(index) = newNode

    assert(geometryByteCount(index) + patch.byteSizeDifference >= 0, "geometryByteCount < 0")
    geometryByteCount(index) += patch.byteSizeDifference

		if(hasEqualChildren){
			val mb = new TextureMeshBuilder
			// replace with newLeaf
      newLeaf.genPolygons(info,mb, v => octree(v).h )
      ( newLeaf, Update(bytePos,byteCount,mb.result) )
		}
		else
			(this,patch)
	}

  override def patchWorld(info:PowerOfTwoCube, insertInfo:PowerOfTwoCube, newNode:NodeUnderMesh, insertByteSize:Int, currentByteOffset:Int, currentByteSize:Int) : UpdateInfo = {
    if(info == insertInfo)
      UpdateInfo(newNode, currentByteOffset, currentByteSize, insertByteSize )
    else {

      val (index,nodeinfo) = info(insertInfo.pos)
      val currentChild = data(index)

      val offset = currentByteOffset + geometryByteCount.take(index).sum
      val updateInfo = currentChild.patchWorld(nodeinfo, insertInfo, newNode, insertByteSize, offset, geometryByteCount(index) )
      data(index) = updateInfo.node
      data(index).refreshFinishedState()


      geometryByteCount(index) = updateInfo.newByteSize
      assert(geometryByteCount(index) >= 0)

      updateInfo.copy( node = this, newByteSize = geometryByteCount.sum )
    }
  }

	override def repolyWorld(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, vertpos:Int, vertcount:Int) = {
    require(vertpos >= 0)
    require(vertcount >= 0)

		val (index,childinfo) = info(p)
		val newvertpos = vertpos + geometryByteCount.view(0,index).sum
		val newvertcount = geometryByteCount(index)

		val patch = data(index).repolyWorld(childinfo, octree, p, newvertpos, newvertcount)
		//vertexzahl hat sich geändert, und braucht ein update
		geometryByteCount(index) += patch.byteSize - patch.byteOldSize
    assert(geometryByteCount(index) >= 0)


    patch
	}

	//TODO: rename nodeInto/info to PowerOftwoCube/area
  override def genMesh(info:PowerOfTwoCube, worldaccess:(Vec3i => Polyeder)) = {
    new MeshNode(this).genMesh(info, worldaccess)
	}
	
	override def getPolygons( info:PowerOfTwoCube, pos:ReadVec3i, from:Int, to:Int):(Int,Int) = {
		val (index,nodeinfo) = info(pos)
		val newfrom = from+geometryByteCount.view(0,index).sum
		val newto = newfrom + geometryByteCount(index)
		data(index).getPolygons( nodeinfo,pos, newfrom, newto )
	}
}

abstract class AbstractUngeneratedNode extends NodeUnderMesh {
  override def finishedGeneration = false

  def getChild(i: Int): downearth.worldoctree.Node = sys.error("UngeneratedNode does not have children")
  def hasChildren: Boolean = false
  def apply(info:PowerOfTwoCube, p:ReadVec3i): Leaf = Config.ungeneratedDefault

  def genMesh(info: PowerOfTwoCube, worldaccess: Vec3i => Polyeder) = new MeshNode(this)
  def genPolygons(info: PowerOfTwoCube,meshBuilder: TextureMeshBuilder,worldaccess: Vec3i => Polyeder): Int = 0
  def getPolygons(info: PowerOfTwoCube,pos:ReadVec3i,from: Int,to: Int): (Int, Int) = (from,to)
  def patchWorld(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, newLeaf:Leaf, vertpos:Int, vertcount:Int) : (NodeUnderMesh, Update) = {
    (this, Update(vertpos,vertcount,(new TextureMeshBuilder).result))
  }

  def patchWorld(info: PowerOfTwoCube, insertInfo: PowerOfTwoCube, newNode: NodeUnderMesh, insertByteSize: Int, currentByteOffset: Int, currentByteSize: Int): UpdateInfo = {
    if(info == insertInfo) {
      UpdateInfo(newNode, currentByteOffset, currentByteSize, insertByteSize )
    }
    else {
      val children = Array.fill[NodeUnderMesh](8)(UngeneratedNode)
      new InnerNodeUnderMesh(children).patchWorld(info, insertInfo, newNode, insertByteSize, currentByteOffset, currentByteSize)
    }
  }

  def repolyWorld(info: PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i,vertpos: Int,vertcount: Int): Update = {
    Update(vertpos,vertcount,(new TextureMeshBuilder).result)
  }

  //similar to updated, but this function also generates patches to update the mesh
  override def updated(info:PowerOfTwoCube, octree:WorldOctree, p:ReadVec3i, newLeaf:Leaf) : NodeUnderMesh = {
    println("ungenerated nodes can't have updates" + info)
    this
  }
}

object UngeneratedNode extends AbstractUngeneratedNode {
  override def toString = "UngeneratedNode"
}
object GeneratingNode extends AbstractUngeneratedNode {
  override def toString = "GeneratingNode"
}

