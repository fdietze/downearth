package downearth.worldoctree

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import downearth._
import downearth.util._
import downearth.world.World
import downearth.generation.WorldDefinition
import scala.collection.mutable.{ArrayBuffer, Queue}
import downearth.rendering.{Update, TextureMeshBuilder, TextureMeshData}

sealed trait Node {
	// im Oktant wird nicht Position und Größe gespeichert, da sie sich vom
	// Elternknoten ableiten lässt. Beim Traversieren durch den baum wird diese
	// Information in Form einer Instanz von NodeInfo weitergereicht.
	
	// Greift mit absoluten Koordinaten auf den Oktant zu
	def apply(info:NodeInfo, p:Vec3i) : Leaf

  def updated(info:NodeInfo, p:Vec3i, newLeaf:Leaf):Node

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
	// Überprüft, ob ein bestimmter Teilbereich des Knotens schon generiert wurde.
	def isSet(info:NodeInfo, pos:NodeInfo):Boolean
	
	// liefert einen Knoten zurück, bei dem der Hexaeder eingefügt wurde.
	def updated(info:NodeInfo, p:Vec3i, newLeaf:Leaf):NodeOverMesh
	
	// Diese Methode ist ähnlich wie patchWorld, nur ohne einen Hexaeder 
	// einzufügen, wird verwendet, um bei patchWorld an den Nachbarn den 
	// Polygonverdeckungstest aufzufrischen.
	def repolyWorld(info:NodeInfo, p:Vec3i):Unit
	
	// extrahiert alle Polygone an einer Position, extrahiert sie also aus dem 
	// Mesh heraus
	def getPolygons( info:NodeInfo, pos:Vec3i):Seq[ConstVec3]
	
	// Ähnlich wie updated, nur dass nicht ein einzelner Hexaeder eingefügt wird,
	// sonden ein ganzer Teilbaum. Funktioniert zur Zeit nur mit MeshNodes, und 
	// Elternknoten von MeshNodes
	def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:NodeOverMesh): NodeOverMesh
}

trait NodeUnderMesh extends Node {
	// Ersetzt im Baum an einigen stellen die Knoten durch MeshNodes, und 
	// aktiviert die Polygongenerierung. Der Baum mit InnerNodes (erbt von 
	// NodeUnderMesh) initialisiert, und anschließend werden im genMesh
	// Schritt die Knoten ersetzt, die tatsächlich höher liegen.
	def genMesh(info:NodeInfo, dstnodesize: Int, worldaccess:(Vec3i => Polyeder)): NodeOverMesh
	// Generiert die Polygone des gesamten Knotens, und fügt sie zum meshBuilder 
	// hinzu, worldaccess wird für den Verdeckungstest mit den Nachbarn gebraucht.
	def genPolygons(info:NodeInfo, meshBuilder:TextureMeshBuilder, worldaccess:(Vec3i => Polyeder)):Int
	
	// liefert einen Knoten zurück, bei dem der Hexaeder eingefügt wurde.
	def updated(info:NodeInfo, p:Vec3i, newLeaf:Leaf):NodeUnderMesh
	
	// Diese Methode ist ähnlich wie patchWorld, nur ohne einen Hexaeder 
	// einzufügen, wird verwendet, um bei patchWorld an den Nachbarn den 
	// Polygonverdeckungstest aufzufrischen.
	def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int) : Update
	
	// extrahiert alle Polygone an einer Position in form des Bereichs von 
	// Polygonen aus dem Mesh
	def getPolygons( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int)
	
	// Ähnlich zu updated, aber diese Funktion generierd auch Updates für das 
	// Mesh, welches sich verändert.
	def patchWorld(info:NodeInfo, p:Vec3i, newLeaf:Leaf, vertpos:Int, vertcount:Int) : (NodeUnderMesh, Update)
}

class InnerNodeOverMesh(val data:Array[NodeOverMesh]) extends NodeOverMesh {
  override def hasChildren = true

  override def getChild(i:Int) = data(i)
	
	def isSet(info:NodeInfo,pos:NodeInfo) = {
		assert(info indexInRange pos,"Not in range: "+info+pos)
		if(info == pos)
			true
		else{
			val (index,nodeinfo) = info(pos.pos)
			data(index).isSet(nodeinfo,pos)
		}
	}
	
	def apply(info:NodeInfo, p:Vec3i) = {
		val (index,nodeinfo) = info(p)
		data(index)(nodeinfo,p)
	}
	
	override def updated(info:NodeInfo, p:Vec3i, newLeaf:Leaf):NodeOverMesh = {
		val (index,childinfo) = info(p)
		data(index) = data(index).updated(childinfo,p, newLeaf)
		
		// TODO refactor diesen teil so weit es geht nach NodeInfo auslagern
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
			data(index).repolyWorld(NodeInfo(nodepos+nv*hsize,hsize),n)
		}
		
		joinChildren
	}

	override def repolyWorld(info:NodeInfo, p:Vec3i) = {
		val (index,childinfo) = info(p)
		data(index).repolyWorld(childinfo,p)
	}
	
	override def toString = data.mkString("(",",",")")
	
	override def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:NodeOverMesh) = {
		if(info == insertinfo)
			insertnode
		else {
			val (index,childinfo) = info(insertinfo.pos)
			data(index) = data(index).insertNode(childinfo, insertinfo, insertnode)
			joinChildren
		}
	}
	
	override def getPolygons( info:NodeInfo, pos:Vec3i) = {
		val (index,nodeinfo) = info(pos)
		data(index).getPolygons( nodeinfo,pos )
	}
	
	// falls alle Kindknoten MeshNodes sind, und zusammen weniger Vertices Haben, 
	// als Vorgeschrieben, so werden sie hier zu einem einzigen Mesh zusammengefügt
	def joinChildren:NodeOverMesh = {
		if( ( true /: data ) ( _ && _.isInstanceOf[MeshNode] ) ) {
			// println("starting Join.")
			val meshNodes = data map (_.asInstanceOf[MeshNode])
			// println("step 0")
			val sum = (0 /: meshNodes) ( _ + _.mesh.size )

			if(sum < Config.maxMeshVertexCount)
        MeshNode.join(meshNodes)
			else
				this
		}
		else
			this
	}
}

class InnerNode(val data:Array[NodeUnderMesh]) extends NodeUnderMesh {

  override def hasChildren = true

  override def getChild(i:Int) = data(i)

	def this(l:Leaf) = this( Array.fill[NodeUnderMesh](8)(l) )
	val vvertcount = new Array[Int](8)
	
	// ist nur dann wahr, wenn alle Kindknoten das selbe Blatt sind.
	def merge_? = {
		val first = data(0)
		( data(1) == first ) && ( data(2) == first ) &&
		( data(3) == first ) && ( data(4) == first ) &&
		( data(5) == first ) && ( data(6) == first ) &&
		( data(7) == first )
	}
	
	// wenn alle kinder das selbe Blatt sind, dann werden sie hier zu einem.
	def merge = {
		if( merge_? )
			data(0)
		else
			this
	}
	
	def apply(info:NodeInfo, p:Vec3i) = {
		val (index,nodeinfo) = info(p)
		data(index)(nodeinfo,p)
	}
	
	def updated(info:NodeInfo, p:Vec3i, newLeaf:Leaf) = {
		val (index,childinfo) = info(p)
		data(index) = data(index).updated(childinfo,p,newLeaf)
		merge
	}
	
	override def genPolygons(info:NodeInfo, meshBuilder:TextureMeshBuilder,worldaccess:(Vec3i => Polyeder)) = {
		for(i <- 0 until 8)
			vvertcount(i) = data(i).genPolygons(info(i),meshBuilder,worldaccess)
		vvertcount.sum
	}
	
	override def patchWorld(info:NodeInfo, p:Vec3i, newLeaf:Leaf, vertpos:Int, vertcount:Int) = {
		val (index,childinfo) = info(p)
		
		val newvertpos = vertpos + vvertcount.view(0,index).sum
		val newvertcount = vvertcount(index)

		val (newNode,patch) = data(index).patchWorld(childinfo,p,newLeaf,newvertpos,newvertcount)

		data(index) = newNode
		
		vvertcount(index) += patch.data.size - patch.size

		if(merge_?){
			val mb = new TextureMeshBuilder
			// replace with newLeaf
			newLeaf.genPolygons(info,mb, v => World.octree(v).h )
			( newLeaf, Update(vertpos,vertcount,mb.result) )
		}
		else
			(this,patch)
	}

	override def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int) = {
		val (index,childinfo) = info(p)
		val newvertpos = vertpos + vvertcount.view(0,index).sum
		val newvertcount = vvertcount(index)

		val patch = data(index).repolyWorld(childinfo, p, newvertpos, newvertcount)
		//vertexzahl hat sich geändert, und braucht ein update
		vvertcount(index) += patch.data.size - patch.size

		patch
	}

	override def genMesh(info:NodeInfo, destnodesize:Int, worldaccess:(Vec3i => Polyeder)) = {
		if(info.size <= destnodesize){
			val replacement = new MeshNode(this)
			replacement.genMesh(info,destnodesize,worldaccess)
		}
		else{
			val newdata = new Array[NodeOverMesh](8)
			for(i <- 0 until  8)
				newdata(i) = data(i).genMesh(info(i), destnodesize, worldaccess)
			val replacement = new InnerNodeOverMesh(newdata)
			replacement
		}
	}
	
	override def getPolygons( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int) = {
		val (index,nodeinfo) = info(pos)
		val newfrom = from+vvertcount.view(0,index).sum
		val newto = newfrom + vvertcount(index)
		data(index).getPolygons( nodeinfo,pos, newfrom, newto )
	}
}

/**
 * this Inner Node is used in the octree to indicate, that this node is not Generated yet
 */
abstract class LeafOverMesh extends NodeOverMesh {

  override def hasChildren = false

  override def getChild(i:Int) = throw new NoSuchElementException("ungenerated inner node doesn't have children")

	def apply(info:NodeInfo, p:Vec3i) = Config.ungeneratedDefault
	
	override def repolyWorld(info: NodeInfo, p: Vec3i){}
	
	override def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:NodeOverMesh):NodeOverMesh = {
		// assert(nodesize >= insertnodesize)
		
		if(info == insertinfo)
			insertnode
		else{
			val replacement = new InnerNodeOverMesh(Array.fill[NodeOverMesh](8)(this))
			val (index, childinfo) = info(insertinfo.pos)
			replacement.data(index) = insertNode(childinfo,insertinfo,insertnode)
			replacement
		}
	}

	// deadNodes haben keine Polygone
	override def getPolygons( info:NodeInfo, pos:Vec3i) = Nil
}

object UngeneratedInnerNode extends LeafOverMesh {
  def isSet(info:NodeInfo,pos:NodeInfo) = false

  //similar to updated, but this function also generates patches to update the mesh
  override def updated(info:NodeInfo, p:Vec3i, newLeaf:Leaf) : NodeOverMesh = {
    println("ungenerated nodes can't have updates" + info)
    this
  }
}

object GeneratingNode extends LeafOverMesh {
  def isSet(info:NodeInfo,pos:NodeInfo) = true

  //similar to updated, but this function also generates patches to update the mesh
  override def updated(info:NodeInfo, p:Vec3i, newLeaf:Leaf) : NodeOverMesh = {
    println("generating nodes can't have updates " + info)
    this
  }
}
