package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import Util._
import Config._

import Hexaeder.planelookup
import collection.Map

trait Octant extends Serializable {
	// im Oktant wird nicht Position und Größe gespeichert, da sie sich vom
	// Elternknoten ableiten lässt. Beim Traversieren durch den baum wird diese
	// Information in Form einer Instanz von NodeInfo weitergereicht.
	
	// Greift mit absoluten Koordinaten auf den Oktant zu
	def apply(info:NodeInfo, p:Vec3i) : Hexaeder
}

// im Octree wird unterschieden, ob sich der Octant oberhalb oder unterhalb des 
// Meshes befindet
trait OctantOverMesh extends Octant {
	// Überprüft, ob ein bestimmter Teilbereich des Knotens schon generiert wurde.
	def isSet(info:NodeInfo, pos:NodeInfo):Boolean
	
	// liefert einen Knoten zurück, bei dem der Hexaeder eingefügt wurde.
	def updated(info:NodeInfo, p:Vec3i, newHexaeder:Hexaeder):OctantOverMesh
	
	// Diese Methode ist ähnlich wie patchWorld, nur ohne einen Hexaeder 
	// einzufügen, wird verwendet, um bei patchWorld an den Nachbarn den 
	// Polygonverdeckungstest aufzufrischen.
	def repolyWorld(info:NodeInfo, p:Vec3i):Unit
	
	// löst aus, dass alle Meshes in allen MeshNodes innerhalb dieses Oktants gezeichnet werden.
	def draw(info:NodeInfo, test:FrustumTest)
	
	// extrahiert alle Polygone an einer Position, extrahiert sie also aus dem 
	// Mesh heraus
	def getPolygons( info:NodeInfo, pos:Vec3i):Seq[ConstVec3] 
	
	// Ähnlich wie updated, nur dass nich ein einzelner Hexaeder eingefügt wird, 
	// sonden ein ganzer Teilbaum. Funktioniert zur Zeit nur mit MeshNodes, und 
	// Elternknoten von MeshNodes
	def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:OctantOverMesh): OctantOverMesh
}

trait OctantUnderMesh extends Octant {
	// Ersetzt im Baum an einigen stellen die Knoten durch MeshNodes, und 
	// aktiviert die Polygongenerierung. Der Baum mit InnerNodes (erbt von 
	// OctantUnderMesh) initialisiert, und anschließend werden im genMesh 
	// Schritt die Knoten ersetzt, die tatsächlich höher liegen.
	def genMesh(info:NodeInfo, dstnodesize: Int, worldaccess:(Vec3i => Hexaeder)): OctantOverMesh
	// Generiert die Polygone des gesamten Knotens, und fügt sie zum meshBuilder 
	// hinzu, worldaccess wird für den Verdeckungstest mit den Nachbarn gebraucht.
	def genPolygons(info:NodeInfo, meshBuilder:TextureMeshBuilder, worldaccess:(Vec3i =>Hexaeder)):Int	
	
	// liefert einen Knoten zurück, bei dem der Hexaeder eingefügt wurde.
	def updated(info:NodeInfo, p:Vec3i, newHexaeder:Hexaeder):OctantUnderMesh
	
	// Diese Methode ist ähnlich wie patchWorld, nur ohne einen Hexaeder 
	// einzufügen, wird verwendet, um bei patchWorld an den Nachbarn den 
	// Polygonverdeckungstest aufzufrischen.
	def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int) : Update[TextureMeshData]
	
	// extrahiert alle Polygone an einer Position in form des Bereichs von 
	// Polygonen aus dem Mesh
	def getPolygons( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int)
	
	// Ähnlich zu updated, aber diese Funktion generierd auch Updates für das 
	// Mesh, welches sich verändert.
	def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int) : (OctantUnderMesh, Update[TextureMeshData])
}



// TODO: eventuell Leaf von Hexaeder erben lassen um eine Refernz zu sparen.
class Leaf(val h:Hexaeder) extends OctantUnderMesh {
	// kann kein deadNode sein
	def isSet(info:NodeInfo,pos:NodeInfo) = true
	
	def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:Octant) = insertnode

	override def apply(info:NodeInfo, p:Vec3i) = h

	override def updated(info:NodeInfo, p:Vec3i, newHexaeder:Hexaeder) = {
		if(h == newHexaeder)
			this
		else{
			// wenn das Blatt einen größeren Bereich abdeckt der voll, 
			// bzw leer ist:
			if(info.size >= 2) {
				val replacement = new InnerNode(h)
				replacement.updated(info, p, newHexaeder)
			}
			else {
				Leaf(newHexaeder)
			}
		}
	}

	override def toString = h.toString

	override def equals(that:Any) = {
		that match {
			case l:Leaf =>
				h == l.h
			case _ =>
				false
		}
	}
	
	// erzeugt aus Zwei aneinender grenzenden Hexaedern die Polygone, die nicht verdeckt werden.
	// performance kritischer bereich, weil es für jedes benachberte Hexaederpaar aufgerufen wird
	def addSurface(from:Hexaeder, to:Hexaeder, pos:Vec3i, dir:Int ,meshBuilder:TextureMeshBuilder) = {
		if(to != UndefHexaeder){
			assert(from != EmptyHexaeder)
			
			import meshBuilder._
			
			val axis = dir >> 1
			val direction = dir & 1
			
			//die beiden achsesen, die nicht axis sind
			val axisa = 1-((axis+1) >> 1)
			val axisb = (2 - (axis >> 1))
			
			var vertexCounter = 0
			
			// val Vector(t0,t1,t2,t3,t4,t5) = from.planetriangles(axis, direction)
			val t = from.planetriangles(axis, direction)
			val t0 = t(0)
			val t1 = t(1)
			val t2 = t(2)
			val t3 = t(3)
			val t4 = t(4)
			val t5 = t(5)
			
			val occluderVertices = to.planetriangles(axis,1-direction)
			val occludingCoords = new collection.mutable.ArrayBuffer[Vec2](6) // es sind nie mehr als 6 Vertices
			
			for(ov ← occluderVertices){
				if( ov(axis) == 1-direction )
					occludingCoords += Vec2(ov(axisa),ov(axisb))
			}
			
			@inline def triangleMax(v0:Vec3, v1:Vec3, v2:Vec3) = {
				(v0(axis) == direction) && (v1(axis) == direction) && (v2(axis) == direction)
			}
			
			@inline def addVertices(v0:Vec3, v1:Vec3, v2:Vec3){
				vertexBuilder += (Vec3(pos) + v0)
				//texCoordBuilder += Vec2( v0(axisa)/2f + (direction & (axis >> 1))/2f , v0(axisb)/2f )
				vertexCounter += 1
				
				vertexBuilder += (Vec3(pos) + v1)
				//texCoordBuilder += Vec2( v1(axisa)/2f + (direction & (axis >> 1))/2f , v1(axisb)/2f )
				vertexCounter += 1
				
				vertexBuilder += (Vec3(pos) + v2)
				//texCoordBuilder += Vec2( v2(axisa)/2f + (direction & (axis >> 1))/2f , v2(axisb)/2f )
				vertexCounter += 1
				
				if( vertexMaterials ) {
					colorBuilder += materialfunction(Vec3(pos) + v0).vec4
					colorBuilder += materialfunction(Vec3(pos) + v1).vec4
					colorBuilder += materialfunction(Vec3(pos) + v2).vec4
				}
				else {
					val centerColor = materialfunction(pos + 0.5f).vec4
					colorBuilder += centerColor
					colorBuilder += centerColor
					colorBuilder += centerColor
				}

				normalBuilder += normalize(cross(v2-v1,v0-v1))
			}
			
			// liegen zwei vertices eines polygons zusammen, hat das polygon keine oberfläche und muss nicht
			// gezeichnet werden
			if( t0 != t1 && t1 != t2 && t0 != t2 ){
				if(to == EmptyHexaeder || !triangleMax(t0,t1,t2))
					addVertices(t0, t1, t2)
				else{
					val v0 = Vec2( t0(axisa), t0(axisb) )
					val v1 = Vec2( t1(axisa), t1(axisb) )
					val v2 = Vec2( t2(axisa), t2(axisb) )
					val flatTriangle = Vector(v0,v1,v2)
					if( !occludes2d(occludee=flatTriangle,occluder=occludingCoords) ) {
						addVertices(t0, t1, t2)
					}
				}
			}
			
			if(t3 != t4 && t4 != t5 && t3 != t5){
				if(to == EmptyHexaeder || !triangleMax(t3,t4,t5))
					addVertices(t3, t4, t5)
				else{
					val v0 = Vec2( t3(axisa), t3(axisb) )
					val v1 = Vec2( t4(axisa), t4(axisb) )
					val v2 = Vec2( t5(axisa), t5(axisb) )
					val flatTriangle = Vector(v0,v1,v2)
					if( !occludes2d(occludee=flatTriangle,occluder=occludingCoords) ) {
						addVertices(t3, t4, t5)
					}
				}
			}
			
			vertexCounter
		}
		else
			0
	}
	
	override def genPolygons(info:NodeInfo, meshBuilder:TextureMeshBuilder,worldaccess:(Vec3i =>Hexaeder)):Int = {
		import info.{pos => nodepos, size => nodesize}
		assert(meshBuilder != null)
		var vertexCounter = 0
		
		if(nodesize == 1) {
			if( h != EmptyHexaeder ){
				for( i <- (0 to 5) ){
					val p2 = nodepos.clone
					p2(i >> 1) += ((i&1)<<1)-1
				
					val to = worldaccess(p2)

					vertexCounter += addSurface(h,to,info.pos,i,meshBuilder)
				}
			}
		}
		else {
			if(h == FullHexaeder) {
				for( dir <- (0 to 5) ) {
					val axis = dir >> 1

					val axisa = 1-((axis+1) >> 1)
					val axisb = (2 - (axis >> 1))

					//TODO: Oberfläche eines Octanten als Quadtree abfragen
					
					for( spos <- Vec2i(0) until Vec2i(info.size) ){
						val p1 = nodepos.clone
						p1( axisa ) += spos(0)
						p1( axisb ) += spos(1)
						p1( axis )  += (nodesize-1) * (dir&1)
						val p2 = p1.clone
						p2( axis ) += ((dir & 1) << 1)-1
						val other = worldaccess(p2)

						vertexCounter += addSurface(h,other,p1,dir,meshBuilder)
					}
				}
			}
		}
		vertexCounter
	}
	
	override def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int) : (OctantUnderMesh, Update[TextureMeshData]) = {
		val replacement = updated(info, p, nh)
		
		val builder = new TextureMeshBuilder
		replacement.genPolygons(info,builder,World.apply _)
		val update = Update(vertpos,vertcount,builder.result)
		(replacement,update)
	}

	override def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int) : Update[TextureMeshData] = {
		val builder = new TextureMeshBuilder 
		genPolygons(info, builder, World.apply _ )
		Update(vertpos,vertcount,builder.result)
	}

	override def genMesh(info:NodeInfo, dstnodesize: Int, worldaccess:(Vec3i => Hexaeder) ):OctantOverMesh = {
		(new MeshNode(this)).genMesh(info,dstnodesize,worldaccess)
	}
	
	def draw(info:NodeInfo,test:FrustumTest){}
	
	override def getPolygons( info:NodeInfo, pos:Vec3i, from:Int, to:Int): (Int,Int) = {
		(from,to)
	}
}

object Leaf{
	def apply(h:Hexaeder) = {
		h match{
			case EmptyHexaeder => EmptyLeaf
			case FullHexaeder => FullLeaf
			case _ => new Leaf(h)
		}
	}
}

case object EmptyLeaf extends Leaf(EmptyHexaeder)
case object FullLeaf extends Leaf(FullHexaeder)

class InnerNodeOverMesh(val data:Array[OctantOverMesh]) extends OctantOverMesh {
	// def this(h:Hexaeder) = this(Array[OctantOverMesh]( (0 until 8).map(_ => Leaf(h)):_* ) )
	
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
	
	override def updated(info:NodeInfo, p:Vec3i, nh:Hexaeder):OctantOverMesh = {
		val (index,childinfo) = info(p)
		data(index) = data(index).updated(childinfo,p, nh)
		
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
	
	override def draw(info:NodeInfo, test:FrustumTest) {
		if( test testNode info ) {
			var i = 0;
			while(i < 8){
				data(i).draw( info(i), test )
				i += 1
			}
		}
	}
	
	override def toString = data.mkString("(",",",")")
	
	override def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:OctantOverMesh) = {
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
	def joinChildren:OctantOverMesh = {
		if( ( data map ( x => x.isInstanceOf[MeshNode]) ).reduce( _ && _ ) ) {
			// println("starting Join.")
			val meshNodes = data map (_.asInstanceOf[MeshNode])
			// println("step 0")
			var sum = 0
			for(meshnode <- meshNodes) {
				sum += meshnode.mesh.size
			}
			
			if(sum < Config.maxMeshVertexCount) {
				val mesh = MutableTextureMesh( meshNodes.map(_.mesh) )
				val node = new InnerNode( meshNodes.map(_.node) )
				
				for(i <- 0 until 8) {
					node.vvertcount(i) = meshNodes(i).mesh.size
					meshNodes(i).mesh.freevbo
				}
				
				val meshnode = new MeshNode(node.merge)
				meshnode.mesh = mesh
				
				meshnode
			}
			
			else
				this
		}
		else
			this
	}
}

class InnerNode(val data:Array[OctantUnderMesh]) extends OctantUnderMesh {
	def this(h:Hexaeder) = this( Array.fill[OctantUnderMesh](8)(Leaf(h)) )
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
	
	def updated(info:NodeInfo, p:Vec3i,h:Hexaeder) = {
		val (index,childinfo) = info(p)
		data(index) = data(index).updated(childinfo,p,h)
		merge
	}
	
	override def genPolygons(info:NodeInfo, meshBuilder:TextureMeshBuilder,worldaccess:(Vec3i =>Hexaeder)) = {
		for(i <- 0 until 8)
			vvertcount(i) = data(i).genPolygons(info(i),meshBuilder,worldaccess)
		vvertcount.sum
	}
	
	override def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int) = {
		val (index,childinfo) = info(p)
		
		val newvertpos = vertpos + vvertcount.view(0,index).sum
		val newvertcount = vvertcount(index)

		val (newNode,patch) = data(index).patchWorld(childinfo,p,nh,newvertpos,newvertcount)

		data(index) = newNode
		
		vvertcount(index) += patch.data.size - patch.size

		if(merge_?){
			val mb = new TextureMeshBuilder
			val replacement = Leaf(nh)
			replacement.genPolygons(info,mb, World.apply _)
			( replacement, Update(vertpos,vertcount,mb.result) )
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

	override def genMesh(info:NodeInfo, destnodesize:Int, worldaccess:(Vec3i => Hexaeder)) = {
		if(info.size <= destnodesize){
			val replacement = new MeshNode(this)
			replacement.genMesh(info,destnodesize,worldaccess)
		}
		else{
			val newdata = new Array[OctantOverMesh](8)
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

//decorator pattern
class MeshNode(var node:OctantUnderMesh) extends OctantOverMesh {
	// Nodes unter dem MeshNode müssen gesetzt sein.
	def isSet(info:NodeInfo,pos:NodeInfo) = true
	
	override def insertNode(info: NodeInfo, insertinfo: NodeInfo, insertnode: OctantOverMesh) = {
		throw new Exception("nodes can't be inserted into MeshNodes" + info)
	}
	
	def apply(info: NodeInfo, p: Vec3i) = node(info,p)

	var mesh:MutableTextureMesh = null
	
	// der einzige OctantOverMesh, der genMesh implementiert
	def genMesh(info:NodeInfo , destnodesize:Int, worldaccess:(Vec3i => Hexaeder)) = {
		assert(mesh == null)
		val meshBuilder = new TextureMeshBuilder
		val result = node.genPolygons(info, meshBuilder, worldaccess)
		mesh = MutableTextureMesh(meshBuilder.result)
		
		// genvbo darf hier noch nicht aufgerufen werden, weil genMesh auch in 
		// anderen Threads als dem render Thread aufgerufen wird. Um die
		// Erzeugung des vbo kümmert sich das mesh selbst beim rendern
		
		this
	}
	
	override def draw(info:NodeInfo, test:FrustumTest){
		if( test testNode info )
			mesh.draw
	}
	
	override def updated(info:NodeInfo, p:Vec3i, nh:Hexaeder) : OctantOverMesh = {
		
		val (replacement,patch) = node.patchWorld(info, p, nh, 0, mesh.size)
		node = replacement
		var patches = patch :: Nil
		
		// Nachbarn die noch innerhalb des Octanten liegen patchen
		var newsize = mesh.size+patch.sizedifference
		for(i <- 0 until 6) {
			val npos = p.clone
			npos(i >> 1) += ((i&1) << 1)-1
			if( info.indexInRange(npos) ){
				val newpatch = node.repolyWorld(info, npos, 0, newsize)
				patches ::=  newpatch
				newsize += newpatch.sizedifference
			}
		}
		
		// mehrer patches die hintereinander abgearbeitet werden können,
		// können hier auch in einem schritt ausgeführt werden
		// da die liste von links aufgebaut wurde muss sie zuerst umgekehrt werden
		mesh applyUpdates patches.reverse
		
		// falls das mesh an dieser Stelle die Maximalgröße Überschreitet, wird es aufgeteilt
		if( mesh.size > Config.maxMeshVertexCount ){
			// println("mesh splitting")
			
			node match {
				case innernode:InnerNode =>
					val newdata = new Array[OctantOverMesh](8)
					val childmeshes = mesh.split(innernode.vvertcount)
					for(i ← 0 until 8){
						val meshnode = new MeshNode(innernode.data(i))
						meshnode.mesh = childmeshes(i)
						newdata(i) = meshnode
					}
					mesh.freevbo
					new InnerNodeOverMesh(newdata)
				case leaf:Leaf =>
					// kommt nur ganz selten vor, denn Blätter haben 
					// meist nicht viele Polygone
					println("Blatt kann nicht geteilt werden")
					this
			}
		}
		else
			this
	}

	override def repolyWorld(info:NodeInfo, p:Vec3i) = {
		// vertpos und vertcount wird von node.repolyWorld gesetzt
		mesh applyUpdates List(node.repolyWorld(info,p,0,mesh.size))
	}
	
	override def getPolygons( info:NodeInfo, pos:Vec3i) = {
		val (from,to) = node.getPolygons( info, pos, 0, mesh.size )
		(from until to) map mesh.vertices
	}
}

object DeadInnerNode extends OctantOverMesh {
	def isSet(info:NodeInfo,pos:NodeInfo) = false
	def apply(info:NodeInfo, p:Vec3i) = Config.ungeneratedDefault
	
	//similar to updated, but this function also generates patches to update the mesh
	override def updated(info:NodeInfo, p:Vec3i, nh:Hexaeder) : OctantOverMesh = {
		println("dead nodes can't be patched")
		this
	}
	
	override def repolyWorld(info: NodeInfo, p: Vec3i){}
	
	override def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:OctantOverMesh):OctantOverMesh = {
		// assert(nodesize >= insertnodesize)
		
		if(info == insertinfo)
			insertnode
		else{
			val replacement = new InnerNodeOverMesh(Array.fill[OctantOverMesh](8)(DeadInnerNode))
			val (index, childinfo) = info(insertinfo.pos)
			replacement.data(index) = insertNode(childinfo,insertinfo,insertnode)
			replacement
		}
	}
	
	// deadNodes sind unsichtbar
	def draw(info:NodeInfo, test:FrustumTest){
		// TODO Hier muss die Anfrage an dien Nodegenerator und die Prediction gestartet werden.
	}
	
	// deadNodes haben keine Polygone
	override def getPolygons( info:NodeInfo, pos:Vec3i) = Nil
}

