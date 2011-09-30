package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import Util._
import Hexaeder.planelookup
import collection.Map

trait Octant extends Serializable {
	// im Oktant wird nicht Position und Größe gespeichert, da sie sich vom
	// Elternknoten ableiten lässt. Beim Traversieren durch den baum wird diese
	// Information in Form einer Instanz von NodeInfo weitergereicht.
	
	// Greift mit absoluten Koordinaten auf den Oktant zu
	def apply(info:NodeInfo, p:Vec3i) : Hexaeder

	// liefert einen Knoten zurück, bei dem der Hexaeder eingefügt wurde.
	def updated(info:NodeInfo, p:Vec3i, newHexaeder:Hexaeder):Octant

	// Überprüft, ob ein bestimmter Teilbereich des Knotens schon generiert wurde.
	def isSet(info:NodeInfo, pos:NodeInfo):Boolean

	// Generiert die Polygone des gesamten Knotens, und fügt sie zum meshBuilder 
	// hinzu, worldaccess wird für den Verdeckungstest mit den Nachbarn gebraucht.
	def genPolygons(info:NodeInfo, meshBuilder:TextureMeshBuilder, worldaccess:(Vec3i =>Hexaeder)):Int

	// Ähnlich zu updated, aber diese Funktion generierd auch Patches, um das 
	// Mesh updaten zu können.
	def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int) : (Octant, Update[TextureMeshData])

	// Diese Methode ist ähnlich wie patchWorld, nur ohne einen Hexaeder 
	// einzufügen, wird verwendet, um bei patchWorld an den Nachbarn den 
	// Polygonverdeckungstest aufzufrischen.
	def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int) : Update[TextureMeshData]

	// Ersetzt im Baum an einigen stellen die Knoten durch MeshNodes, und 
	// aktiviert die Polygongenerierung.
	def genMesh(info:NodeInfo, dstnodesize: Int, worldaccess:(Vec3i => Hexaeder) ):Octant
	
	// Ähnlich wie updated, nur dass nich ein einzelner Hexaeder eingefügt wird, 
	// sonden ein ganzer Teilbaum. Funktioniert zur Zeit nur mit MeshNodes, und 
	// Elternknoten von MeshNodes
	def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:Octant) : Octant
	
	// löst aus, dass alle Meshes in allen MeshNodes innerhalb dieses Oktants gezeichnet werden.
	// TODO frustum-Culling
	def draw(info:NodeInfo, test:FrustumTest)
	
	def getPolygonsOverMesh( info:NodeInfo, pos:Vec3i):Seq[ConstVec3] //return slice applied on the vertices
	def getPolygonsUnderMesh( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int) //return slice
	
	// patches one side of a Node
	def patchSurface(info:NodeInfo, dstinfo:NodeInfo, dir:Int, vertpos:Int, vertcount:Int):List[Update[TextureMeshData]]
}

// NodeInfo enthält die Metainformationen für einen Knoten im Octree, also
// Position in Weltkoordanaten und Größe. Zudem hat die Klasse noch Methoden,
// um Metainformationen der Kindknoten berechnen zu können.
case class NodeInfo(pos:Vec3i, size:Int) {
	def upperPos = pos+size
	// Wenn die Kinder als Array3D gespeichert werden würden, dann wäre dies die
	// Berechnung ihres Index. Das Array3D wird nicht mehr verwendet, aber an 
	// vielen stellen wird noch sein Verhalten imitiert.
	def indexVec(p:Vec3i,nodepos:Vec3i = pos,nodesize:Int = size) = ((p-nodepos)*2)/nodesize
	
	// macht aus dem Vec3i index einen flachen index, der auf ein Array 
	// angewendet werden kann
	def flat(ivec:Vec3i) = ivec.x+(ivec.y<<1)+(ivec.z<<2)
	
	// macht aus einem flachen Index wieder ein Vec3i-Index
	def index2vec(idx:Int) =
		Vec3i((idx & 1),(idx & 2) >> 1,(idx & 4) >> 2)
	
	// Erzeugung des NodeInfo vom Kindknoten, aus einem Vektor-Index
	def apply(p:Vec3i):(Int,NodeInfo) = {
		assert( indexInRange(p) )
		val v = indexVec(p,pos,size)
		val index = flat(v)
		val hsize = size >> 1
		(index,NodeInfo(pos+v*hsize,hsize) )
	}
	
	// Erzeugung des NodeInfo vom Kindknoten, aus einem flachen Index
	def apply(index:Int):NodeInfo = {
		val v = index2vec(index)
		val hsize = size >> 1
		NodeInfo(pos+v*hsize,hsize)
	}
	
	def indexInRange(p:Vec3i) = Util.indexInRange(p,pos,size)
	
	def indexInRange(p:NodeInfo):Boolean = indexInRange(p.pos) && indexInRange(p.pos+p.size-1)
	
	// Listet alle die Koordinaten auf, die innerhalb von beiden Bereichen sind.
	def intersection(that:NodeInfo):Iterable[Vec3i] = {
		val pos1 = max(pos,that.pos)
		val pos2 = min(upperPos,that.upperPos)
		pos1 until pos2
	}
}

// im Octree wird unterschieden, ob sich der Octant oberhalb oder unterhalb des 
// Meshes befindet
trait OctantOverMesh extends Octant
trait OctantUnderMesh extends Octant


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
	def addSurface(from:Hexaeder,to:Hexaeder,pos:Vec3i,dir:Int,meshBuilder:TextureMeshBuilder) = {
		if(to != UndefHexaeder){
			assert(from != EmptyHexaeder)
			
			import meshBuilder._
			
			val axis = dir >> 1
			val direction = dir & 1
		
			//die beiden achsesen, die nicht axis sind
			val axisa = 1-((axis+1) >> 1)
			val axisb = (2 - (axis >> 1))
			
			var vertexCounter = 0
			
			val t = from.planetriangles(axis, direction)
			
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
				texCoordBuilder += Vec2( v0(axisa)/2f + (direction & (axis >> 1))/2f , v0(axisb)/2f )
				vertexCounter += 1
				
				vertexBuilder += (Vec3(pos) + v1)
				texCoordBuilder += Vec2( v1(axisa)/2f + (direction & (axis >> 1))/2f , v1(axisb)/2f )
				vertexCounter += 1
				
				vertexBuilder += (Vec3(pos) + v2)
				texCoordBuilder += Vec2( v2(axisa)/2f + (direction & (axis >> 1))/2f , v2(axisb)/2f )
				vertexCounter += 1
				
				normalBuilder += normalize(cross(v2-v1,v0-v1))
			}
			
			// liegen zwei vertices eines polygons zusammen, hat das polygon keine oberfläche und muss nicht
			// gezeichnet werden
			if(t(0) != t(1) && t(1) != t(2) && t(0) != t(2)){
				if(to == EmptyHexaeder || !triangleMax(t(0),t(1),t(2)))
					addVertices(t(0), t(1), t(2))
				else{
					val flatTriangle = t map (v => Vec2(v(axisa),v(axisb)));
					if( !occludes2d(occludee=flatTriangle,occluder=occludingCoords) ) {
						addVertices(t(0), t(1), t(2))
					}
				}
			}
			
			if(t(3) != t(4) && t(4) != t(5) && t(3) != t(5)){
				if(to == EmptyHexaeder || !triangleMax(t(3),t(4),t(5)))
					addVertices(t(3), t(4), t(5))
				else{
					val flatTriangle = t map (v => Vec2(v(axisa),v(axisb)));
					if( !occludes2d(occludee=flatTriangle,occluder=occludingCoords) ) {
						addVertices(t(3), t(4), t(5))
					}
				}
			}
			
			vertexCounter
		}
		else
			0
	}
	
	def genPolygons(info:NodeInfo, meshBuilder:TextureMeshBuilder,worldaccess:(Vec3i =>Hexaeder)):Int = {
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
			if(h == FullHexaeder){
				for( dir <- (0 to 5) ){
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
	
	override def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int) : (Octant, Update[TextureMeshData]) = {
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

	override def genMesh(info:NodeInfo, dstnodesize: Int, worldaccess:(Vec3i => Hexaeder) ):Octant = {
		(new MeshNode(this)).genMesh(info,dstnodesize,worldaccess)
	}
	
	def draw(info:NodeInfo,test:FrustumTest){}
	
	def getPolygonsOverMesh( info:NodeInfo, pos:Vec3i) = {
		throw new NoSuchMethodException("dont call this in Leaf")
	}
	
	def getPolygonsUnderMesh( info:NodeInfo, pos:Vec3i, from:Int, to:Int): (Int,Int) = {
		(from,to)
	}
	
	def patchSurface(info:NodeInfo, dstinfo:NodeInfo, dir:Int, vertpos:Int, vertcount:Int):List[Update[TextureMeshData]] = {
		List(repolyWorld(info,dstinfo.pos, vertpos, vertcount))
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

class InnerNodeOverMesh(h:Hexaeder) extends OctantUnderMesh {
	val data = new Array[Octant](8)
	//initiali the 8 child nodes
	for(fidx <- 0 until 8) {
		data(fidx) = Leaf(h)
	}
	
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

	def merge_? = {
		val first = data(0)
		var merge = true
			for(i <- data )
				merge = merge && (i == first)
		merge
	}

	def updated(info:NodeInfo, p:Vec3i,h:Hexaeder) = {
		val (index,childinfo) = info(p)

		data(index) = data(index).updated(childinfo,p,h)

		if(merge_?)
			Leaf(h)
		else
			this
	}

	def genMesh(info:NodeInfo, dstnodesize: Int, worldaccess:(Vec3i => Hexaeder) ):Octant = {
		throw new NoSuchMethodException("if InnerNodeOverMesh exists then a mesh should already be generated")
	}
	
	override def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int):(Octant, Update[TextureMeshData]) = {
		//TODO nachbarn patchen
		val (index,childinfo) = info(p)
		data(index).patchWorld(childinfo,p, nh, -1, -1)
		
		
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
			data(index).repolyWorld(NodeInfo(nodepos+nv*hsize,hsize),n,-1,-1)
		}
		
		(this,null)
	}

	override def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int):Update[TextureMeshData] = {
		val (index,childinfo) = info(p)
		data(index).repolyWorld(childinfo,p, -1, -1)
		null
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
	
	override def genPolygons(info:NodeInfo , meshBuilder:TextureMeshBuilder, worldaccess:(Vec3i =>Hexaeder)):Int = 
		throw new NoSuchMethodException("in root use genMesh instead of genPolygons")
	
	def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:Octant) = {
		
		if(info == insertinfo)
			insertnode
		else{
			val (index,childinfo) = info(insertinfo.pos)
			data(index) = data(index).insertNode(childinfo, insertinfo, insertnode)
			joinChildren
		}
		// TODO merge?
	}
	
	def getPolygonsOverMesh( info:NodeInfo, pos:Vec3i) = {
		val (index,nodeinfo) = info(pos)
		data(index).getPolygonsOverMesh( nodeinfo,pos )
	}
	
	def getPolygonsUnderMesh( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int) = {
		throw new NoSuchMethodException("dont call this over Vertex Array")
	}
	
	def patchSurface(info:NodeInfo, dstinfo:NodeInfo, dir:Int, vertpos:Int, vertcount:Int) : List[Update[TextureMeshData]] = {
		if(dstinfo indexInRange info){ // info <= dstinfo
			val indices = planelookup(dir)
			for(i <- indices) yield {
				data(i).patchSurface(info(i),dstinfo, dir, 0, 0)
			}
			// there are no patches over Mesh
			Nil
		}
		else{
			val (index,nodeinfo) = info(dstinfo.pos)
			assert(nodeinfo indexInRange dstinfo)
			data(index).patchSurface(nodeinfo,dstinfo,dir,0,0)
		}
	}
	
	def joinChildren:Octant = {
		try{
			val meshNodes = data.asInstanceOf[Array[MeshNode]]
			var sum = 0
			for(meshnode <- meshNodes){
				sum += meshnode.mesh.size
			}
			if(sum < Config.maxMeshVertexCount){
				val childmeshes = meshNodes map (_.mesh)
				val mesh = MutableTextureMesh( childmeshes )
				val node = new MeshNode(this)
				node.mesh = mesh
				childmeshes.foreach(_.freevbo)
				for(i <- 0 until 8){
					data(i) = meshNodes(i).node
				}
				node
			}
			else
				this
		}
		catch{
			case _ =>
				this
		}
	}
}

class InnerNode(h:Hexaeder) extends InnerNodeOverMesh(h) {
	// this node is under the vertex array, and may not have unset nodes
	override def isSet(info:NodeInfo,pos:NodeInfo) = true
	
	val vvertcount = new Array[Int](8)
	
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
			val replacement = new InnerNodeOverMesh(EmptyHexaeder)
			for(i <- 0 until  8)
				replacement.data(i) = data(i).genMesh(info(i), destnodesize, worldaccess)
			replacement
		}
	}
	
	override def getPolygonsOverMesh( info:NodeInfo, pos:Vec3i) = {
		throw new NoSuchMethodException("dont call this under Vertex Array")
	}
	
	override def getPolygonsUnderMesh( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int) = {
		val (index,nodeinfo) = info(pos)
		val newfrom = from+vvertcount.view(0,index).sum
		val newto = newfrom + vvertcount(index)
		data(index).getPolygonsUnderMesh( nodeinfo,pos, newfrom, newto )
	}
	
	override def patchSurface(info:NodeInfo, dstinfo:NodeInfo, dir:Int, vertpos:Int, vertcount:Int) : List[Update[TextureMeshData]] = {
		assert(vvertcount.sum == vertcount)
		
		if(dstinfo indexInRange info){ // info <= dstinfo
			val indices = planelookup(dir)
			var patches:List[Update[TextureMeshData]] = Nil
			for(i <- indices) {
				val newvertpos = vertpos + vvertcount.view(0,i).sum
				val childpatches = data(i).patchSurface(info(i),dstinfo, dir, newvertpos , vvertcount(i))
				val sizedifference = childpatches.map(_.sizedifference).sum
				vvertcount(i) += sizedifference
				patches :::= childpatches
			}
			patches
		}
		else{
			val (index,nodeinfo) = info(dstinfo.pos)
			assert(nodeinfo indexInRange dstinfo)
			val newvertpos = vertpos + vvertcount.view(0,index).sum
			data(index).patchSurface(nodeinfo,dstinfo,dir,newvertpos,vvertcount(index))
		}
	}
}

//decorator pattern
class MeshNode(var node:Octant) extends Octant {
	// Nodes under the vertex Array must be set
	def isSet(info:NodeInfo,pos:NodeInfo) = true
	
	def insertNode(info: NodeInfo, insertinfo: NodeInfo, insertnode: Octant) = {
		throw new NoSuchMethodException("no nodes can be inserted inside of inner nodes with vertex arrays")
	}
	
	def genPolygons(info: NodeInfo, meshBuilder: TextureMeshBuilder, worldaccess: (Vec3i) => Hexaeder) = {
		throw new NoSuchMethodException("an inner node with vertex array does not generate polygons")
	}
	
	def updated(info: NodeInfo, p: Vec3i, nh: Hexaeder) = {
		throw new NoSuchMethodException("use patch for inner nodes with vertex arrays instead")
	}
	
	def apply(info: NodeInfo, p: Vec3i) = node(info,p)

	var mesh:MutableTextureMesh = null
	
	override def genMesh(info:NodeInfo , destnodesize:Int, worldaccess:(Vec3i => Hexaeder)) = {
		assert(mesh == null)
		val meshBuilder = new TextureMeshBuilder
		val result = node.genPolygons(info, meshBuilder, worldaccess)
		mesh = MutableTextureMesh(meshBuilder.result)
		
		//genvbo darf hier noch nicht aufgerufen werden, weil genMesh auch in anderen Threads als dem render Thread aufgerufen wird
		//um die erzeugung des vbo kümmert sich das mesh selbst beim rendern
		//mesh.genvbo
		
		this
	}
	
	override def draw(info:NodeInfo, test:FrustumTest){
		if( test testNode info )
			mesh.draw
	}
	
	override def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int) : (Octant, Update[TextureMeshData]) = {
		
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
		
		// es wurde schon gepatched, deshalb muss dieser patch nicht mehr mitgeschleppt werden
		// merge auf Nodes mit Vertex Arrays ist noch nicht implementiert
		(this,null)
	}

	override def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int) = {
		// vertpos und vertcount wird von node.repolyWorld gesetzt
		mesh applyUpdates List(node.repolyWorld(info,p,0,mesh.size))
		null 
	}
	
	override def getPolygonsOverMesh( info:NodeInfo, pos:Vec3i) = {
		val (from,to) = node.getPolygonsUnderMesh( info, pos, 0, mesh.size )
		(from until to) map mesh.vertices
	}
	
	override def getPolygonsUnderMesh( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int) = {
		throw new NoSuchMethodException("dont call this over Vertex Array")
	}
	
	def patchSurface(info:NodeInfo, dstinfo:NodeInfo, dir:Int, vertpos:Int, vertcount:Int):List[Update[TextureMeshData]] = {		
		var patches:List[Update[TextureMeshData]] = node.patchSurface(info, dstinfo, dir, 0, mesh.size)
		mesh applyUpdates patches.reverse
		
		Nil
	}
}

object DeadInnderNode extends Octant{
	def isSet(info:NodeInfo,pos:NodeInfo) = false
	def apply(info:NodeInfo, p:Vec3i) = Config.ungeneratedDefault
	def updated(info:NodeInfo, p:Vec3i,nh:Hexaeder) = {
		println("update out of World")
		this
	}
	/**generates the polygons for this Octant
	 * @return number of added vertices
	 */
	// diese methode wird nicht gebraucht, da wir uns oberhalb der VBOs befinden, und auch keine Hexaeder definiert sind
	def genPolygons(info:NodeInfo,meshBuilder:TextureMeshBuilder,worldaccess:(Vec3i =>Hexaeder)) = {
		throw new NoSuchMethodException("dead nodes can't generate Polygons")
	}

	//similar to updated, but this function also generates patches to update the mesh
	def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int) : (Octant, Update[TextureMeshData]) = {
		println("dead nodes can't be patched")
		(this,null)
	}

	// TODO EmptyPatch
	def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int) : Update[TextureMeshData] = null

	override def genMesh(info:NodeInfo, dstnodesize: Int, worldaccess:(Vec3i => Hexaeder)) = this

	def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:Octant) = {
		// assert(nodesize >= insertnodesize)
		
		if(info == insertinfo)
			insertnode
		else{
			val replacement = new InnerNodeOverMesh(EmptyHexaeder)
			for(i <- 0 until 8)
				replacement.data(i) = DeadInnderNode

			val (index, childinfo) = info(insertinfo.pos)
			replacement.data(index) = insertNode(childinfo,insertinfo,insertnode)
			replacement
		}
		// TODO merge?
	}
	
	def draw(info:NodeInfo, test:FrustumTest){}
	
	override def getPolygonsOverMesh( info:NodeInfo, pos:Vec3i) = Nil
	
	override def getPolygonsUnderMesh( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int) = {
		throw new NoSuchMethodException("dont call this over Vertex Array")
	}
	
	def patchSurface(info:NodeInfo, dstinfo:NodeInfo, dir:Int, vertpos:Int, vertcount:Int):List[Update[TextureMeshData]] = {
		Nil
	}
}

