package xöpäx

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import Util._
import collection.Map

// TODO wird diese klasse noch gebraucht?
case class WorldNodeInfo(pos:Vec3i,size:Int,value:Hexaeder)

case class NodeInfo(pos:Vec3i,size:Int){
	def upperPos = pos+size
	// wenn die Kinder als Array3D gespeichert werden würden, dann wäre dies die Berechnung ihres index
	def indexVec(p:Vec3i,nodepos:Vec3i = pos,nodesize:Int = size) = ((p-nodepos)*2)/nodesize
	// macht aus dem Vec3i index einen flachen index, der auf ein array angewendet werden kann
	def flat(ivec:Vec3i) = ivec.x+(ivec.y<<1)+(ivec.z<<2)
	// macht aus einem flachen index wieder ein Vec3i index
	def index2vec(idx:Int) =
		Vec3i((idx & 1),(idx & 2) >> 1,(idx & 4) >> 2)
	
	def apply(p:Vec3i):(Int,NodeInfo) = {
		assert( indexInRange(p) )
		val v = indexVec(p,pos,size)
		val index = flat(v)
		val hsize = size >> 1
		(index,NodeInfo(pos+v*hsize,hsize) )
	}
	
	def apply(index:Int):NodeInfo = {
		val v = index2vec(index)
		val hsize = size >> 1
		NodeInfo(pos+v*hsize,hsize)
	}
	
	def direction(dir:Int) = dir match {
		case 0 => List(0,2,4,6)
		case 1 => List(1,3,5,7)
		case 2 => List(0,1,4,5)
		case 3 => List(2,3,6,7)
		case 4 => List(0,1,2,3)
		case 5 => List(4,5,6,7)
	}
	
	def indexInRange(p:Vec3i) = Util.indexInRange(p,pos,size)
	
	def indexInRange(p:NodeInfo):Boolean = indexInRange(p.pos) && indexInRange(p.pos+p.size-1)
	
	// currently nodeInfo does not support renctangular spaces
	// so the return has to be a Seq
	def intersection(that:NodeInfo):Iterable[Vec3i] = {
		val pos1 = max(pos,that.pos)
		val pos2 = min(upperPos,that.upperPos)
		pos1 until pos2
	}
	
}

trait Octant extends Serializable{
	def apply(info:NodeInfo, p:Vec3i) : Hexaeder
	def updated(info:NodeInfo, p:Vec3i,nh:Hexaeder):Octant

	/**generates the polygons for this Octant
	 * @return number of added vertices
	 */
	def isSet(info:NodeInfo,pos:NodeInfo):Boolean
	// creates polygons in subtree and adds them to meshBuilder
	def genPolygons(info:NodeInfo, meshBuilder:TextureMeshBuilder, worldaccess:(Vec3i =>Hexaeder)):Int
	//similar to updated, but this function also generates patches to update the mesh
	def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int) : (Octant, Patch[TextureMeshData])
	//similar to patch, but it does not change anything in the Tree
	def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int) : Patch[TextureMeshData]
	// adds InnerNodeWithVertexArray into the tree, and creates Meshes inside of them
	def genMesh(info:NodeInfo, dstnodesize: Int, worldaccess:(Vec3i => Hexaeder) ):Octant
	def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:Octant) : Octant
	def draw
	
	def getPolygonsOverVertexArray( info:NodeInfo, pos:Vec3i):Seq[ConstVec3] //return slice applied on the vertices
	def getPolygonsUnderVertexArray( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int) //return slice
	
	// removes all Futures
	def cleanFutures(info:NodeInfo):Octant
	
	// pateches one side of a Node
	def patchSurface(info:NodeInfo, dstinfo:NodeInfo, dir:Int, vertpos:Int, vertcount:Int):List[Patch[TextureMeshData]]
}

abstract class OctantOverVertexArray extends Octant
abstract class OctantUnterVertexArray extends Octant

class Leaf(val h:Hexaeder) extends OctantUnterVertexArray{
	// a leaf is always defined
	def isSet(info:NodeInfo,pos:NodeInfo) = true
	
	def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:Octant) = insertnode

	override def apply(info:NodeInfo, p:Vec3i) = h

	override def updated(info:NodeInfo, p:Vec3i,nh:Hexaeder) = {
		if(h == nh)
			this
		else{
			if(info.size >= 2) {
				// go deeper into the tree?
				val replacement = new InnerNode(h)
				replacement.updated(info,p,nh)
			}
			else {
				Leaf(nh)
			}
		}
	}

	override def toString = if(h eq null) "null" else h.toString

	override def equals(that:Any) = {
		that match {
			case l:Leaf =>
				h == l.h
			case _ =>
				false
		}
	}
	
	// Fügt die oberfläche zwischen zwei hexaedern zum meshBuilder hinzu
	def addSurface(from:Hexaeder,to:Hexaeder,pos:Vec3i,dir:Int,meshBuilder:TextureMeshBuilder) = {
		assert(meshBuilder != null)
		assert(from != EmptyHexaeder)

		import meshBuilder._
		
		val axis = dir >> 1
		val direction = dir & 1
		
		//die beiden achsesen, die nicht axis sind
		val axisa = 1-((axis+1) >> 1)
		val axisb = (2 - (axis >> 1))
		

		var vertexCounter = 0
	
		val triangleCoords = from.planetriangles(axis, direction)
		val occludingCoords = to.planetriangles(axis,1-direction).filter(v => v(axis) == 1-direction) map
				(v => Vec2(v(axisa),v(axisb)))
	
		val (t1,t2) = triangleCoords splitAt 3
	
		def triangleMax( s:Seq[Vec3] ) = {
			var isMax = true
			for( v <- s ){
				isMax = isMax && (v(axis) == direction)
			}
			isMax
		}
		
		def addVertices(t:Seq[Vec3]){
			for(v <- t){
				vertexBuilder += (Vec3(pos) + v)
				texCoordBuilder += Vec2( v(axisa)/2f + (direction & (axis >> 1))/2f , v(axisb)/2f )
				vertexCounter += 1
			}
			normalBuilder += normalize(cross(t(2)-t(1),t(0)-t(1)))
		}
		
		for( t <- List( t1, t2 ) ) {
		
			// liegen zwei vertices eines polygons zusammen, hat das polygon keine oberfläche und muss nicht
			// gezeichnet werden
			if(t(0) != t(1) && t(1) != t(2) && t(0) != t(2)){
				if(to == EmptyHexaeder || !triangleMax(t))
					addVertices(t)
				else{
					val flatTriangle = t map (v => Vec2(v(axisa),v(axisb)));
					if( !occludes2d(occludee=flatTriangle,occluder=occludingCoords) ){
						addVertices(t)
					}
				}
			}
		}
		vertexCounter
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
	
	override def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int) : (Octant, Patch[TextureMeshData]) = {
		val replacement = updated(info, p, nh)
		
		val builder = new TextureMeshBuilder
		replacement.genPolygons(info,builder,World.apply _)
		val patch = Patch(vertpos,vertcount,builder.result)
		(replacement,patch)
	}

	override def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int) : Patch[TextureMeshData] = {
		val builder = new TextureMeshBuilder 
		genPolygons(info, builder, World.apply _ )
		Patch(vertpos,vertcount,builder.result)
	}

	override def genMesh(info:NodeInfo, dstnodesize: Int, worldaccess:(Vec3i => Hexaeder) ):Octant = {
		(new InnerNodeWithVertexArray(this)).genMesh(info,dstnodesize,worldaccess)
	}
	
	def draw{}
	
	def getPolygonsOverVertexArray( info:NodeInfo, pos:Vec3i) = {
		throw new NoSuchMethodException("dont call this in Leaf")
	}
	
	def getPolygonsUnderVertexArray( info:NodeInfo, pos:Vec3i, from:Int, to:Int): (Int,Int) = {
		(from,to)
	}
	
	def cleanFutures(info:NodeInfo):Octant = {
		throw new NoSuchMethodException("dont call this in Leaf, Here shouldn't be a FutureNode")
	}
	
	def patchSurface(info:NodeInfo, dstinfo:NodeInfo, dir:Int, vertpos:Int, vertcount:Int):List[Patch[TextureMeshData]] = List(repolyWorld(info,dstinfo.pos, vertpos, vertcount))
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

class InnerNodeOverVertexArray(h:Hexaeder) extends OctantUnterVertexArray {
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
		throw new NoSuchMethodException("if InnerNodeOverVertexArray exists then a mesh should already be generated")
	}
	
	override def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int):(Octant, Patch[TextureMeshData]) = {
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

	override def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int):Patch[TextureMeshData] = {
		val (index,childinfo) = info(p)
		data(index).repolyWorld(childinfo,p, -1, -1)
		null
	}
	
	override def draw{
		for(child <- data)
			child.draw
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
			this
		}
		// TODO merge?
	}
	
	def getPolygonsOverVertexArray( info:NodeInfo, pos:Vec3i) = {
		val (index,nodeinfo) = info(pos)
		data(index).getPolygonsOverVertexArray( nodeinfo,pos )
	}
	
	def getPolygonsUnderVertexArray( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int) = {
		throw new NoSuchMethodException("dont call this over Vertex Array")
	}
	
	def cleanFutures(info:NodeInfo):Octant = {
		for(i <- 0 until 8){
			data(i) = data(i).cleanFutures( info(i) )
		}
		this
	}
	
	def patchSurface(info:NodeInfo, dstinfo:NodeInfo, dir:Int, vertpos:Int, vertcount:Int) : List[Patch[TextureMeshData]] = {
		if(dstinfo indexInRange info){ // info <= dstinfo
			val indices = info.direction(dir)
			for(i <- indices) yield {
				data(i).patchSurface(info(i),dstinfo, dir, vertpos, vertcount)
			}
			// there are no patches over vertexArray
			Nil
		}
		else{
			val (index,nodeinfo) = info(dstinfo.pos)
			assert(nodeinfo indexInRange dstinfo)
			data(index).patchSurface(nodeinfo,dstinfo,dir,vertpos,vertcount)
		}
	}
}

class InnerNode(h:Hexaeder) extends InnerNodeOverVertexArray(h) {
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
			( replacement, Patch(vertpos,vertcount,mb.result) )
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
			val replacement = new InnerNodeWithVertexArray(this)
			replacement.genMesh(info,destnodesize,worldaccess)
		}
		else{
			val replacement = new InnerNodeOverVertexArray(EmptyHexaeder)
			for(i <- 0 until  8)
				replacement.data(i) = data(i).genMesh(info(i), destnodesize, worldaccess)
			replacement
		}
	}
	
	override def getPolygonsOverVertexArray( info:NodeInfo, pos:Vec3i) = {
		throw new NoSuchMethodException("dont call this under Vertex Array")
	}
	
	override def getPolygonsUnderVertexArray( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int) = {
		val (index,nodeinfo) = info(pos)
		val newfrom = from+vvertcount.view(0,index).sum
		val newto = newfrom + vvertcount(index)
		data(index).getPolygonsUnderVertexArray( nodeinfo,pos, newfrom, newto )
	}
	
	override def cleanFutures(info:NodeInfo):Octant = {
		throw new NoSuchMethodException("FutureNodes shouldn exist under Vertex Array")
	}
	
	override def patchSurface(info:NodeInfo, dstinfo:NodeInfo, dir:Int, vertpos:Int, vertcount:Int) : List[Patch[TextureMeshData]] = {
		if(dstinfo indexInRange info){ // info <= dstinfo
			val indices = info.direction(dir)
			var patches:List[Patch[TextureMeshData]] = Nil
			for(i <- indices) {
				val childpatches = data(i).patchSurface(info(i),dstinfo, dir, vvertcount.view(0,i).sum, vvertcount(i))
				val sizedifference = childpatches.map(_.sizedifference).sum
				vvertcount(i) += sizedifference
				patches :::= childpatches
			}
			patches
		}
		else{
			val (index,nodeinfo) = info(dstinfo.pos)
			assert(nodeinfo indexInRange dstinfo)
			data(index).patchSurface(nodeinfo,dstinfo,dir,vvertcount.view(0,index).sum,vvertcount(index))
		}
	}
}

//decorator pattern
class InnerNodeWithVertexArray(var node:Octant) extends Octant {
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
		mesh = new MutableTextureMesh(meshBuilder.result)
		
		//genvbo darf hier noch nicht aufgerufen werden, weil genMesh auch in anderen Threads als dem render Thread aufgerufen wird
		//um die erzeugung des vbo kümmert sich das mesh selbst beim rendern
		//mesh.genvbo
		
		this
	}
	
	override def draw{
		mesh.draw
	}
	
	override def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int) : (Octant, Patch[TextureMeshData]) = {
		
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
		mesh patch patches.reverse
		
		// es wurde schon gepatched, deshalb muss dieser patch nicht mehr mitgeschleppt werden
		// merge auf Nodes mit Vertex Arrays ist noch nicht implementiert
		(this,null)
	}

	override def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int) = {
		// vertpos und vertcount wird von node.repolyWorld gesetzt
		mesh patch List(node.repolyWorld(info,p,0,mesh.size))
		null 
	}
	
	override def getPolygonsOverVertexArray( info:NodeInfo, pos:Vec3i) = {
		val (from,to) = node.getPolygonsUnderVertexArray( info, pos, 0, mesh.size )
		(from until to) map mesh.vertices
	}
	
	override def getPolygonsUnderVertexArray( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int) = {
		throw new NoSuchMethodException("dont call this over Vertex Array")
	}
	
	def cleanFutures(info:NodeInfo):Octant = this
	
	def patchSurface(info:NodeInfo, dstinfo:NodeInfo, dir:Int, vertpos:Int, vertcount:Int):List[Patch[TextureMeshData]] = {		
		var patches:List[Patch[TextureMeshData]] = node.patchSurface(info, dstinfo, dir, 0, mesh.size)
		mesh patch patches.reverse
		
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
	def patchWorld(info:NodeInfo, p:Vec3i, nh:Hexaeder, vertpos:Int, vertcount:Int) : (Octant, Patch[TextureMeshData]) = {
		println("dead nodes can't be patched")
		(this,null)
	}

	// TODO EmptyPatch
	def repolyWorld(info:NodeInfo, p:Vec3i, vertpos:Int, vertcount:Int) : Patch[TextureMeshData] = null

	override def genMesh(info:NodeInfo, dstnodesize: Int, worldaccess:(Vec3i => Hexaeder)) = this

	def insertNode(info:NodeInfo, insertinfo:NodeInfo, insertnode:Octant) = {
		// assert(nodesize >= insertnodesize)
		
		if(info == insertinfo)
			insertnode
		else{
			val replacement = new InnerNodeOverVertexArray(EmptyHexaeder)
			for(i <- 0 until 8)
				replacement.data(i) = DeadInnderNode

			val (index, childinfo) = info(insertinfo.pos)
			replacement.data(index) = insertNode(childinfo,insertinfo,insertnode)
			replacement
		}
		// TODO merge?
	}
	
	def draw{}
	
	override def getPolygonsOverVertexArray( info:NodeInfo, pos:Vec3i) = Nil
	
	override def getPolygonsUnderVertexArray( info:NodeInfo, pos:Vec3i, from:Int, to:Int):(Int,Int) = {
		throw new NoSuchMethodException("dont call this over Vertex Array")
	}
	
	def cleanFutures(info:NodeInfo):Octant = this
	
	def patchSurface(info:NodeInfo, dstinfo:NodeInfo, dir:Int, vertpos:Int, vertcount:Int):List[Patch[TextureMeshData]] = {
		Nil
	}
}

