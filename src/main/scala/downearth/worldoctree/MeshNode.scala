package downearth.worldoctree

import downearth.{Config}
import simplex3d.math.Vec3i
import scala.collection.mutable
import downearth.rendering.{MutableTextureMesh, TextureMeshBuilder, ObjMesh}

/**
 * User: arne
 * Date: 27.05.13
 * Time: 01:02
 */

object MeshNode {
  def join(meshNodes:Array[MeshNode]) = {

    // joining mesh Nodes, maybe better as method of MeshNode
    val mesh = MutableTextureMesh( meshNodes.map(_.mesh) )
    val node = new InnerNodeUnderMesh( meshNodes.map(_.node) )

    for(i <- 0 until 8) {
      node.vvertcount(i) = meshNodes(i).mesh.size
      meshNodes(i).mesh.freevbo
    }

    val meshNode = new MeshNode(node.merge)
    meshNodes.foreach( meshNode.objMeshes ++= _.objMeshes )

    meshNode.mesh = mesh
    meshNode
  }
}

//decorator pattern
class MeshNode(var node:NodeUnderMesh = UngeneratedNode) extends NodeOverMesh {
  var mesh:MutableTextureMesh = null

  val objMeshes = new mutable.ArrayBuffer[(PowerOfTwoCube,ObjMesh)]

  // Nodes unter dem MeshNode müssen gesetzt sein.
  def isSet(info:PowerOfTwoCube,pos:PowerOfTwoCube) = true

  override def hasChildren = node.hasChildren

  override def getChild(i:Int) = node.getChild(i)

  override def insertNode(info: PowerOfTwoCube, insertInfo: PowerOfTwoCube, insertNode: NodeOverMesh) = {
    insertNode match {
      case n:MeshNode =>
        if(info == insertInfo) {
          mesh.freevbo()
          n
        } else {
          objMeshes ++= n.objMeshes
          val updateInfo = node.patchWorld(info, insertInfo, n.node, n.mesh.size, 0, mesh.size)
          node = updateInfo.node

          // TODO vertices ins mesh einfügen
          this
        }
    }


    //node = node.insertNode(area, insertArea, insertNode)
  }

  def apply(info: PowerOfTwoCube, p: Vec3i) = node(info,p)

  // der einzige NodeOverMesh, der genMesh implementiert
  def genMesh(info:PowerOfTwoCube , destnodesize:Int, worldaccess:(Vec3i => Polyeder)) = {
    assert(mesh == null)
    val meshBuilder = new TextureMeshBuilder

    // TODO warum wird result nicht verwendet?
    val result = node.genPolygons(info, meshBuilder, worldaccess)
    mesh = MutableTextureMesh(meshBuilder.result)

    // genvbo darf hier noch nicht aufgerufen werden, weil genMesh auch in
    // anderen Threads als dem render Thread aufgerufen wird. Um die
    // Erzeugung des vbo kümmert sich das mesh selbst beim rendern

    // collecting all ObjMeshes from the children
    val todo = mutable.Queue( (info,node) )

    while( !todo.isEmpty ) {
      val (info,node) = todo.dequeue()
      if(node.hasChildren)
        for( i <- 0 until 8 ) {
          todo.enqueue( Tuple2(info(i), node.getChild(i).asInstanceOf[NodeUnderMesh]) )
        }

      if( node.isInstanceOf[ObjLeaf] ) {
        objMeshes += Tuple2(info, node.asInstanceOf[ObjLeaf].mesh)
      }
    }


    this
  }

  override def updated(info:PowerOfTwoCube, p:Vec3i, newLeaf:Leaf) : NodeOverMesh = {

    val leafInfo = PowerOfTwoCube(p,1)

    objMeshes.find {
      case (info, mesh) => info == leafInfo
    } match {
      case Some( t ) =>
        objMeshes.remove(objMeshes.indexOf(t))
      case None =>
    }

    if( newLeaf.isInstanceOf[ObjLeaf] ) {
      println("inserting objMesh at " + info)
      objMeshes += Tuple2( PowerOfTwoCube(p,1), newLeaf.asInstanceOf[ObjLeaf].mesh)
    }

    val (replacement,patch) = node.patchWorld(info, p, newLeaf, 0, mesh.size)
    node = replacement
    var patches = patch :: Nil

    // Nachbarn die noch innerhalb des Octanten liegen patchen
    var newsize = mesh.size+patch.sizedifference
    for(i <- 0 until 6) {
      val npos = p.clone
      npos(i >> 1) += ((i&1) << 1)-1
      if( info.indexInRange(npos) ) {
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
    if( mesh.size > Config.maxMeshVertexCount ) {
      split(info)
    }
    else
      this
  }

  def split(info:PowerOfTwoCube) = {
    node match {
      case innernode:InnerNodeUnderMesh =>
        val newdata = new Array[NodeOverMesh](8)
        val childmeshes = mesh.split(innernode.vvertcount)
        for(i <- 0 until 8) {
          val childInfo = info(i)
          val meshnode = new MeshNode(innernode.data(i))
          meshnode.objMeshes ++= objMeshes.filter {
            case (objInfo,_) => childInfo(i).indexInRange(objInfo)
          }
          meshnode.mesh = childmeshes(i)
          newdata(i) = meshnode
        }
        mesh.freevbo()
        new InnerNodeOverMesh(newdata)

      case leaf:Leaf =>
        // kommt nur ganz selten vor, denn Blätter haben
        // meist nicht viele Polygone
        throw new Exception("Blatt kann nicht geteilt werden")

      case UngeneratedNode =>
        val newdata = new Array[NodeOverMesh](8)
        for(i <- 0 until 8) {
          newdata(i) = new MeshNode(UngeneratedNode)
        }
        mesh.freevbo()
        new InnerNodeOverMesh(newdata)
    }
  }

  override def repolyWorld(info:PowerOfTwoCube, p:Vec3i) = {
    // vertpos und vertcount wird von node.repolyWorld gesetzt
    mesh applyUpdates List(node.repolyWorld(info,p,0,mesh.size))
  }

  override def getPolygons( info:PowerOfTwoCube, pos:Vec3i) = {
    val (from,to) = node.getPolygons( info, pos, 0, mesh.size )
    (from until to) map mesh.vertices
  }
}
