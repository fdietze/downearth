package downearth.worldoctree

import downearth.{Config}
import simplex3d.math.Vec3i
import simplex3d.math.double.ConstVec3
import scala.collection.mutable
import downearth.rendering.{Update, TextureMesh, TextureMeshBuilder, ObjMesh}
import simplex3d.math.floatx.Vec3f

/**
 * User: arne
 * Date: 27.05.13
 * Time: 01:02
 */

object MeshNode {
  implicit class MeshNodeArray(meshNodes:Array[MeshNode]) {
    def join = {
      val mesh = TextureMesh( meshNodes.map(_.mesh) )
        val node = new InnerNodeUnderMesh( meshNodes.map(_.node) )

        for(i <- 0 until 8) {
          node.geometryByteCount(i) = meshNodes(i).mesh.byteSize
          meshNodes(i).mesh.freevbo()
        }

        val meshNode = new MeshNode(node.merge)
        meshNodes.foreach( meshNode.objMeshes ++= _.objMeshes )

        meshNode.mesh = mesh
        meshNode
      }
  }

  def ungenerated = {
    val meshNode = new MeshNode(UngeneratedNode)
    meshNode.mesh = TextureMesh.empty
    meshNode
  }

  def generating = {
    val meshNode = new MeshNode(GeneratingNode)
    meshNode.mesh = TextureMesh.empty
    meshNode
  }
}

//decorator pattern
class MeshNode(var node:NodeUnderMesh) extends NodeOverMesh {
//  assert(!node.isInstanceOf[Leaf])
  var mesh:TextureMesh = null

  val objMeshes = new mutable.ArrayBuffer[(PowerOfTwoCube,ObjMesh)]

  override def toString = s"MeshNode(node=$node, mesh=$mesh)"

  // Nodes unter dem MeshNode müssen gesetzt sein.
  def isSet(info:PowerOfTwoCube,pos:PowerOfTwoCube) = true

  override def hasChildren = node.hasChildren

  override def getChild(i:Int) = node.getChild(i)

  override def insertNode(info: PowerOfTwoCube, insertInfo: PowerOfTwoCube, insertNode: NodeOverMesh) = {
    insertNode match {
      case newNode:MeshNode =>
        if(info == insertInfo) {
          mesh.freevbo()
          newNode
        } else {
          objMeshes ++= newNode.objMeshes
          val updateInfo = node.patchWorld(info, insertInfo, newNode.node, newNode.mesh.byteSize, 0, mesh.byteSize)
          val update = Update(updateInfo.oldByteOffset, updateInfo.oldByteSize, newNode.mesh.data)

          node = updateInfo.node match {
            case n:InnerNodeUnderMesh => n.merge
            case n => n
          }
          node.refreshFinishedState()

          mesh.freevbo()
          mesh = mesh applyUpdate update

          if(mesh.byteSize > Config.maxMeshByteSize)
            this.split(info)
          else
            this
        }
    }
  }

  def apply(info: PowerOfTwoCube, p: Vec3i) = node(info,p)

  // der einzige NodeOverMesh, der genMesh implementiert
  def genMesh(info:PowerOfTwoCube, worldaccess:(Vec3i => Polyeder)) = {
    assert(mesh == null)
    val meshBuilder = new TextureMeshBuilder

    // TODO warum wird result nicht verwendet?
    val result = node.genPolygons(info, meshBuilder, worldaccess)
    mesh = TextureMesh(meshBuilder)

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

  override def updated(info:PowerOfTwoCube, pos:Vec3i, newLeaf:Leaf) : NodeOverMesh = {

    val leafInfo = PowerOfTwoCube(pos,1)

    objMeshes.find {
      case (info, mesh) => info == leafInfo
    } match {
      case Some( t ) =>
        objMeshes.remove(objMeshes.indexOf(t))
      case None =>
    }

    if( newLeaf.isInstanceOf[ObjLeaf] ) {
      println("inserting objMesh at " + info)
      objMeshes += Tuple2( PowerOfTwoCube(pos,1), newLeaf.asInstanceOf[ObjLeaf].mesh)
    }

    val (replacement,patch) = node.patchWorld(info, pos, newLeaf, 0, mesh.byteSize)
    node = replacement
    var patches = patch :: Nil

    // Nachbarn die noch innerhalb des Octanten liegen patchen
    var newsize = mesh.byteSize + patch.byteSizeDifference
    for(i <- 0 until 6) {
      val npos = pos.clone
      npos(i >> 1) += ((i&1) << 1)-1
      if( info.indexInRange(npos) ) {
        val newpatch = node.repolyWorld(info, npos, 0, newsize)
        patches ::=  newpatch
        newsize += newpatch.byteSizeDifference
      }
    }

    // mehrer patches die hintereinander abgearbeitet werden können,
    // können hier auch in einem schritt ausgeführt werden
    // da die liste von links aufgebaut wurde muss sie zuerst umgekehrt werden
    mesh.freevbo()
    mesh = mesh applyUpdates patches.reverse

    // falls das mesh an dieser Stelle die Maximalgröße Überschreitet, wird es aufgeteilt
    if( mesh.byteSize > Config.maxMeshByteSize ) {
      split(info)
    }
    else
      this
  }

  def split(info:PowerOfTwoCube) = {
    node match {
      case innernode:InnerNodeUnderMesh =>
        val newdata = new Array[NodeOverMesh](8)
        val childmeshes = mesh.split(innernode.geometryByteCount)
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

  override def repolyWorld(info:PowerOfTwoCube, p:Vec3i) {
    // vertpos und vertcount wird von node.repolyWorld gesetzt
    mesh = mesh applyUpdates List(node.repolyWorld(info,p,0,mesh.byteSize))
  }

  override def getPolygons( info:PowerOfTwoCube, pos:Vec3i) = {
    val (from,to) = node.getPolygons( info, pos, 0, mesh.byteSize )

    val reader = mesh.data.duplicate()
    for(i <- from until to) yield {
      reader.position(TextureMesh.byteStride*i+TextureMesh.vertexOffset)
      ConstVec3( glwrapper.util.getVec3f(reader, Vec3f(0)) )
    }
  }
}
