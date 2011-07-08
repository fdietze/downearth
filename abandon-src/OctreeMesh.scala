package xöpäx

import com.bulletphysics.collision.{shapes, broadphase}
import shapes.{TriangleMeshShape,StridingMeshInterface}
import broadphase.BroadphaseNativeType

/*
import com.bulletphysics.linearmath.Transform
import javax.vecmath.Vector3f

class OctreeMesh(data: WorldOctree, smi: StridingMeshInterface) extends TriangleMeshShape(smi){
	def getShapeType =  BroadphaseNativeType.TRIANGLE_MESH_SHAPE_PROXYTYPE
	override def getName = getClass.getName
	// constructor
	localAabbMin.set(Float.MinValue,Float.MinValue,Float.MinValue)
	localAabbMax.set(Float.MaxValue,Float.MaxValue,Float.MaxValue)
	
	override def getLocalScaling(out: Vector3f): Vector3f = {
		out.x = 1
		out.y = 1
		out.z = 1
		out
	}
	override def setLocalScaling(scaling: Vector3f){
		throw new Exception("scaling cant be changed")
	}
}
*/