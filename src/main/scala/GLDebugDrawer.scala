package openworld

import com.bulletphysics.linearmath.DebugDrawModes;
import com.bulletphysics.linearmath.IDebugDraw;
import javax.vecmath.Vector3f
import collection.mutable.ArrayBuilder

import DebugDrawModes._

// für Debugzwecke von JBullet
object DirectDrawer extends IDebugDraw{
	import org.lwjgl.opengl.GL11._
	
	override def drawLine(from:Vector3f, to:Vector3f, color:Vector3f) {
		glBegin(GL_LINES)
		glColor3f(color.x,color.y,color.z)
		glVertex3f(from.x,from.y,from.z)
		glVertex3f(to.x,to.y,to.z)
		glEnd
	}
	
	var debugMode = DebugDrawModes.DRAW_WIREFRAME | DebugDrawModes.DRAW_AABB
	
	override def setDebugMode(mode:Int){
		debugMode = mode
	}
	override def getDebugMode = debugMode
	
	override def draw3dText(pos: Vector3f, text: String){}
	
	override def reportErrorWarning(warningString:String){
		println(warningString)
	}
	
	override def drawTriangle(v0:Vector3f, v1:Vector3f, v2:Vector3f, color:Vector3f, alpha:Float) {
		glColor4f(color.x,color.y,color.z,alpha)
		glBegin(GL_TRIANGLES)
		glVertex3f(v0.x, v0.y, v0.z)
		glVertex3f(v1.x, v1.y, v1.z)
		glVertex3f(v2.x, v2.y, v2.z)
		glEnd
	}
	
	override def drawTriangle(v0:Vector3f, v1:Vector3f, v2:Vector3f, n0:Vector3f, n1:Vector3f, n2:Vector3f, color:Vector3f, alpha:Float ) {
		glColor4f(color.x,color.y,color.z,alpha)
		
		glBegin(GL_TRIANGLES)
		glNormal3f(n0.x, n0.y, n0.z)
		glVertex3f(v0.x, v0.y, v0.z)
		glNormal3f(n1.x, n1.y, n0.z)
		glVertex3f(v1.x, v1.y, v1.z)
		glNormal3f(n2.x, n2.y, n0.z)
		glVertex3f(v2.x, v2.y, v2.z)
		glEnd
	}
	
	import simplex3d.math.double.Vec3
	
	override def drawContactPoint(pointOnB:Vector3f , normalOnB:Vector3f , distance:Float , lifeTime:Int, color:Vector3f ) {
		if ((debugMode & DebugDrawModes.DRAW_CONTACT_POINTS) != 0) {
			val to = Vec3(pointOnB.x,pointOnB.y,pointOnB.z)*(distance*100f)
			val from = pointOnB
			glBegin(GL_LINES)
			glColor3f(color.x,color.y,color.z)
			glVertex3f(from.x,from.y,from.z)
			glVertex3d(to.x,to.y,to.z)
			glEnd
		}
	}
}

