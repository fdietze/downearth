package xöpäx

import com.bulletphysics.linearmath.DebugDrawModes;
import com.bulletphysics.linearmath.IDebugDraw;
import javax.vecmath.Vector3f
import collection.mutable.ArrayBuilder

import DebugDrawModes._

object VertexArrayDrawer extends IDebugDraw{

	private val DEBUG_NORMALS = false;
	private var debugMode:Int = DRAW_WIREFRAME | NO_DEACTIVATION
	private val tmpVec = new Vector3f();

	private var vertices = ArrayBuilder.make[Float]
	private var colors = ArrayBuilder.make[Float]
	
	override def drawLine(from:Vector3f, to:Vector3f, color:Vector3f) {
		if (debugMode > 0) {
			vertices ++= Seq(from.x,from.y,from.z,to.x,to.y,to.z)
			colors ++= Seq(color.x,color.y,color.z,color.x,color.y,color.z)
		}
	}

	override def setDebugMode(debugMode:Int) {
		this.debugMode = debugMode;
	}

	override def draw3dText(location:Vector3f, textString:String ) {
	}

	override def reportErrorWarning(warningString:String) {
		System.err.println(warningString);
	}

	override def drawContactPoint(pointOnB:Vector3f , normalOnB:Vector3f , distance:Float , lifeTime:Int, color:Vector3f ) {
		if ((debugMode & DebugDrawModes.DRAW_CONTACT_POINTS) != 0) {
			val to = tmpVec;
			to.scaleAdd(distance*100f, normalOnB, pointOnB);
			val from = pointOnB;

			// JAVA NOTE: added
			if (DEBUG_NORMALS) {
				/*
				to.normalize(normalOnB);
				to.scale(10f);
				to.add(pointOnB);

				gl.glLineWidth(3f);
				gl.glPointSize(6f);
				gl.glBegin(GL_POINTS);
				gl.glColor3f(color.x, color.y, color.z);
				gl.glVertex3f(from.x, from.y, from.z);
				gl.glEnd();
				*/
			}

			vertices ++= Seq(from.x,from.y,from.z,to.x,to.y,to.z)
			colors ++= Seq(color.x,color.y,color.z,color.x,color.y,color.z)

		}
	}

	override def getDebugMode = debugMode

	def drawAndReset{
		val vertexArray = vertices.result
		val colorArray = colors.result

		import org.lwjgl.BufferUtils.createFloatBuffer
		val vertexBuffer = createFloatBuffer(vertexArray.size)
		val colorBuffer = createFloatBuffer(colorArray.size)


		import org.lwjgl.opengl.GL11._

		//glEnableClientState(GL_COLOR_ARRAY)
		//glEnableClientState(GL_VERTEX_ARRAY)

		glVertexPointer(3,0,vertexBuffer)
		glColorPointer(3,0,colorBuffer)

		glDrawArrays(GL_LINES,0,vertexArray.size/3)

		//glDisableClientState(GL_COLOR_ARRAY)
		//glDisableClientState(GL_VERTEX_ARRAY)

		vertices = ArrayBuilder.make[Float]
		colors = ArrayBuilder.make[Float]
	}
}

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
	
	override def drawTriangle(v0:Vector3f, v1:Vector3f, v2:Vector3f, color:Vector3f, alpha:Float){
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
	
	import simplex3d.math.float.Vec3
	
	override def drawContactPoint(pointOnB:Vector3f , normalOnB:Vector3f , distance:Float , lifeTime:Int, color:Vector3f ) {
		if ((debugMode & DebugDrawModes.DRAW_CONTACT_POINTS) != 0) {
			val to = Vec3(pointOnB.x,pointOnB.y,pointOnB.z)*(distance*100f);
			val from = pointOnB
			glBegin(GL_LINES)
			glColor3f(color.x,color.y,color.z)
			glVertex3f(from.x,from.y,from.z)
			glVertex3f(to.x,to.y,to.z)
			glEnd
		}
	}
}

