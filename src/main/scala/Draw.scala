package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import simplex3d.data._
import simplex3d.data.float._

import org.lwjgl.opengl.GL11._

// nützliche Methoden, um verschiedene Objekte zu zeichnen.
object Draw {
	def renderAxis {
		glBegin(GL_LINES)
		glColor3f(1,0,0)
		glVertex3f(0,0,0)
		glVertex3f(1,0,0)
		glColor3f(0,1,0)
		glVertex3f(0,0,0)
		glVertex3f(0,1,0)
		glColor3f(0,0,1)
		glVertex3f(0,0,0)
		glVertex3f(0,0,1)
		glEnd
	}
	
	def renderCube(size:Float) {
		glPushMatrix
		glScalef(size,size,size)
		glBegin(GL_LINES)
		for(i <- 0 to 1;j <- 0 to 1;k <- 0 to 1)
			glVertex3f(i,j,k)
		for(i <- 0 to 1;j <- 0 to 1;k <- 0 to 1)
			glVertex3f(i,k,j)
		for(i <- 0 to 1;j <- 0 to 1;k <- 0 to 1)
			glVertex3f(k,i,j)
		glEnd
		glPopMatrix
	}
	
	def crossHair{
		glBegin(GL_LINES)
		glVertex2i(-15, 0)
		glVertex2i( -5, 0)
		glVertex2i(  5, 0)
		glVertex2i( 15, 0)
		
		glVertex2i(0, -15)
		glVertex2i(0,  -5)
		glVertex2i(0,   5)
		glVertex2i(0,  15)
		glEnd
	}
	
	// rendert den Umriss eines Hexaeders, um ihn für die Selektion hervorheben zu können.
	def renderHexaeder(h:Hexaeder) {
		val verts = h.vertices
		
		val indices = Seq(0,1,2,3,4,5,6,7,0,2,1,3,4,6,5,7,0,4,1,5,2,6,3,7)
		
		try {
			glBegin(GL_LINES)
			for(v <- indices map verts)
				glVertex3f(v.x,v.y,v.z)
			glEnd
		}
		catch {
			case e:Exception => 
				println("cant draw Hexaeder: " + h + "\nvertices: " + h.vertices)
				throw e
		}
	}


	// simuliert ein Konsolenähnliches verhalten, um Text auf dem Bildschirm darzustellen
	var textCache:List[String] = Nil
	
	def addText(msg:Any) {
		textCache ::= msg.toString
	}
	
	def drawTexts{
		import org.newdawn.slick.Color.white
		if( textCache.size > 0 ) {
			glEnable(GL_BLEND)
			glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
			val pos = Vec2i(20,20)
			for( msg <- textCache ) {
				MyFont.font.drawString(pos.x, pos.y, msg, white)
				pos.y += 20
			}
			
			glDisable(GL_BLEND)
			textCache = Nil
		}
	}
}
