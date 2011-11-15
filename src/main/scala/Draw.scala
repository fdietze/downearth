package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import simplex3d.data._
import simplex3d.data.float._

import org.lwjgl.opengl.GL11._

import Util._
import Config._

object ConsoleFont {
	import org.newdawn.slick.UnicodeFont
	import org.newdawn.slick.font.effects._
	import java.awt.{Font,Color};
	import java.util.List
	val font = new UnicodeFont(new Font("Monospace", Font.BOLD, 14))
	font.addAsciiGlyphs
	font.addGlyphs("äöüÄÖÜß")
	val effects = font.getEffects.asInstanceOf[List[Effect]]
	effects add new ShadowEffect
	effects add new ColorEffect(Color.WHITE)
	font.loadGlyphs
	
	def height = font.getLineHeight
}

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
			glScale(size)
			plaincube
		glPopMatrix
	}
	
	def renderCuboid(size:Vec3) {
		glPushMatrix
			glScalev(size)
			plaincube
		glPopMatrix
	}
	
	def plaincube {
		glBegin(GL_LINES)
		for(i <- 0 to 1;j <- 0 to 1;k <- 0 to 1)
			glVertex3f(i,j,k)
		for(i <- 0 to 1;j <- 0 to 1;k <- 0 to 1)
			glVertex3f(i,k,j)
		for(i <- 0 to 1;j <- 0 to 1;k <- 0 to 1)
			glVertex3f(k,i,j)
		glEnd
	}
	
	def crossHair{
		glPushMatrix
			glTranslatef(screenWidth/2,screenHeight/2,0)
			glDisable(GL_LIGHTING)
			glDisable(GL_TEXTURE_2D)
			glColor3f(1,1,1)
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
		glPopMatrix
	}
	
	// rendert den Umriss eines Hexaeders, um ihn für die Selektion hervorheben zu können.
	def renderPolyeder(h:Polyeder) {
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
	
	def highlight(pos:Vec3i, polyeder:Polyeder) {
		glDisable(GL_LIGHTING)
		glDisable(GL_TEXTURE_2D)

		glPushMatrix
			glTranslatev(pos)

			// Transparent
			glDisable(GL_DEPTH_TEST)
			glEnable(GL_BLEND)
			glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
			glColor4f(1,1,1,0.25f)
			renderPolyeder(polyeder)
			glDisable(GL_BLEND)
			
			// Not Transparent
			glEnable(GL_DEPTH_TEST)
			renderPolyeder(polyeder)
		glPopMatrix
	}

	def drawNodeInfo(nodeinfo:NodeInfo) {
		import nodeinfo.{pos,size}
		glDisable(GL_LIGHTING)
		glDisable(GL_TEXTURE_2D)

		glPushMatrix
			glTranslatev(pos + 0.1f)
			Draw.renderCube(size - 0.2f)
		glPopMatrix
	}
	
	def drawCuboid(cuboid:Cuboid) {
		import cuboid.{pos,size}
		glDisable(GL_LIGHTING)
		glDisable(GL_TEXTURE_2D)

		glPushMatrix
			glTranslatev(pos + 0.1f)
			Draw.renderCuboid(cuboid.size - 0.2f)
		glPopMatrix
	}

	// Für den Debugdraw: alle Bereiche, die gesampled werden
//	var sampledNodes:List[NodeInfo] = Nil
	var predictedCuboids:List[Cuboid] = Nil
//	def addSampledNode(nodeinfo:NodeInfo) { sampledNodes ::= nodeinfo }
	def addPredictedCuboid(cuboid:Cuboid) { predictedCuboids ::= cuboid }
	
	def drawSampledNodes {
		/*
		glColor3f(1,0,0)
		for( nodeinfo <- sampledNodes )
			drawNodeInfo(nodeinfo)
		*/
		for( cuboid <- predictedCuboids ) {
			if( cuboid.isCube )
				glColor3f(0,0,1)
			else
				glColor3f(1,0,1)
			drawCuboid(cuboid)
		}
	}


	// simuliert ein Konsolenähnliches verhalten, um Text auf dem Bildschirm darzustellen
	val textCache = collection.mutable.ArrayBuffer[String]()
	
	def addText(msg:Any) {
		textCache += msg.toString
	}
	
	def drawTexts {
		import org.newdawn.slick.Color.white
		if( textCache.size > 0 ) {
			glEnable(GL_BLEND)
			glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
			val posx = 20
			var posy = 20
			for( msg <- textCache ) {
				ConsoleFont.font.drawString(posx, posy, msg, white)
				posy += ConsoleFont.height
			}
			glDisable(GL_BLEND)
			textCache.clear
		}
	}
	
	def drawDispayEvent(event:DisplayEvent, pos:Int) {
		import org.newdawn.slick.Color.white
		
		val posx = Config.screenWidth - 20 - ConsoleFont.font.getWidth(event.textMessage)
		val posy = 20 + ConsoleFont.height * pos
		
		glEnable(GL_BLEND)
		glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
		ConsoleFont.font.drawString(posx, posy, event.textMessage, white)
		glDisable(GL_BLEND)
	}
}
