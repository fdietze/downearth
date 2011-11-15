package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import org.lwjgl.opengl.GL11._

import Config._
import Util._


// die GUI wird sebst als Kamera implementiert weil sie ihre eigene 2D Szene hat
object GUI extends Camera {
	
	def applyortho {
		glDisable(GL_DEPTH_TEST)
		glDisable(GL_LIGHTING)
		
		glMatrixMode(GL_PROJECTION)
		glLoadIdentity
		glOrtho(0, screenWidth, screenHeight, 0, -100, 100)
		
		glMatrixMode(GL_MODELVIEW)
		glLoadIdentity
	}
	
	def renderScene {
		glPolygonMode( GL_FRONT_AND_BACK, GL_FILL ) // no wireframes
		applyortho


		Draw.addText("%d fps" format Main.currentfps)
		Draw.addText("drawcalls: " + World.drawcalls + ", empty: " + World.emptydrawcalls + "")
		Draw.addText("frustum culled nodes: " + World.frustumculls)
		Draw.addText("")
		Draw.addText("Inventory: " + Player.inventory.materials)
		Draw.addText("Selected Block: " + World.lastraytraycedblock )
		if( !Player.isGhost ) {
			Draw.addText("Player Position: " + round10(Player.position) )
			Draw.addText("Player Velocity: " + round10(Player.velocity) )
		}

		val w = new Widget(Vec2i(20,200), Vec2i(80,60)) with Background with Border
		w.draw
		
		
		Draw.drawTexts
		DisplayEventManager.draw

		Draw.crossHair
	}
}

class Widget(val position:Vec2i, val size:Vec2i) {
	def draw {
		glDisable(GL_LIGHTING)
		glDisable(GL_TEXTURE_2D)
	}
}

trait Border extends Widget {
	override def draw {
		super.draw
		//TODO: Draw with slick?
		glColor3f(1,1,1)
		
		glBegin(GL_LINE_LOOP)
			glVertex2i(position.x         , position.y)
			glVertex2i(position.x         , position.y + size.y)
			glVertex2i(position.x + size.x, position.y + size.y)
			glVertex2i(position.x + size.x, position.y)
		glEnd
	}
}

trait Background extends Widget {
	override def draw {
		super.draw
		//TODO: Draw with slick?
		glColor4f(1,1,1,0.25f)

		glEnable(GL_BLEND)
			glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
			glBegin(GL_QUADS)
				glVertex2i(position.x         , position.y)
				glVertex2i(position.x         , position.y + size.y)
				glVertex2i(position.x + size.x, position.y + size.y)
				glVertex2i(position.x + size.x, position.y)
			glEnd
		glDisable(GL_BLEND)
	}
}
