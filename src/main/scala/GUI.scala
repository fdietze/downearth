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
		Draw.addText("drawcalls: " + World.drawcalls +
			", empty: " + World.emptydrawcalls + "")
		Draw.addText("frustum culled nodes: " + World.frustumculls)
		Draw.addText("")
		Draw.addText("Inventory: " + Player.inventory.materials)
		Draw.addText("Selected Block: " + World.lastraytraycedblock )
		if( !Player.isGhost ) {
			Draw.addText("Player Position: " + round10(Player.position) )
			Draw.addText("Player Velocity: " + round10(Player.velocity) )
		}

		glDisable(GL_LIGHTING)
		glDisable(GL_TEXTURE_2D)
		
		Draw.drawTexts
		DisplayEventManager.draw

		Draw.crossHair

		MainWidget.invokeDraw()
	}
}

object MainWidget extends FreePanel(Vec2i(0),Vec2i(screenWidth,screenHeight)) {
	children += new FreePanel(Vec2i(20,200), Vec2i(120,100)) {
		border = new LineBorder
		background = new ColorBackground
		
		children += new FreePanel(Vec2i(20,20), Vec2i(80,60)) {
			border = new LineBorder(Vec4(0,1,0,1))
			background = new ColorBackground(Vec4(0,0,1,0.25f))
		}
		
	}

	children += new FreePanel(Vec2i(150,200), Vec2i(120,100)) {
		border = new LineBorder
		background = new ColorBackground
		
		children += new FreePanel(Vec2i(20,20), Vec2i(80,60)) {
			border = new LineBorder(Vec4(0,1,0,1))
			background = new ColorBackground(Vec4(0,0,1,0.25f))

			children += new FreePanel(Vec2i(20,20), Vec2i(20,20)) {
				border = new LineBorder(Vec4(1,0,0,1))
				background = new ColorBackground(Vec4(1,0,0,0.25f))
			
			}
			override def mouseIn(mousePos0:Vec2i, mousePos1:Vec2i) {
				background = new ColorBackground(Vec4(0,1,0,0.25f))
			}
			override def mouseOut(mousePos0:Vec2i, mousePos1:Vec2i) {
				background = new ColorBackground(Vec4(0,0,1,0.25f))
			}
		}
	}
}
