package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import org.lwjgl.opengl.GL11._

import Config._
import Util._


// die GUI wird sebst als Kamera implementiert weil sie ihre eigene 2D Szene hat
object GUI extends Camera {

	MainWidget.children += new FreePanel(Vec2i(20,200), Vec2i(120,100)) {
		border = new LineBorder
		background = new ColorBackground
		
		children += new FreePanel(position + Vec2i(20,20), Vec2i(80,60)) {
			border = new LineBorder(Vec4(0,1,0,1))
			background = new ColorBackground(Vec4(0,0,1,0.25f))
		}
		
	}

	MainWidget.children += new FreePanel(Vec2i(150,200), Vec2i(120,100)) with Dragable {
		override def toString = "WhiteWidget"
		border = new LineBorder
		background = new ColorBackground
		
		children += new FreePanel(position + Vec2i(20,20), Vec2i(80,60)) with Dragable {
			override def toString = "BlueWidget"
			border = new LineBorder(Vec4(0,1,0,1))
			background = new ColorBackground(Vec4(0,0,1,0.25f))

			children += new FreePanel(position + Vec2i(20,20), Vec2i(20,20)) with Dragable {
				override def toString = "RedWidget"
				border = new LineBorder(Vec4(1,0,0,1))
				background = new ColorBackground(Vec4(1,0,0,0.25f))
			
			}
			override def mouseIn(mousePos0:Vec2i, mousePos1:Vec2i) {
				background = new ColorBackground(Vec4(0,1,0,0.25f))
			}
			override def mouseOut(mousePos0:Vec2i, mousePos1:Vec2i) {
				background = new ColorBackground(Vec4(0,0,1,0.25f))
			}
			override def mouseDown(mousePos:Vec2i) {
				border = new LineBorder(Vec4(1,0,0,1))
			}
			override def mouseUp(mousePos:Vec2i) {
				border = new LineBorder(Vec4(0,1,0,1))
			}
		}
	}
	
	MainWidget.children += new AutoPanel(Vec2i(200, 20), Vec2i(100,100)) with Dragable {
		
		def newWidget = new Widget(position + Vec2i(20,20), Vec2i(20,20)) with Dragable {
			override def toString = "RedWidget"
			border = new LineBorder(Vec4(1,0,0,1))
			background = new ColorBackground(Vec4(1,0,0,0.25f))
		}
		
		for( i <- 0 until 5 )
			children += newWidget
		
		override def mouseClicked(mousePos:Vec2i) = arrangeChildren
	}
	
	MainWidget.children ++= Range(0,4).map(
		i => new MaterialWidget(i,Vec2i( screenWidth - i * 48 - 48  , screenHeight - 48 ) )
	)

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
		
		glDisable( GL_LIGHTING )
		glDisable( GL_TEXTURE_2D )
		glEnable(GL_BLEND)
		
		glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
		
		Draw.drawTexts
		DisplayEventManager.draw
		
		Draw.crossHair
		
		MainWidget.invokeDraw
		
		glDisable(GL_BLEND)
	}
}


class MaterialWidget(val matId:Int, _pos:Vec2i)
	extends TextureWidget(_pos, Vec2i(32), TextureManager.materials, Vec2(matId/4f,0), Vec2(0.25f,1) ) {
	
	override def mouseDown(mousePos:Vec2i) {
		ConstructionTool.selectedMaterial = matId
		DisplayEventManager.showEventText("Material " + matId)
	}
}
