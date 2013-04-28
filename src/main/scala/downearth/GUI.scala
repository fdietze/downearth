package downearth.gui

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import org.lwjgl.opengl.GL11._
import org.newdawn.slick.opengl.Texture
import org.lwjgl.input.Mouse

import openworld.Config._
import openworld.Util._
import openworld._

// die GUI wird sebst als Kamera implementiert weil sie ihre eigene 2D Szene hat
object GUI {
	
	val inventory = new Inventory(Vec2i(20, 200), Vec2i(200,200)) {
		background = new ColorBackground(Vec4(0.1,0.1,0.1,0.7))
		border = new LineBorder(Vec4(0,0,0,1))
		
		children += new Hammer(position+Vec2i(0 , 0))
		children += new Shovel(position+Vec2i(40, 0))
		children ++= Range(0,4).map(
			i => new MaterialWidget(i, position + Vec2i(i * 40, 40) )
		)
		
		children ++= Range(0, ConstructionTool.all.size).map(
			i => new ShapeWidget(i, position + Vec2i(i * 40, 80))
		)
		
		arrangeChildren()
		setTopRight
		
		var moved = false
		def setTopRight = setPosition(Vec2i(JavaFxMain.width.toInt - size.x - 20, 20))
		override def dragStop(mousePos:Vec2i) { moved = true }
	}
	
	MainWidget.children += inventory
	
	def applyortho {
		glDisable(GL_DEPTH_TEST)
		glDisable(GL_LIGHTING)
		
		glMatrixMode(GL_PROJECTION)
		glLoadIdentity
		glOrtho(0, JavaFxMain.width, JavaFxMain.height, 0, -100, 100)
		
		glMatrixMode(GL_MODELVIEW)
		glLoadIdentity
	}
	
	def renderScene {
		glPolygonMode( GL_FRONT_AND_BACK, GL_FILL ) // no wireframes
		applyortho
		
//  Draw.addText("%d fps" format Main.currentfps)
//	Draw.addText("drawcalls: " + World.drawcalls + ", empty: " + World.emptydrawcalls + "")
//	Draw.addText("frustum culled nodes: " + World.frustumculls)
//  Draw.addText("")
//  Draw.addText("Inventory: " + Player.inventory.materials)
//		if( !Player.isGhost ) {
//			Draw.addText("Player Position: " + round10(Player.position) )
//			Draw.addText("Player Velocity: " + round10(Player.velocity) )
//		}
		
		glDisable( GL_LIGHTING )
		glDisable( GL_TEXTURE_2D )
		glEnable(GL_BLEND)
		glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
		
		Draw.drawTexts
		DisplayEventManager.draw

		if( Mouse.isGrabbed )
			Draw.crossHair
		
		MainWidget.invokeDraw
		
		glDisable(GL_BLEND)
	}
}

class Hammer(_pos:Vec2i) extends ToolWidget( ConstructionTool, _pos, Vec2(0),      Vec2(0.5) )
class Shovel(_pos:Vec2i) extends ToolWidget( Shovel, _pos, Vec2(0.5,0), Vec2(0.5) )


class MaterialWidget(val matId:Int, _pos:Vec2i)
	extends TextureWidget(_pos, Vec2i(32), TextureManager.materials, Vec2(matId/4,0), Vec2(0.25,1) )
	with InventoryItem {
	
	override def mouseClicked(mousePos:Vec2i) {
		super.mouseClicked(mousePos)
		Player.selectTool(ConstructionTool)
		ConstructionTool.selectedMaterial = matId
		DisplayEventManager.showEventText("Material " + matId)
	}
	
	override def draw {
		super.draw

		val text = floor(Player.inventory.materials(matId)).toInt
		val textSize = Vec2i(ConsoleFont.font.getWidth(text.toString) + 2, ConsoleFont.height)
		val textPos = position + size - textSize
		import org.newdawn.slick.Color.white
		Draw.drawString(textPos, text, white)
	}
	
	override def selected = ConstructionTool.selectedMaterial == matId
}


class ToolWidget(val tool:PlayerTool, _pos:Vec2i, _texPosition:Vec2, _texSize:Vec2)
	extends TextureWidget(_pos, Vec2i(32), TextureManager.tools, _texPosition, _texSize)
	with InventoryItem {
	
	override def mouseClicked(mousePos:Vec2i) {
		super.mouseClicked(mousePos)
		Player.selectTool(tool)
		DisplayEventManager.showEventText("Tool " + tool)
	}

	override def selected = Player.activeTool eq tool
}

class ShapeWidget(val shapeId:Int, _pos:Vec2i) extends Widget(_pos, Vec2i(32)) with InventoryItem {
	val preferredAngle = 30.0
	val degPerSec = 180.0
	var inOffset = 0.0
	var outOffset = 0.0
	//def degTime = Main.uptime*degPerSec/1000.0
  val degTime = 0
	var lastMouseOut = degTime - 360.0
	
	override def draw {
		super.draw
		
		glPushMatrix
			glColor4f(1,1,1,1)
			glTranslate3dv(Vec3(position+size/2,0))
			glScalef(20,20,20)
			glRotatef(72,1,0,0)
			if( mouseOver || (degTime - lastMouseOut + outOffset) < 360.0 )
				glRotated(degTime - inOffset + preferredAngle,0,0,1)
			else
				glRotated(preferredAngle,0,0,1)
			glTranslatef(-0.5f,-0.5f,-0.5f)
			Draw.renderPolyeder(ConstructionTool.all(shapeId)(0))
		glPopMatrix
	}
	
	override def mouseIn(mousePos0:Vec2i, mousePos1:Vec2i) {
		if( (degTime - lastMouseOut + outOffset) >= 360.0 )
			inOffset = mod(degTime, 360.0)
	}
	
	override def mouseOut(mousePos0:Vec2i, mousePos1:Vec2i) {
		lastMouseOut = degTime
		outOffset = mod(degTime - inOffset, 360.0)
	}
	
	def selected = ConstructionTool.id == shapeId

	override def mouseClicked(mousePos:Vec2i) {
		super.mouseClicked(mousePos)
		Player.selectTool(ConstructionTool)
		ConstructionTool.id = shapeId
		DisplayEventManager.showEventText("Shape " + shapeId)
	}
}


trait InventoryItem extends Draggable {
	def selected:Boolean
	
	override def dragStop(mousePos:Vec2i) = {
		// draw this item last to not interrupt the positioning of the others
		// TODO: let item stay on top
		val inventory = parent
		inventory.children -= this
		inventory.children += this
		inventory.arrangeChildren(300)
	}
	
	override def draw {
		border match {
			case b:LineBorder =>
				if( selected )
					b.color := Vec4(0.2,0.4,1,1)
				else
					if( mouseOver )
						b.color := Vec4(0.6,0.8,1,1)
					else
						b.color := Vec4(1)
		}
		background match {
			case b:ColorBackground =>
				if( selected )
					b.color := Vec4(0.7,0.8,1,0.25)
				else
					if( mouseOver )
						b.color := Vec4(0.8,0.9,1,0.25)
					else
						b.color := Vec4(1,1,1,0.25)
		}
		super.draw
	}
}


class Inventory(_pos:Vec2i, _size:Vec2i) extends GridPanel(_pos, _size, 40) with Draggable






