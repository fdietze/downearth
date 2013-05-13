package downearth.gui

import simplex3d.math._
import simplex3d.math.double._

import downearth._
import downearth.rendering.TextureManager
import simplex3d.math.doublex.functions._

class Hammer(_pos:Vec2i) extends ToolWidget( ConstructionTool, _pos, Vec2(0),      Vec2(0.5) )
class Shovel(_pos:Vec2i) extends ToolWidget( Shovel, _pos, Vec2(0.5,0), Vec2(0.5) )


class MaterialWidget(val matId:Int, _pos:Vec2i)
	extends TextureWidget(_pos, Vec2i(32), TextureManager.materials, Vec2(matId/4.0,0), Vec2(0.25,1) )
	with InventoryItem {

	override def mouseClicked(mousePos:Vec2i) {
		super.mouseClicked(mousePos)
		Player.selectTool(ConstructionTool)
		ConstructionTool.selectedMaterial = matId
		DisplayEventManager.showEventText("Material " + matId)
	}

	override def selected = ConstructionTool.selectedMaterial == matId
}

class ToolWidget(val tool:PlayerTool, _pos:Vec2i, _texPosition:Vec2, _texSize:Vec2)
	extends TextureWidget(_pos, Vec2i(32), TextureManager.tools, _texPosition, _texSize)
	with InventoryItem {

  override def select() {
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


  override def mouseIn(mousePos0:Vec2i, mousePos1:Vec2i) {
    if( (degTime - lastMouseOut + outOffset) >= 360.0 )
      inOffset = mod(degTime, 360.0)
    super.mouseIn(mousePos0, mousePos1)
  }

  override def mouseOut(mousePos0:Vec2i, mousePos1:Vec2i) {
    lastMouseOut = degTime
    outOffset = mod(degTime - inOffset, 360.0)
    super.mouseOut(mousePos0, mousePos1)
  }

	override def mouseClicked(mousePos:Vec2i) {
		super.mouseClicked(mousePos)
		Player.selectTool(ConstructionTool)

    lineBorderColor := Vec4(0.2,0.4,1,1)

    ConstructionTool.id = shapeId
		DisplayEventManager.showEventText("Shape " + shapeId)
	}

  override def select() {
    Player.selectTool(ConstructionTool)
    ConstructionTool.id = shapeId
    DisplayEventManager.showEventText("Shape " + shapeId)
  }

  override def unselect() {}
}


trait InventoryItem extends Draggable {
  def inventory = parent.asInstanceOf[Inventory]
	def selected = (inventory.selected == this)
  lineBorderColor := Vec4(1)
	
	override def dragStop(mousePos:Vec2i) = {
		// draw this item last to not interrupt the positioning of the others
		// TODO: let item stay on top
		val inventory = parent
		inventory.children -= this
		inventory.children += this
		inventory.arrangeChildren(300)
	}

  override def mouseIn(mousePos0:Vec2i, mousePos1:Vec2i) {
    if( !selected )
      lineBorderColor := Vec4(0.6,0.8,1,1)
  }

  override def mouseOut(mousePos0:Vec2i, mousePos1:Vec2i) {
    if( !selected )
      lineBorderColor := Vec4(1)
  }

  def select() {}
  def unselect() {}

  override def mouseClicked(mousePos:Vec2i) {
    super.mouseClicked(mousePos)
    lineBorderColor := Vec4(0.2,0.4,1,1)
    if(inventory.selected != null) {
      inventory.selected.lineBorderColor := Vec4(1)
      inventory.selected.unselect()
    }
    inventory.selected = this
    select()
  }

}

class Inventory(_pos:Vec2i, _size:Vec2i) extends GridPanel(_pos, _size, 40) with Draggable {
  var selected:InventoryItem = null
}







