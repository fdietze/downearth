package downearth.gui

import simplex3d.math._
import simplex3d.math.double._

import downearth._
import downearth.rendering.TextureManager
import simplex3d.math.doublex.functions._
import downearth.util.Logger
import downearth.tools.{TestBuildTool, Shovel, ConstructionTool, PlayerTool}
import downearth.resources.Material
import downearth.gui.Border._

class MaterialWidget(val material:Material, val position:Vec2i, val gameState:GameState)
	extends TextureWidget(gameState.textureManager.materials(material.texId))
	with InventoryItem {
  import gameState.tools.constructionTool
  import gameState.player

  val matId = material.id

	override def selected = constructionTool.selectedMaterial == matId

  override def select() {
    player.selectTool(constructionTool)
    constructionTool.selectedMaterial = matId
    println(s"selected ${material.name}")
    DisplayEventManager.showEventText("ColorMaterial " + matId)
  }
}

class ToolWidget(val tool:PlayerTool, val position:Vec2i, gameState:GameState)
	extends TextureWidget(gameState.textureManager.tools, tool.texturePos, tool.textureSize)
	with InventoryItem with Logger {

  import gameState._

  override def select() {
    player.selectTool(tool)
    log.println("Tool " + tool)
  }

	override def selected = player.activeTool eq tool
}

class ShapeWidget(val shapeId:Int, val position:Vec2i, val player:Player, val tool:ConstructionTool) extends Widget with InventoryItem {
	val preferredAngle = 30.0
	val degPerSec = 180.0
	var inOffset = 0.0
	var outOffset = 0.0
	//def degTime = Main.uptime*degPerSec/1000.0
  val degTime = 0
	var lastMouseOut = degTime - 360.0

  addReaction {
  case MouseIn =>
    if( (degTime - lastMouseOut + outOffset) >= 360.0 )
      inOffset = mod(degTime, 360.0)
  case MouseOut =>
    lastMouseOut = degTime
    outOffset = mod(degTime - inOffset, 360.0)
  }

  override def select() {
    player.selectTool(tool)
    tool.id = shapeId
    DisplayEventManager.showEventText("Shape " + shapeId)
  }
}


trait InventoryItem extends Draggable {
  case class Select(item:InventoryItem) extends WidgetEvent
  case class UnSelect(item:InventoryItem) extends WidgetEvent

  override val size = Vec2i(32)
  listenTo(this)

  def inventory = parent.asInstanceOf[Inventory]
	def selected = (inventory.selected == this)
  lineBorderColor := Vec4(1)

  def select() {}
  def unselect() {}

  addReaction {
  case MouseIn =>
    if( !selected )
      lineBorderColor := Vec4(0.6,0.8,1,1)
  case MouseOut =>
    if( !selected )
      lineBorderColor := Vec4(1)
  case MouseClicked(mousePos) =>
    lineBorderColor := Vec4(0.2,0.4,1,1)
    if(inventory.selected != null && (inventory.selected ne this) ) {
      inventory.selected.lineBorderColor := Vec4(1)
      inventory.selected.unselect()
    }
    inventory.selected = this
    select()
  case DragEnd(pos) =>
    // draw this item last to not interrupt the positioning of the others (whatever that means)
    inventory.setToTop(this)
    inventory.arrangeChildren(300)
  }
}

class Inventory(_pos:Vec2i, _size:Vec2i) extends GridPanel(_pos, _size, 40) {
  var selected:InventoryItem = null

  def setToTop( item:InventoryItem) {
    // last position will be drawn last (on top)
    val i = children.indexOf(item)
    val j =  children.size - 1

    val tmp = children(i)
    children(i) = children(j)
    children(j) = tmp
  }

  override def toString = "Inventory"
}

object Inventory {
  def gameInventoryWidget(gameState:GameState, inventoryButton:Widget, _parent:Panel) = {
    import gameState.{player, tools}
    val i = new Inventory(Vec2i(20, 200), Vec2i(200,200))

    import i._

      i.parent = _parent
      backGroundColor := Vec4(0.1,0.1,0.1,0.7)
      border = LineBorder

      val shovel = new ToolWidget( tools.shovel, position+Vec2i(40, 0), gameState ) { override def toString = "shovel"}
      val hammer = new ToolWidget( tools.constructionTool, position+Vec2i(0 , 0), gameState ) { override def toString = "hammer"}
      val superTool = new ToolWidget( tools.testBuildTool, position+Vec2i(80,0), gameState ) { override def toString = "supertool"}

      children += hammer
      children += shovel
      children += superTool

      children ++= Range(0, tools.constructionTool.all.size).map(
        i => new ShapeWidget(i, position + Vec2i(i * 40, 80), player, tools.constructionTool)
      )

      selected = shovel

      listenTo(parent)
      listenTo(inventoryButton)

      addReaction {
        case WidgetResized(w) if w eq parent =>
          val newPos = Vec2i(0)
          newPos.x = w.size.x - size.x - 20
          newPos.y = 20
          setPosition(newPos,0)
        case ButtonClicked(`inventoryButton`) =>
          visible = !visible
      }

      arrangeChildren()
    i
  }
}







