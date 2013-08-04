package downearth.gui
import Border._
import Background._
import downearth._

import org.lwjgl.opengl.Display

import simplex3d.math.Vec2i
import simplex3d.math.double._
import downearth.tools._

class MainWidget(gameState:GameState) extends Panel { mainWidget =>
  import gameState.{mainWidget => _, _ }

  val position = Vec2i(0)
  val size = Vec2i(Display.getWidth,Display.getHeight)

  border = NoBorder
  background = NoBackground

  override def setPosition(newPos:Vec2i, delay:Int) {}

  val dragStartPos = Vec2i(0)
  val startDir = Vec3(0)

  addReaction {
    case MouseClicked(pos) =>
      player.primaryAction
    case DragStart(firstPos:Vec2i) =>
      dragStartPos := firstPos
      startDir := player.dir
    case MouseDrag(mousePos0, mousePos1) =>

      val mouseDelta = Vec2(mousePos1 - mousePos0)
      mouseDelta *= 2.0 / size.y

      val deltaAngle = Vec3(mouseDelta.yx, 0)

      player.rotate(deltaAngle)
  }

  override def safePosition(newPos:Vec2i) = {
    this.position
  }

  val drawCallLabel       = new Label( Vec2i(20,20), "<not set>" )
  val playerPositionLabel = new Label( Vec2i(20,40), "<not set>" )
  val inventoryButton = new Button(    Vec2i(20,60), "inventory")
  val keySettingsButton = new Button(  Vec2i(20,80), "key settings" ){
    override  def onClick() {
      keySettingWidget.visible = !keySettingWidget.visible
      boolSettingsWidget.visible =  false
      doubleSettingsWidget.visible = false
    }
  }
  val boolSettingsButton = new Button( Vec2i(20,100), "bool settings" ){
    override  def onClick() {
      keySettingWidget.visible = false
      boolSettingsWidget.visible = !boolSettingsWidget.visible
      doubleSettingsWidget.visible = false
    }
  }
  val doubleSettingsButton = new Button( Vec2i(20,120), "double settings"){
    override def onClick() {
      keySettingWidget.visible = false
      boolSettingsWidget.visible = false
      doubleSettingsWidget.visible = !doubleSettingsWidget.visible
    }
  }

  val inventory = Inventory.gameInventory(gameState, inventoryButton, mainWidget)

  val keySettingWidget = new KeySettingsWidget( Vec2i(20,140), Config )
  keySettingWidget.visible = false
  val boolSettingsWidget = new BoolSettingsWidget( Vec2i(20,140), Config )
  boolSettingsWidget.visible = false
  val doubleSettingsWidget = new DoubleSettingsWidget( Vec2i(20,140), Config )
  doubleSettingsWidget.visible = false

  publish( WidgetResized(mainWidget) )

  children ++= Seq(
    inventory,
    inventoryButton,
    drawCallLabel,
    playerPositionLabel,
    keySettingWidget,
    keySettingsButton,
    boolSettingsWidget,
    boolSettingsButton,
    doubleSettingsButton,
    doubleSettingsWidget
  )

}
