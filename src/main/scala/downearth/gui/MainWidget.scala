package downearth.gui

import org.lwjgl.opengl.Display
import simplex3d.math.Vec2i
import simplex3d.math.double.{Vec3,Vec4}
import downearth.{Config, ConstructionTool, Player}

import Border._
import Background._

object MainWidget extends FreePanel(Vec2i(0),Vec2i(Display.getWidth,Display.getHeight) ) {

	border = NoBorder
	background = NoBackground

	override def setPosition(newPos:Vec2i, delay:Int) {}

  addReaction {
  case MouseClicked(pos) =>
    Player.primaryAction
  case MouseDrag(mousePos0,mousePos1) =>
    val mouseDelta = mousePos1 - mousePos0
    val delta_angle = Vec3(0)

    delta_angle.y = mouseDelta.x/300.0
    delta_angle.x = mouseDelta.y/300.0

    Player.rotate(delta_angle)
  }

  override def safePosition(newPos:Vec2i) = {
    this.position
  }

  val inventory = new Inventory(Vec2i(20, 200), Vec2i(200,200)) {
    backGroundColor := Vec4(0.1,0.1,0.1,0.7)
    border = LineBorder

    val shovel = new Shovel(position+Vec2i(40, 0))
    children += new Hammer(position+Vec2i(0 , 0))
    children += shovel
    children ++= Range(0,4).map(
      i => new MaterialWidget(i, position + Vec2i(i * 40, 40) )
    )
    children ++= Range(0, ConstructionTool.all.size).map(
      i => new ShapeWidget(i, position + Vec2i(i * 40, 80))
    )

    selected = shovel

    listenTo(MainWidget)

    addReaction {
    case WidgetResized(MainWidget) =>
      val newPos = Vec2i(0)
      newPos.x = MainWidget.size.x - size.x - 20
      newPos.y = 20
      setPosition(newPos,0)
    }

    arrangeChildren()
  }

  val keySettingWidget = new KeySettingsWidget( Vec2i(20,20), Config )

  publish( WidgetResized(this) )

  children += inventory
  children += keySettingWidget
}