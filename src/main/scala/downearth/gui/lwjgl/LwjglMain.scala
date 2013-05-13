package downearth.gui.lwjgl

import org.lwjgl.opengl.{DisplayMode, ContextAttribs, Display}
import downearth.gui.{WidgetEventDispatcher, MainWidget}
import downearth.Config

/**
 * User: arne
 * Date: 29.04.13
 * Time: 01:00
 */
class LwjglMain {
  def width = Display.getWidth
  def height = Display.getHeight

  var gameLoop:LwjglGameLoop = null
  def main(args: Array[String]) {

    Display.setDisplayMode( new DisplayMode(Config.windowResolutionWidth,Config.windowResolutionHeight) )
    Display.setResizable(true)
    Display.create()

    gameLoop = new LwjglGameLoop

    val wed = new WidgetEventDispatcher(MainWidget)
    wed.listenTo(gameLoop)

    gameLoop.run()
  }
}
