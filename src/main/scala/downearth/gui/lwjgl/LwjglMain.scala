package downearth.gui.lwjgl

import org.lwjgl.opengl.{DisplayMode, ContextAttribs, Display}

/**
 * User: arne
 * Date: 29.04.13
 * Time: 01:00
 */
class LwjglMain {
  def width = Display.getWidth
  def height = Display.getHeight

  def main(args: Array[String]) {

    Display.setDisplayMode( new DisplayMode(640,480) )
    Display.setResizable(true)
    Display.create()

    val gameLoop = new LwjglGameLoop
    gameLoop.run()
  }
}
