package downearth.gui

import simplex3d.math._
import simplex3d.math.double._
import org.lwjgl.opengl.GL11._

import downearth.util._

object Border extends Enumeration {
  type Border = Value
  val NoBorder, LineBorder = Value
}

object Background extends Enumeration {
  type Background = Value
  val NoBackground, ColorBackGround = Value
}

