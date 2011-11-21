package openworld.gui

import simplex3d.math._
import simplex3d.math.float._
import org.lwjgl.opengl.GL11._

import openworld.Config._
import openworld.Util._
import openworld._

trait Border {
	def draw(position:Vec2i, size:Vec2i)
}

object NoBorder extends Border {
	def draw(position:Vec2i, size:Vec2i) {}
}

class LineBorder(val color:Vec4 = Vec4(1)) extends Border {
	def draw(position:Vec2i, size:Vec2i) {
		glColor4fv(color)
		
		glBegin(GL_LINE_LOOP)
			glVertex2i(position.x-1       , position.y)
			glVertex2i(position.x         , position.y + size.y)
			glVertex2i(position.x + size.x, position.y + size.y)
			glVertex2i(position.x + size.x, position.y)
		glEnd
	}
}

trait Background {
	def draw(position:Vec2i, size:Vec2i)
}

object NoBackground extends Background {
	def draw(position:Vec2i, size:Vec2i) {}
}

class ColorBackground(val color:Vec4 = Vec4(1,1,1,0.25f)) extends Background {
	def draw(position:Vec2i, size:Vec2i) {
		glColor4fv(color)
		
		glBegin(GL_QUADS)
			glVertex2i(position.x         , position.y)
			glVertex2i(position.x         , position.y + size.y)
			glVertex2i(position.x + size.x, position.y + size.y)
			glVertex2i(position.x + size.x, position.y)
		glEnd
	}
}
