package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import org.lwjgl.opengl.GL11._

import Config._
import Util._


class Widget(val position:Vec2i, val size:Vec2i) {
	var border:Border = new NoBorder
	var background:Background = new NoBackground

	def invokeDraw(offset:Vec2i = Vec2i(0)) {
		background.draw(offset + position, size)
		draw(offset)
		border.draw(offset + position, size)
	}

	def invokeMouseClicked(mousePos:Vec2i) {
		mouseClicked(mousePos)
	}
	
	def invokeMouseMoved(mousePos0:Vec2i, mousePos1:Vec2i) {
		mouseMoved(mousePos0, mousePos1)

		if( !indexInRange(mousePos0, position, size)
		 &&  indexInRange(mousePos1, position, size) )
		 	invokeMouseIn(mousePos0, mousePos1)

		if(  indexInRange(mousePos0, position, size)
		 && !indexInRange(mousePos1, position, size) )
		 	invokeMouseOut(mousePos0, mousePos1)
	}

	def invokeMouseIn(mousePos0:Vec2i, mousePos1:Vec2i) {
		mouseIn(mousePos0, mousePos1)
	}
	
	def invokeMouseOut(mousePos0:Vec2i, mousePos1:Vec2i) {
		mouseOut(mousePos0, mousePos1)
	}
	

	def draw(offset:Vec2i = Vec2i(0)) { }

	def mouseClicked(mousePos:Vec2i) {
		DisplayEventManager.showEventText("mouseClicked: " + this)
	}

	def mouseMoved(mousePos0:Vec2i, mousePos1:Vec2i) {
		//DisplayEventManager.showEventText("mouseMoved: " + mousePos0 + ", " + mousePos1)
	}

	def mouseIn(mousePos0:Vec2i, mousePos1:Vec2i) {
		DisplayEventManager.showEventText("mouseIn: " + mousePos0 + ", " + mousePos1)
	}
	
	def mouseOut(mousePos0:Vec2i, mousePos1:Vec2i) {
		DisplayEventManager.showEventText("mouseOut: " + mousePos0 + ", " + mousePos1)
	}
	
	//def mouseDragged(mousePos0:Vec2i,mousePos1:Vec2i) {}
	
	override def toString = "Widget(%s, %s)" format( position, size )
}

class Panel(position:Vec2i, size:Vec2i) extends Widget(position, size) {
	val children = new collection.mutable.ArrayBuffer[Widget]
	override def toString = "Panel(%s, %s)" format( position, size )
}

class FreePanel(position:Vec2i, size:Vec2i) extends Panel(position,size) {
	override def invokeDraw(offset:Vec2i = Vec2i(0)) {
		super.invokeDraw(offset)
		children.foreach( _.invokeDraw(position + offset) )
	}
	
	override def invokeMouseClicked(mousePos:Vec2i) {
		super.invokeMouseClicked(mousePos)
		for(child <- children)
			if( indexInRange(mousePos, position + child.position, child.size) )
				child.invokeMouseClicked(mousePos - position)
	}

	override def invokeMouseMoved(mousePos0:Vec2i, mousePos1:Vec2i) {
		super.invokeMouseMoved(mousePos0, mousePos1)
		for(child <- children)
			if( indexInRange(mousePos0, position + child.position, child.size)  
			 || indexInRange(mousePos1, position + child.position, child.size) )
				child.invokeMouseMoved(mousePos0 - position, mousePos1 - position)
	}
}

	
/* 
	
	def invokeMouseDragged(pMousePos:Vec2i,mousePos:Vec2i) {
		if( indexInRange(pMousePos,position,size)
		 && indexInRange(mousePos,position,size) ) {
			mouseDragged(pMousePos,mousePos)
			for(c <- children){
				c.invokeMouseDragged(pMousePos,mousePos)
			}
		}
	}
	
}

trait Dragable extends Widget {
	override def mouseDragged(pMousePos:Vec2i,mousePos:Vec2i) {
		position += mousePos
		position -= pMousePos
	}
}*/





abstract class Border {
	def draw(position:Vec2i, size:Vec2i)
}

class NoBorder extends Border {
	def draw(position:Vec2i, size:Vec2i) {}
}

class LineBorder(color:Vec4 = Vec4(1)) extends Border {
	def draw(position:Vec2i, size:Vec2i) {
		glColor4v(color)
		
		glEnable(GL_BLEND)
			glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
			glBegin(GL_LINE_LOOP)
				glVertex2i(position.x-1       , position.y)
				glVertex2i(position.x         , position.y + size.y)
				glVertex2i(position.x + size.x, position.y + size.y)
				glVertex2i(position.x + size.x, position.y)
			glEnd
		glDisable(GL_BLEND)
	}
}

abstract class Background {
	def draw(position:Vec2i, size:Vec2i)
}

class NoBackground extends Background {
	def draw(position:Vec2i, size:Vec2i) {}
}

class ColorBackground(color:Vec4 = Vec4(1,1,1,0.25f)) extends Background {
	def draw(position:Vec2i, size:Vec2i) {
		glColor4v(color)

		glEnable(GL_BLEND)
			glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
			glBegin(GL_QUADS)
				glVertex2i(position.x         , position.y)
				glVertex2i(position.x         , position.y + size.y)
				glVertex2i(position.x + size.x, position.y + size.y)
				glVertex2i(position.x + size.x, position.y)
			glEnd
		glDisable(GL_BLEND)
	}
}
