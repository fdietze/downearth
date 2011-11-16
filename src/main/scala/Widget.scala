package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import org.lwjgl.opengl.GL11._

import Config._
import Util._


class Widget(val position:Vec2i, val size:Vec2i) {
	var parent:Widget = null
	var border:Border = new NoBorder
	var background:Background = new NoBackground
	var mousePressed = false
	
	val dragStartPos = Vec2i(0)
	var dragging = false
	
	def dragStart {	}
	
	def dragStop(mousePos:Vec2i) {
		
	}
	
	def getAbsolutePosition:Vec2i = {
		if( parent == null )
			position
		else
			position + parent.getAbsolutePosition
	}
	
	def clickDelta = 2f
	
	def setSafePosition(pos:Vec2i) {
		val newpos = 
			if( parent != null )
				min(max(Vec2i(0), pos), parent.size - size)
			else
				pos
				
		position := newpos
	}
	
	def invokeDraw(offset:Vec2i = Vec2i(0)) {
		background.draw(offset + position, size)
		draw(offset)
		border.draw(offset + position, size)
	}

	def invokeMouseDown(mousePos:Vec2i) {
		mousePressed = true
		dragStartPos := mousePos
		mouseDown(mousePos)
	}

	def invokeMouseUp(mousePos:Vec2i) {
		mousePressed = false
		mouseUp(mousePos)
		val moved = dragStartPos - mousePos
		if( dragging ) {
			dragging = false
			dragStop(mousePos)
			if(length(moved) <= clickDelta )
				mouseClicked(mousePos)
		}
	}
	
	def invokeMouseMoved(mousePos0:Vec2i, mousePos1:Vec2i) {
		mouseMoved(mousePos0, mousePos1)
		
		if( !indexInRange(mousePos0, position, size)
		 &&  indexInRange(mousePos1, position, size) )
		 	mouseIn(mousePos0, mousePos1)
		else { // if mouse is not moved from out to in, but moved
			if( mousePressed ) {
				if(!dragging) {
					dragStart
					dragging = true
				}
				mouseDragged(mousePos0:Vec2i, mousePos1:Vec2i)
			}
		}
		if(  indexInRange(mousePos0, position, size)
		 && !indexInRange(mousePos1, position, size) )
		 	mouseOut(mousePos0, mousePos1)
	}
	
	def draw(offset:Vec2i = Vec2i(0)) {}

	def mouseClicked(mousePos:Vec2i) {
		DisplayEventManager.showEventText("mouseClicked: " + this)
	}

	def mouseDown(mousePos:Vec2i) {
		DisplayEventManager.showEventText("mouseDown: " + mousePos)
	}
	
	def mouseUp(mousePos:Vec2i) {
		DisplayEventManager.showEventText("mouseUp: " + mousePos)
	}

	def mouseMoved(mousePos0:Vec2i, mousePos1:Vec2i) {
		//DisplayEventManager.showEventText("mouseMoved: " + mousePos0 + ", " + mousePos1)
	}

	def mouseIn(mousePos0:Vec2i, mousePos1:Vec2i) {
		//DisplayEventManager.showEventText("mouseIn: " + mousePos0 + ", " + mousePos1)
	}
	
	def mouseOut(mousePos0:Vec2i, mousePos1:Vec2i) {
		//DisplayEventManager.showEventText("mouseOut: " + mousePos0 + ", " + mousePos1)
	}
	
	def mouseDragged(mousePos0:Vec2i,mousePos1:Vec2i) {
		DisplayEventManager.showEventText("mouseDragged: " + mousePos0 + ", " + mousePos1)
	}
	
	override def toString = "Widget(%s, %s)" format( position, size )
}

class Panel(position:Vec2i, size:Vec2i) extends Widget(position, size) {
	private def thispanel = this
	val children = new collection.mutable.Buffer[Widget] {
		val buffer = new collection.mutable.ArrayBuffer[Widget]
		// trait Buffer implementieren, um automatisch die Parents eines
		// hinzugefügten Widgets zu setzen
		
		def +=(child:Widget) = {
			child.parent = thispanel
			buffer += child
			this
		}

		def +=:(child:Widget) = {
			child.parent = thispanel
			child +=: buffer
			this
		}
		
		def remove(n: Int) = {
			buffer(n).parent = null
			buffer.remove(n)
		}
		
		def insertAll(n: Int, elems: Traversable[Widget]) {
			for(c <- elems)
				c.parent = thispanel
			buffer.insertAll(n,elems)
		}
		
		def clear {
			for(c <- buffer)
				c.parent = null
			buffer.clear
		}
		
		def length = buffer.length
		
		def update (n: Int, newelem: Widget) {
			buffer(n).parent = null
			newelem.parent = thispanel
			buffer(n) = newelem 
		}
		
		def apply (n: Int) = buffer(n)
		def iterator = buffer.iterator
	}
	
	override def toString = "Panel(%s, %s)" format( position, size )
}

class FreePanel(position:Vec2i, size:Vec2i) extends Panel(position,size) {
	var pressedWidget:Widget = this
	
	override def invokeDraw(offset:Vec2i = Vec2i(0)) {
		super.invokeDraw(offset)
		for( c <- children )
			c.invokeDraw(position + offset)
	}
	
	override def invokeMouseDown(mousePos:Vec2i) {
		children.find( child => indexInRange(mousePos, position + child.position, child.size) ) match {
		case Some(child) =>
			pressedWidget = child
			child.invokeMouseDown(mousePos - position)
		case None =>
			pressedWidget = this
			super.invokeMouseDown(mousePos)
		}
	}

	override def invokeMouseUp(mousePos:Vec2i) {
		if (pressedWidget != this){
			pressedWidget.invokeMouseUp(mousePos - position)
		} else {
			super.invokeMouseUp(mousePos)
		}
	}

	override def invokeMouseMoved(mousePos0:Vec2i, mousePos1:Vec2i) {
		super.invokeMouseMoved(mousePos0, mousePos1)
		for(child <- children)
			if( indexInRange(mousePos0, position + child.position, child.size)  
			 || indexInRange(mousePos1, position + child.position, child.size)
 			 || child.mousePressed )
				child.invokeMouseMoved(mousePos0 - position, mousePos1 - position)
	}
}


trait Dragable extends Widget {
	// Drag-Start-Widget-Position
	val dswp = Vec2i(0)
	
	override def dragStart {
		dswp := position
	}
	
	override def mouseDragged(mousePos0:Vec2i, mousePos1:Vec2i) {
		setSafePosition(dswp - dragStartPos + mousePos1)
	}
}

abstract class Border {
	def draw(position:Vec2i, size:Vec2i)
}

class NoBorder extends Border {
	def draw(position:Vec2i, size:Vec2i) {}
}

class LineBorder(color:Vec4 = Vec4(1)) extends Border {
	def draw(position:Vec2i, size:Vec2i) {
		glColor4fv(color)
		
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
		glColor4fv(color)

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
