package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import org.lwjgl.opengl.GL11._
import org.newdawn.slick.opengl.Texture

import Config._
import Util._

object MainWidget extends FreePanel(Vec2i(0),Vec2i(screenWidth,screenHeight)) {
	border = NoBorder
	background = NoBackground
	override def toString = "MainWidget"
	override def setPosition(newPos:Vec2i) {}
}

class Widget( val position:Vec2i, val size:Vec2i) {
	
	def setPosition(newPos:Vec2i) {
		position := min( max(parent.position, newPos), parent.position + parent.size - size)
	}
	
	var parent:Panel = MainWidget
	var border:Border = new LineBorder()
	var background:Background = new ColorBackground()
	var mousePressed = false
	
	val dragStartPos = Vec2i(0)
	var dragging = false
	
	def dragStart {}
	
	def dragStop(mousePos:Vec2i) {}
	
	def clickDelta = 2f
	
	def invokeDraw {
		background.draw(position, size)
		draw
		border.draw(position, size)
	}

	def invokeMouseDown(mousePos:Vec2i) {
		mousePressed = true
		dragStartPos := mousePos
		mouseDown(mousePos)
	}

	def invokeMouseUp(mousePos:Vec2i) {
		if( mousePressed && !indexInRange(mousePos, position, size) )
			mouseOut(dragStartPos, mousePos)
		mousePressed = false
		mouseUp(mousePos)
		
		if( dragging ) {
			dragging = false
			dragStop(mousePos)
		} else {
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
					if( length(mousePos1 - dragStartPos) >= clickDelta ) {
						dragging = true
						dragStart
						mouseDragged(dragStartPos, mousePos1)
					}
				}
				else
					mouseDragged(mousePos0:Vec2i, mousePos1:Vec2i)
			}
		}
		
		if(  indexInRange(mousePos0, position, size)
		 && !indexInRange(mousePos1, position, size) )
		 	if( !mousePressed )
		 		mouseOut(mousePos0, mousePos1)
	}
	
	def draw {}

	def mouseClicked(mousePos:Vec2i) {}

	def mouseDown(mousePos:Vec2i) {}
	
	def mouseUp(mousePos:Vec2i) {}

	def mouseMoved(mousePos0:Vec2i, mousePos1:Vec2i) {}

	def mouseIn(mousePos0:Vec2i, mousePos1:Vec2i) {}
	
	def mouseOut(mousePos0:Vec2i, mousePos1:Vec2i) {}
	
	def mouseDragged(mousePos0:Vec2i,mousePos1:Vec2i) {}
	
	override def toString = "Widget(%s, %s)" format( position, size )
}

class Label(_pos:Vec2i,_text:String) extends Widget(_pos, Vec2i( ConsoleFont.font.getWidth(_text), ConsoleFont.height ) ) {
	private var m_text = _text
	def text = m_text
	def	text_=(s:String){
		m_text = s
	
	}
	override def draw {
		import org.newdawn.slick.Color.white
		ConsoleFont.font.drawString( position.x, position.y, text, white )
	}
}

class TextureWidget(_position:Vec2i, _size:Vec2i, texture:Texture, texPosition:Vec2, texSize:Vec2) extends Widget(_position, _size) {
	override def draw {
		glColor4f(1,1,1,1)
		
		texture.bind
		glEnable(GL_TEXTURE_2D)
		glBegin(GL_QUADS)
		
		glTexCoord2f(texPosition.x, texPosition.y)
		glVertex2i(position.x         , position.y          )
		glTexCoord2f(texPosition.x, texPosition.y + texSize.y)
		glVertex2i(position.x         , position.y + size.y )
		glTexCoord2f(texPosition.x + texSize.x, texPosition.y + texSize.y)
		glVertex2i(position.x + size.x, position.y + size.y )
		glTexCoord2f(texPosition.x + texSize.x, texPosition.y)
		glVertex2i(position.x + size.x, position.y          )
		
		glEnd
		glDisable(GL_TEXTURE_2D)
	}
}


abstract class Panel(_position:Vec2i, _size:Vec2i) extends Widget(_position, _size) {
	private def thispanel = this
	
	override def setPosition(newPos:Vec2i) {
		val oldPos = position.clone
		super.setPosition(newPos)
		val delta = position - oldPos
		for( child <- children )
			child.setPosition( child.position + delta )
	}
	
	def arrangeChildren {}

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

class FreePanel(_position:Vec2i, _size:Vec2i) extends Panel(_position,_size) {
	var pressedWidget:Widget = this
	
	override def invokeDraw {
		background.draw(position, size)
		draw
		for( child <- children )
			child.invokeDraw
		border.draw(position, size)
	}
	
	override def invokeMouseDown(mousePos:Vec2i) {
		children.find(
			child => indexInRange(mousePos, child.position, child.size)
		) match {
		case Some(child) =>
			pressedWidget = child
			child.invokeMouseDown(mousePos)
		case None =>
			pressedWidget = this
			super.invokeMouseDown(mousePos)
		}
	}
	
	override def invokeMouseUp(mousePos:Vec2i) {
		if (pressedWidget != this){
			pressedWidget.invokeMouseUp(mousePos)
		} else {
			super.invokeMouseUp(mousePos)
		}
	}

	override def invokeMouseMoved(mousePos0:Vec2i, mousePos1:Vec2i) {
		super.invokeMouseMoved(mousePos0, mousePos1)
		for(child <- children)
			if( indexInRange(mousePos0, child.position, child.size)  
			 || indexInRange(mousePos1, child.position, child.size)
 			 || (child eq pressedWidget) )
				child.invokeMouseMoved(mousePos0, mousePos1)
	}
}

class AutoPanel(position:Vec2i, size:Vec2i, space:Int = 5) extends FreePanel(position, size) {
	override def arrangeChildren {
		var x = space
		var y = space
		var maxHeight = 0
		for( child <- children ) {
			if( x + child.size.x + space > size.x ) {
				x = space
				y += maxHeight + space
				maxHeight = 0
			}
			
			child.setPosition( position + Vec2i(x,y) )
			maxHeight = max(maxHeight, child.size.y)
			x += child.size.x + space
		}
	}
}


trait Dragable extends Widget {
	// Drag-Start-Widget-Position
	val dragOriginalPosition = Vec2i(0)
	
	override def dragStart {
		dragOriginalPosition := position
	}
	
	override def mouseDragged(mousePos0:Vec2i, mousePos1:Vec2i) {
		super.mouseDragged(mousePos0, mousePos1)
		setPosition( dragOriginalPosition - dragStartPos + mousePos1 )
	}
}

