package openworld

import simplex3d.math._
import simplex3d.math.float._
import simplex3d.math.float.functions._

import org.lwjgl.opengl.GL11._

import Config._
import Util._


class Widget( _position:Vec2i, val size:Vec2i) {
	def position = _position
	def position_=(newpos:Vec2i) {
		if(parent != null)
			_position := min( max(parent.position, newpos), parent.position + parent.size - size)
		else
			_position := newpos
	}
	
	var parent:Widget = null
	var border:Border = new NoBorder
	var background:Background = new NoBackground
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
					dragging = true
					dragStart
				}
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


abstract class Panel(_position:Vec2i, _size:Vec2i) extends Widget(_position, _size) {
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
	
	// def getChildPosition(child:Int) = children(child).position TODO wofür wenn man auch direkt auf children zugreifen kann ?
	
	override def position_=(pos:Vec2i){
		val prepos = position.clone
		super.position_=(pos)
		for(child <- children){
			child.position = child.position + position - prepos
		}
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

class AutoPanel(position:Vec2i, size:Vec2i) extends FreePanel(position, size) {
	override def invokeDraw {
		background.draw(position, size)
		draw
		for( child <- children )
			child.invokeDraw
		border.draw(position, size)
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
		// TODO hier könnte sich was geändert haben
		position = dragOriginalPosition - dragStartPos + mousePos1
	}
}

