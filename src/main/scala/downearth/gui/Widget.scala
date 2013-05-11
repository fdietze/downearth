package downearth.gui

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import org.lwjgl.opengl.GL11._

import downearth.Config._
import downearth.util._
import downearth.{ConstructionTool, Player, Main}
import downearth.rendering.{Draw, ConsoleFont, Texture}
import org.lwjgl.opengl.Display
import downearth.gui.Border._
import downearth.gui.Background._

object MainWidget extends FreePanel(Vec2i(0),Vec2i(Display.getWidth,Display.getHeight) ) {

	border = NoBorder
	background = NoBackground
	mouseOver = true

	override def setPosition(newPos:Vec2i, delay:Int) {}
	override def mouseClicked(pos:Vec2i) = Player.primaryAction

	override def mouseDragged(mousePos0:Vec2i, mousePos1:Vec2i) {
		val mouseDelta = mousePos1 - mousePos0
		val delta_angle = Vec3(0)
		
		delta_angle.y = mouseDelta.x/300.0
		delta_angle.x = mouseDelta.y/300.0
		
		Player.rotate(delta_angle)
	}

	override def resize(newSize:Vec2i) {
		size := newSize
	}

  val inventory = new Inventory(Vec2i(20, 200), Vec2i(200,200)) {
    backGroundColor := Vec4(0.1,0.1,0.1,0.7)
    border = LineBorder

    children += new Hammer(position+Vec2i(0 , 0))
    children += new Shovel(position+Vec2i(40, 0))
    children ++= Range(0,4).map(
      i => new MaterialWidget(i, position + Vec2i(i * 40, 40) )
    )

    children ++= Range(0, ConstructionTool.all.size).map(
      i => new ShapeWidget(i, position + Vec2i(i * 40, 80))
    )

    arrangeChildren()
    setTopRight

    var moved = false
    def setTopRight = setPosition(Vec2i(Main.width.toInt - size.x - 20, 20))
    override def dragStop(mousePos:Vec2i) { moved = true }
  }

  children += inventory
}
/*
def position:Vec2i
def position_=(newPos:Vec2i)
def size:Vec2i
def size_=(newSize:Vec2i)

def parent:Panel
def parent_=(newParent)

var border:Border = new LineBorder()
var background:Background = new ColorBackground()
*/

class Widget( val position:Vec2i, val size:Vec2i) {
	
	def time = System.currentTimeMillis
	var animationStartTime:Long = 0
	var animationEndTime:Long = 0
	val animationStartPosition = position.clone
	val animationEndPosition = position.clone
	
	def safePosition(newPos:Vec2i) = {
		min( max(parent.position, newPos), parent.position + parent.size - size)
	}
	
	def setPosition(newPos:Vec2i, delay:Int = 0) {
		val newSafePos = safePosition(newPos)
		animationEndPosition := newSafePos
		if( delay <= 0 ) {
			position := newSafePos
			animationEndTime = time
		}
		else {
			animationStartTime = time
			animationEndTime = time + delay
			animationStartPosition := position
		}
	}

	def resize(newSize:Vec2i) {
		size := newSize
		position := safePosition(position)
	}
	
	var parent:Panel = MainWidget
	var border:Border = LineBorder
  val lineBorderColor = Vec4(0)
  val backGroundColor = Vec4(1,1,1,0.25)
	var background:Background = ColorBackGround
	var mousePressed = false
	var mouseOver = false
	
	val dragStartPos = Vec2i(0)
	var dragging = false
	
	def clickDelta = 5.0
	
	def invokeAnimation {
		if( animationEndTime <= time )
			position := animationEndPosition
		else {
			val progress = (time - animationStartTime).toDouble / (animationEndTime - animationStartTime)
			position := Vec2i(lerpVec2i(animationStartPosition, animationEndPosition, progress))
		}
	}

	def invokeMouseDown(mousePos:Vec2i) {
		mousePressed = true
		dragStartPos := mousePos
		mouseDown(mousePos)
	}

	def invokeMouseUp(mousePos:Vec2i) {
		if( mousePressed && !indexInRange(mousePos, position, size) )
			invokeMouseOut(dragStartPos, mousePos)

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
		if( !indexInRange(mousePos0, position, size)
		 &&  indexInRange(mousePos1, position, size) ) {
			if( mouseOver != true ) {
				invokeMouseIn(mousePos0, mousePos1)
				if( parent != null )
					parent.invokeMouseOut(mousePos0, mousePos1)
			}
		}
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
			if( !mousePressed ) {
				invokeMouseOut(mousePos0, mousePos1)
				if( parent != null )
					parent.invokeMouseIn(mousePos0, mousePos1)
			}

		mouseMoved(mousePos0, mousePos1)
	}
	
	def invokeMouseIn(mousePos0:Vec2i, mousePos1:Vec2i) {
		mouseOver = true
		mouseIn(mousePos0, mousePos1)
	}

	def invokeMouseOut(mousePos0:Vec2i, mousePos1:Vec2i) {
		mouseOver = false
		mouseOut(mousePos0, mousePos1)
	}
	
	// Methods to be overridden
	def mouseClicked(mousePos:Vec2i) {}
	def mouseDown(mousePos:Vec2i) {}
	def mouseUp(mousePos:Vec2i) {}
	def mouseMoved(mousePos0:Vec2i, mousePos1:Vec2i) {}
	def mouseIn(mousePos0:Vec2i, mousePos1:Vec2i) {}
	def mouseOut(mousePos0:Vec2i, mousePos1:Vec2i) {}
	def mouseDragged(mousePos0:Vec2i,mousePos1:Vec2i) {}
	def dragStart {}
	def dragStop(mousePos:Vec2i) {}
	
	
	override def toString = getClass.getName.split('.').last //"%s(%s, %s)" format( getClass.getName, position, size )
}

class Label(_pos:Vec2i,_text:String) extends Widget(_pos, Vec2i(0)) {
	def updateSize {
		size.x = ConsoleFont.font.getWidth(m_text)
		size.y = ConsoleFont.height
	}

	private var m_text = _text
	updateSize

	def text = m_text
	def	text_=(s:Any) {
		m_text = s.toString
		updateSize
	}
}

class TextureWidget(_position:Vec2i, _size:Vec2i, val texture:Texture, val texPosition:Vec2, val texSize:Vec2) extends Widget(_position, _size) {}

// TODO: Typparameter übergeben
abstract class Panel(_position:Vec2i, _size:Vec2i) extends Widget(_position, _size) {
	private def thispanel = this
	
	override def setPosition(newPos:Vec2i, delay:Int) {
		val oldPos = position.clone
		super.setPosition(newPos)
		val delta = position - oldPos
		for( child <- children ) {
			// We can be sure that (child.position + delta) is still safe
			child.position += delta
			child.animationStartPosition += delta
			child.animationEndPosition += delta
		}
	}
	
	def arrangeChildren(delay:Int = 0) {}

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
}

// TODO nichts sagender Name
class FreePanel(_position:Vec2i, _size:Vec2i) extends Panel(_position,_size) {
	var pressedWidget:Widget = this
	
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
			invokeMouseMoved(pressedWidget.dragStartPos, mousePos)
			pressedWidget = this
		} else {
			super.invokeMouseUp(mousePos)
		}
	}

	override def invokeMouseMoved(mousePos0:Vec2i, mousePos1:Vec2i) {
		super.invokeMouseMoved(mousePos0, mousePos1)
		if( pressedWidget == this ) {
			for(child <- children)
				if( indexInRange(mousePos0, child.position, child.size)  
				 || indexInRange(mousePos1, child.position, child.size) )
					child.invokeMouseMoved(mousePos0, mousePos1)
		}
		else
			pressedWidget.invokeMouseMoved(mousePos0, mousePos1)
	}
}

// TODO nichts sagender Name
class AutoPanel(position:Vec2i, size:Vec2i, space:Int = 5) extends FreePanel(position, size) {
	override def arrangeChildren(delay:Int = 0) {
		var x = space
		var y = space
		var maxHeight = 0
		for( child <- children ) {
			if( x + child.size.x + space > size.x ) {
				x = space
				y += maxHeight + space
				maxHeight = 0
			}
			
			child.setPosition( position + Vec2i(x,y), delay )
			maxHeight = max(maxHeight, child.size.y)
			x += child.size.x + space
		}
	}
}


class GridPanel(position:Vec2i, size:Vec2i, val cellsize:Int = 30) extends FreePanel(position, size) {
	override def arrangeChildren(delay:Int = 0) {
		val raster = new collection.mutable.HashMap[Vec2i,Widget]
		for( child <- children ) {
			val childRelCenter = -position + child.position + child.size / 2
			val closestCell = Vec2i(round((childRelCenter - cellsize / 2) / cellsize.toDouble))
			
			// filter out all already used cells and select the closest one
			val newCell = (((Vec2i(0) until size/cellsize) map ( x => Vec2i(x) )).toSet -- raster.keys).minBy( p => length( closestCell - p ) )

			raster(newCell) = child
			child.setPosition( position + newCell * cellsize - child.size / 2 + cellsize / 2, delay )
		}
	}
}


trait Draggable extends Widget {
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
