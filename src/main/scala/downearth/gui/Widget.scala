package downearth.gui

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import downearth.util._
import downearth.rendering.{ConsoleFont, Texture}
import downearth.gui.Border._
import downearth.gui.Background._
import System.{currentTimeMillis => time}
import org.lwjgl.input.Keyboard

abstract class Widget extends Listener with Publisher {
  val position:Vec2i
  val size:Vec2i

  var animationStartTime:Long = 0
	var animationEndTime:Long = 0
	lazy val animationStartPosition = position.clone
	lazy val animationEndPosition = position.clone
  var mouseOver = false
  var visible = true

  addReaction {
  case MouseIn =>
    mouseOver = true
  case MouseOut =>
    mouseOver = false
  }

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
    if( size != newSize ) {
      size := newSize
      position := safePosition(position)
      publish( WidgetResized( this ) )
    }
	}
	
	var parent:Panel = MainWidget
	var border:Border = LineBorder
  val lineBorderColor = Vec4(0)
  var background = ColorBackGround
  val backGroundColor = Vec4(1,1,1,0.25)

  // TODO take the animation to it's own class
	def invokeAnimation {
		if( animationEndTime <= time )
			position := animationEndPosition
		else {
			val progress = (time - animationStartTime).toDouble / (animationEndTime - animationStartTime)
			position := Vec2i(lerpVec2i(animationStartPosition, animationEndPosition, progress))
		}
	}

	override def toString = getClass.getName.split('.').last //"%s(%s, %s)" format( getClass.getName, position, size )
}

class Label(val position:Vec2i, _text:String) extends Widget {
  private var m_text = _text
  val size = Vec2i(ConsoleFont.font.getWidth(m_text), ConsoleFont.height)

	def updateSize() {
    val newSize = Vec2i(ConsoleFont.font.getWidth(m_text), ConsoleFont.height)
		resize(newSize)
	}

	def text = m_text
	def	text_=(s:Any) {
		m_text = s.toString
		updateSize()
	}

  override def toString = s"Label($m_text)"
}

abstract class TextureWidget(val texture:Texture, val texPosition:Vec2, val texSize:Vec2) extends Widget {}

// a panel a gui element that has children
abstract class Panel extends Widget { thispanel =>
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

	val children = new collection.mutable.ArrayBuffer[Widget] with WidgetBuffer {
    def parent = thispanel
  }
}

// this panel clamps the position of the child elements to the border of the panel
class ClampPositionPanel(val position:Vec2i, val size:Vec2i, space:Int = 5) extends Panel {
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

class GridPanel(val position:Vec2i, val size:Vec2i, val cellsize:Int = 30) extends Panel {
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
  val dragStartPos = Vec2i(0)

  addReaction {
  case DragStart(pos) =>
    dragOriginalPosition := position
    dragStartPos := pos
    println("DragStart"+this)
  case MouseDrag(pos1, pos2) =>
    setPosition( dragOriginalPosition + (pos2 - dragStartPos) )
  }
}

class KeySettingsWidget(val position:Vec2i, config:AnyRef) extends Panel {
  val fields = config.getClass.getDeclaredFields.filter( _.getName.startsWith("key") )

  var y = position.y
  var maxWidth = 0
  for( field <- fields ) {
    field.setAccessible( true )
    val key = field.getInt( config )
    val keyName = Keyboard.getKeyName(key)
    val child = new Label( Vec2i(position.x,y), field.getName + " " + keyName )
    child.addReaction {
    case KeyPress(key) =>
      println("got key press " + Keyboard.getKeyName(key))
      child.text = field.getName + " " + Keyboard.getKeyName(key)
      field.setInt(config, key)
    }

    y += child.size.y
    maxWidth = maxWidth max child.size.x
    children += child
  }

  val size = Vec2i(maxWidth, y-position.y)
}