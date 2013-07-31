package downearth.gui

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import downearth.util._
import downearth.rendering.{ConsoleFont}
import downearth.gui.Border._
import downearth.gui.Background._
import System.{currentTimeMillis => time}
import org.lwjgl.input.Keyboard
import glwrapper.Texture2D

abstract class Widget extends Listener with Publisher {
  val position:Vec2i
  val size:Vec2i

  var parent:Panel = null
  var border:Border = LineBorder
  val lineBorderColor = Vec4(0)
  var background = ColorBackGround
  val backGroundColor = Vec4(1,1,1,0.25)

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

  // TODO take the animation to it's own class
	def invokeAnimation() {
		if( animationEndTime <= time )
			position := animationEndPosition
		else {
			val progress = (time - animationStartTime).toDouble / (animationEndTime - animationStartTime)
			position := Vec2i(lerpVec2(Vec2(animationStartPosition), Vec2(animationEndPosition), progress))
		}
	}

	override def toString = getClass.getName.split('.').last //"%s(%s, %s)" format( getClass.getName, position, size )
}

class Label(val position:Vec2i, _text:String) extends Widget {
  private var m_text = _text
  backGroundColor.a = 0
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

class Button(val position:Vec2i, val text:String) extends Widget {
  val size = Vec2i(ConsoleFont.font.getWidth(text), ConsoleFont.height)
  border = LineBorder
  lineBorderColor := Vec4(1)

  def onClick() {}

  addReaction {
    case MouseIn =>
      backGroundColor.a = 0.5
    case MouseOut =>
      backGroundColor.a = 0.25
    case MouseClicked(pos) =>
      onClick()
      publish( ButtonClicked(this) )
  }
}

class Slider(val position:Vec2i) extends Panel { slider =>
  val size = Vec2i( 256, 16 )

  border = LineBorder
  lineBorderColor := Vec4(1)

  def value = slideable.value

  val slideable = new Widget {
    val size     = Vec2i( 20, 12 )
    val position = slider.position + slider.size / 2 - size / 2
    val minX     = slider.position.x + 2
    val maxX     = slider.position.x + slider.size.x - size.x - 2
    val width = maxX-minX

    border = LineBorder
    lineBorderColor := Vec4(1)

    def value = (position.x - minX) / width.toDouble

    addReaction {
      case MouseIn => backGroundColor := Vec4(1,1,1,0.5)
      case MouseOut => backGroundColor := Vec4(1,1,1,0.25)
      case MouseDrag(oldPos, newPos) =>
        val dist = newPos.x - oldPos.x
        val oldX = position.x

        position.x += dist
        if(position.x > maxX) position.x = maxX
        if(position.x < minX) position.x = minX

        if( oldX != position.x ){
          setPosition(position,0)
          publish( SliderChanged(slider) )
        }
    }
  }

  children += slideable
}

abstract class TextureWidget(val texture:Texture2D, val texPosition:Vec2, val texSize:Vec2) extends Widget {}

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

class StackPanel(val position:Vec2i)(children:Widget*) extends Panel {
  val size = ( Vec2i(0,0) /: children ) {
    case (v,child) =>
      v.x = max(child.size.x,v.x)
      v.y += child.size.y
      v
  }

  arrangeChildren()

  override def arrangeChildren(delay:Int = 0) {
    var y = position.y

    for( child <- children ) {
      child.setPosition( position + Vec2i(0,y), delay )
      y += child.size.y
    }
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
  val size = {
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
    Vec2i(maxWidth, y-position.y)
  }
}

class BoolSettingsWidget(val position:Vec2i, config:AnyRef) extends Panel {

  val size = {
    var y = position.y
    var maxWidth = 0
    for( field <- config.getClass.getDeclaredFields ) {
      field.setAccessible(true)
      if( field.get(config).isInstanceOf[Boolean] && !field.getName.startsWith("bitmap$") ) {
        val value = field.getBoolean(config)
        val child = new Button( Vec2i(position.x,y), field.getName )
        if( value )
          child.backGroundColor := Vec4(0,1,0,0.25)
        else
          child.backGroundColor := Vec4(1,0,0,0.25)

        child.addReaction {
          case MouseClicked(pos) =>

          if( field.getBoolean(config) ){
            field.setBoolean(config,false)
            child.backGroundColor := Vec4(1,0,0,0.25)
          }
          else {
            field.setBoolean(config,true)
            child.backGroundColor := Vec4(0,1,0,0.25)
          }
        }

        y += child.size.y
        maxWidth = maxWidth max child.size.x
        children += child
      }
    }
    Vec2i(maxWidth, y-position.y)
  }
}

class DoubleSettingsWidget(val position:Vec2i, config:AnyRef) extends Panel { widget =>

  val size = {
    var y = position.y
    var maxWidth = 0
    for( field <- config.getClass.getDeclaredFields ) {
      field.setAccessible(true)
      if( field.get(config).isInstanceOf[Double] ) {
        val initialValue = field.getDouble(config)
        val slider = new Slider( Vec2i(position.x,y) )
        slider.listenTo(slider.slideable)

        val name = new Label( Vec2i(position.x, y), field.getName )
        val valueLabel = new Label( Vec2i(position.x+slider.size.x/2, y), initialValue.toString )

        slider.addReaction {
          case SliderChanged(slider) =>
            val newValue = pow(10, slider.value * 2 - 1) * initialValue
            val oldValue = field.getDouble(config)
            field.setDouble(config, newValue)
            valueLabel.text = "%4.2f" format newValue
        }

        y += slider.size.y
        maxWidth = maxWidth max slider.size.x
        children ++= Seq( slider, name, valueLabel )
      }
    }
    Vec2i(maxWidth, y-position.y)
  }
}
