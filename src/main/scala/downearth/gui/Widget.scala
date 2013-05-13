package downearth.gui

import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import downearth.util._
import downearth.{ConstructionTool, Player, Main}
import downearth.rendering.{ConsoleFont, Texture}
import org.lwjgl.opengl.Display
import downearth.gui.Border._
import downearth.gui.Background._
import System.{currentTimeMillis => time}
import downearth.gui.{MouseOut, MouseIn}

object MainWidget extends FreePanel(Vec2i(0),Vec2i(Display.getWidth,Display.getHeight) ) {

	border = NoBorder
	background = NoBackground

	override def setPosition(newPos:Vec2i, delay:Int) {}

  addReaction {
  case MouseClicked(pos) =>
    Player.primaryAction
  case MouseDrag(mousePos0,mousePos1) =>
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
    def setTopRight = setPosition(Vec2i(Main.width - size.x - 20, 20))

    addReaction {
    case DragEnd(_) =>
      moved = true
    }
  }

  children += inventory
}

class Widget( val position:Vec2i, val size:Vec2i) extends Listener[WidgetEvent] with Publisher[WidgetEvent] {
	var animationStartTime:Long = 0
	var animationEndTime:Long = 0
	val animationStartPosition = position.clone
	val animationEndPosition = position.clone

  var mouseOver = false

//  addReaction {
//  case event =>
//    print(this)
//    print(": ")
//    println(event)
//  }

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
		size := newSize
		position := safePosition(position)
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

  override def toString = s"Label($m_text)"
}

class TextureWidget(_position:Vec2i, _size:Vec2i, val texture:Texture, val texPosition:Vec2, val texSize:Vec2) extends Widget(_position, _size) {}

// TODO: Typparameter übergeben
abstract class Panel(_position:Vec2i, _size:Vec2i) extends Widget(_position, _size) { thispanel =>

	
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
class FreePanel(_position:Vec2i, _size:Vec2i) extends Panel(_position,_size)

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
  val dragStartPos = Vec2i(0)

  addReaction {
  case DragStart(pos) =>
    dragOriginalPosition := position
    dragStartPos := pos
  case MouseDrag(pos1, pos2) =>
    setPosition( dragOriginalPosition + (pos2 - dragStartPos) )
  }
}

