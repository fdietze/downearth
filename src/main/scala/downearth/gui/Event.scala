package downearth.gui

import simplex3d.math.Vec2i
import scala.collection.mutable.{ArrayBuffer, HashSet}

/**
 * User: arne
 * Date: 12.05.13
 * Time: 15:24
 */

trait Event

// TODO maybe use only case objects/ enumeration to define the event and sore all data in a singleton to reduce event creation overhead
trait InputEvent extends Event
case class MouseMove(oldPos:Vec2i, newPos:Vec2i) extends InputEvent
case class MouseDown(pos:Vec2i, button:Int) extends InputEvent
case class MouseUp(pos:Vec2i, button:Int) extends InputEvent
case class KeyPress(key:Int) extends InputEvent
case class KeyRelease(key:Int) extends InputEvent

trait WidgetEvent extends Event
case class DragStart(firstPos:Vec2i) extends WidgetEvent
case class DragEnd(lastPos:Vec2i) extends WidgetEvent
case class MouseDrag(oldPos:Vec2i, newPos:Vec2i) extends WidgetEvent
case class MouseClicked(pos:Vec2i) extends WidgetEvent
case object MouseIn extends WidgetEvent
case object MouseOut extends WidgetEvent
case class WidgetMoved(widget:Widget) extends WidgetEvent
case class WidgetResized(widget:Widget) extends WidgetEvent
case class ButtonClicked(widget:Widget) extends WidgetEvent

trait Listener {
  val reactions = ArrayBuffer[PartialFunction[Event,Unit]]()

  def addReaction( fun:PartialFunction[Event,Unit] ) {
    reactions += fun
  }

  final def handleEvent(event:Event) {
    reactions.foreach( reaction => if(reaction isDefinedAt event) reaction(event) )
  }

  final def listenTo(publisher:Publisher) {
    publisher.subscribe(this)
  }

  final def deafTo(publisher:Publisher) {
    publisher.unsubscribe(this)
  }
}

trait Publisher {
  val listeners = HashSet[Listener]()

  def subscribe(listener:Listener) {
    listeners += listener
  }

  def unsubscribe(listener:Listener) {
    listeners += listener
  }

  def publish(event:Event) {
    for( listener <- listeners ) {
      listener.handleEvent(event)
    }
  }
}
