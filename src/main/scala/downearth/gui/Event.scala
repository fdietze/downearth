package downearth.gui

import simplex3d.math.Vec2i
import scala.collection.mutable.{ArrayBuffer, HashSet}

/**
 * User: arne
 * Date: 12.05.13
 * Time: 15:24
 */

// TODO maybe use only case objects/ enumeration to define the event and sore all data in a singleton to reduce event creation overhead
trait MouseEvent
case class MouseMove(oldPos:Vec2i, newPos:Vec2i) extends MouseEvent
case class MouseDown(pos:Vec2i, button:Int) extends MouseEvent
case class MouseUp(pos:Vec2i, button:Int) extends MouseEvent


trait WidgetEvent
case class DragStart(firstPos:Vec2i) extends WidgetEvent
case class DragEnd(lastPos:Vec2i) extends WidgetEvent
case class MouseDrag(oldPos:Vec2i, newPos:Vec2i) extends WidgetEvent
case class MouseClicked(pos:Vec2i) extends WidgetEvent
case object MouseIn extends WidgetEvent
case object MouseOut extends WidgetEvent


// maybe -E
trait Listener[E] {
  val reactions = ArrayBuffer[PartialFunction[E,Unit]]()

  def addReaction( fun:PartialFunction[E,Unit] ) {
    reactions += fun
  }

  final def handleEvent(event:E) {
    reactions.foreach( reaction => if(reaction isDefinedAt event) reaction(event) )
  }

  final def listenTo(publisher:Publisher[E]) {
    publisher.subscribe(this)
  }

  final def deafTo(publisher:Publisher[E]) {
    publisher.unsubscribe(this)
  }
}

// maybe +E
trait Publisher[E] {
  val listeners = HashSet[Listener[E]]()

  def subscribe(listener:Listener[E]) {
    listeners += listener
  }

  def unsubscribe(listener:Listener[E]) {
    listeners += listener
  }

  def publish(event:E) {
    for( listener <- listeners ) {
      listener.handleEvent(event)
    }
  }
}
