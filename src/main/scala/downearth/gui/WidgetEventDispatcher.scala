package downearth.gui

import downearth.util._
import simplex3d.math.Vec2i

class WidgetEventDispatcher(widget:Widget) extends Listener[MouseEvent] {
  var currentWidget = widget
  var mousePressed = false
  var dragging = false
  val dragStartPos = Vec2i(0)

  addReaction {
    case MouseDown(mousePos, 0) =>
      mousePressed = true
      dragStartPos := mousePos
    case MouseUp(mousePos, 0) =>
      mousePressed = false
      if( dragging ) {
        currentWidget handleEvent DragEnd(mousePos)
        dragging = false
      }
      else {
        currentWidget handleEvent MouseClicked(mousePos)
      }
    case MouseMove(oldPos, newPos) =>
      if( !mousePressed ) {
        while( !indexInRange(newPos, currentWidget.position, currentWidget.size) ) {
          currentWidget handleEvent MouseOut
          currentWidget = currentWidget.parent
          currentWidget handleEvent MouseIn
        }
        var continue = true
        while(continue) {
          val result:Option[Widget] =
            currentWidget match {
              case panel:Panel =>
                panel.children.find( w => indexInRange(newPos,w.position,w.size) )
              case _ => None
            }
          result match {
            case Some(w) =>
              currentWidget handleEvent MouseOut
              currentWidget = w
              currentWidget handleEvent MouseIn
            case None =>
              continue = false
          }
        }
      }
      else {
        dragging = true
        currentWidget handleEvent MouseDrag(oldPos, newPos)
      }
  }
}