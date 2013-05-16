package downearth.gui

import downearth.util._
import simplex3d.math.Vec2i
import simplex3d.math.double.Vec2
import simplex3d.math.double.functions.distance

class WidgetEventDispatcher(val topWidget:Widget) extends Listener {
  val DragThreshHold = 5.0

  var currentWidget = topWidget
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
        while( !indexInRange(newPos, currentWidget.position, currentWidget.size) && currentWidget != topWidget ) {
          currentWidget handleEvent MouseOut
          currentWidget = currentWidget.parent
          currentWidget handleEvent MouseIn
        }

        // locate widget where the mouse is over
        var continue = true
        while(continue) {
          val result:Option[Widget] =
            currentWidget match {
              case panel:Panel =>
                panel.children.find( w => w.visible && indexInRange(newPos,w.position,w.size) )
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
        if( !dragging ) {
          if( distance(Vec2(dragStartPos),Vec2(newPos)) > DragThreshHold ) {
            currentWidget handleEvent DragStart(dragStartPos)
            currentWidget handleEvent MouseDrag(dragStartPos, newPos)
            dragging = true
          }
        }
        else {
          currentWidget handleEvent MouseDrag(oldPos, newPos)
        }
      }
    case kp =>
      currentWidget handleEvent kp
  }
}