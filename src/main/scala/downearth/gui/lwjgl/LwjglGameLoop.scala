package downearth.gui.lwjgl

import downearth._
import org.lwjgl.opengl.Display
import org.lwjgl.input.{Keyboard, Mouse}
import simplex3d.math.Vec2i
import simplex3d.math.double._
import downearth.Config._
import downearth.util._
import downearth.gui.MainWidget
import simplex3d.math.doublex.functions._

/**
 * User: arne
 * Date: 29.04.13
 * Time: 00:39
 */
class LwjglGameLoop extends GameLoop {
  def swapBuffers() {
    Display.swapBuffers()
    Display.update()
  }

  def extraLoopOperation() {
    handleInput()
  }

  def handleInput() {
    import Mouse._
    import Keyboard._

    if(Display.isCloseRequested)
      finished = true

    lastMousePos := mousePos
    mousePos := Vec2i(getX, Main.height.toInt-Mouse.getY)


    val mouseDelta = Vec2i(getDX, getDY)
    // Move and rotate player
    val delta = Vec3(0)
    val delta_angle = Vec3(0)

    if(isKeyDown(keyForward))
      delta.z -= 1
    if(isKeyDown(keyBackward))
      delta.z += 1
    if(isKeyDown(keyLeft))
      delta.x -= 1
    if(isKeyDown(keyRight))
      delta.x += 1


    if( Mouse.isGrabbed ) {

      // rotate with mouse
      delta_angle.y = -mouseDelta.x/300.0
      delta_angle.x = mouseDelta.y/300.0

      // Turbo mode
      if( turbo && Mouse.isButtonDown(0) )
        Player.primaryAction


      // Keyboard Events
      while ( Keyboard.next ) {
        if (getEventKeyState) {
          getEventKey match {
            case `keyMouseGrab` =>
              Mouse setGrabbed false
            case `keyPlayerReset` =>
              Player.resetPos
            case `keyStreaming` =>
              streamWorld = !streamWorld
            case `keyWireframe` =>
              wireframe = !wireframe
            case `keyFrustumCulling` =>
              frustumCulling = !frustumCulling
            case `keyScreenshot` =>
              screenShot( "screenshot" )
            case `keyTurbo` =>
              turbo = ! turbo
              DisplayEventManager.showEventText("Turbo is "+(if(turbo) "on" else "off")+"." )
            case `keyQuit` =>
              finished = true
            case `keyPausePhysics` =>
              BulletPhysics.pause = !BulletPhysics.pause
            case `keyDebugDraw` =>
              debugDraw = !debugDraw
            case `keyToggleGhostPlayer` =>
              Player.toggleGhost
            case `keyJump` =>
              Player.jump
            case _ =>
          }
        }
      }

      // Mouse events
      while( Mouse.next ) {
        ( getEventButton, getEventButtonState ) match {
          case (0 , true) => // left down
          case (0 , false) => // left up
            Player.primaryAction
          case (1 , true) => // right down
          case (1 , false) => // right up
            //Player.secondarybutton
            Mouse setGrabbed false
            Mouse setCursorPosition( Main.width.toInt / 2, Main.height.toInt / 2)
          case (-1, false) => // wheel
          // Player.updownbutton( Mouse.getDWheel / 120 )
          case _ =>
        }
      }
    }
    else { // if Mouse is not grabbed

      if( mouseDelta != Vec2i(0) ) { // if Mouse is moved
        MainWidget.invokeMouseMoved(lastMousePos, mousePos)
      }

      // Keyboard Events
      while ( Keyboard.next ) {
        if (getEventKeyState) {
          getEventKey match {
            case `keyMouseGrab` =>
              Mouse setGrabbed true
            case `keyScreenshot` =>
              screenShot( "screenshot" )
            case `keyQuit` =>
              finished = true
            case _ =>
          }
        }
      }

      // Mouse events
      while( Mouse.next ) {
        ( getEventButton, getEventButtonState ) match {
          case (0 , true) => // left down
            MainWidget.invokeMouseDown(mousePos)
          case (0 , false) => // left up
            MainWidget.invokeMouseUp(mousePos)
          case (1 , true) => // right down
          case (1 , false) => // right up
            Mouse setGrabbed true
          case (-1, false) => // wheel
          case _ =>
        }
      }

    }

    val factor = if(turbo) cameraTurboSpeed else cameraSpeed
    Player.move(factor*(delta/max(1,length(delta)))*timeStep)
    Player.rotate(2.0*delta_angle)

  }
}
