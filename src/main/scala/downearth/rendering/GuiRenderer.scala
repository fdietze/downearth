package downearth.rendering

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.Display
import org.lwjgl.input.Mouse

import simplex3d.math.doublex.functions._
import simplex3d.math.Vec2i
import simplex3d.math.double.
_
import downearth._
import downearth.gui._
import downearth.gui.Border._
import downearth.gui.Background._
import downearth.util._
import downearth.tools.ConstructionTool

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 5/27/13
 * Time: 5:39 PM
 * To change this template use File | Settings | File Templates.
 */
object GuiRenderer {

  def renderGui() {

      glPolygonMode( GL_FRONT_AND_BACK, GL_FILL ) // no wireframes

      glDisable( GL_DEPTH_TEST )
      glDisable( GL_LIGHTING )

      glMatrixMode( GL_PROJECTION )
      glLoadIdentity()
      glOrtho(0, Display.getWidth, Display.getHeight, 0, -100, 100)

      glMatrixMode(GL_MODELVIEW)
      glLoadIdentity()

      // log.println("%d fps" format Main.currentfps )

      glDisable( GL_LIGHTING )
      glDisable( GL_TEXTURE_2D )
      glEnable( GL_BLEND )
      glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA )

      GlDraw.drawTexts
      DisplayEventManager.draw

      if( Mouse.isGrabbed )
        GlDraw.crossHair()

      drawWidget(MainWidget)

      glDisable(GL_BLEND)
    }

    def drawWidget(widget:Widget) {
      if( !widget.visible )
        return

      widget.invokeAnimation
      widget.background match {
        case ColorBackGround =>
          drawColorBackGround(widget.position, widget.size, widget.backGroundColor)
        case NoBackground =>
      }

      import widget.{lineBorderColor => c}

      widget.border match {
        case LineBorder =>
          glColor4d(c.r,c.g,c.b,c.a)
          drawLineBorder(widget.position, widget.size)
        case NoBorder =>
      }

      // no pattern matching, because branching in non-exclusive
      if( widget.isInstanceOf[ShapeWidget] ){
        drawShapeWidget(widget.asInstanceOf[ShapeWidget])
      }
      if( widget.isInstanceOf[Label]) {
        GlDraw.drawString( widget.position, widget.asInstanceOf[Label].text)
      }
      if( widget.isInstanceOf[Button]) {
        GlDraw.drawString( widget.position, widget.asInstanceOf[Button].text)
      }
      if( widget.isInstanceOf[TextureWidget] ) {
        val tw = widget.asInstanceOf[TextureWidget]
        import tw._

        glColor4f(1,1,1,1)

        texture.bind()
        glEnable(GL_TEXTURE_2D)
        glBegin(GL_QUADS)

        glTexCoord2d(tw.texPosition.x, texPosition.y)
        glVertex2i(position.x         , position.y          )
        glTexCoord2d(tw.texPosition.x, texPosition.y + texSize.y)
        glVertex2i(position.x         , position.y + size.y )
        glTexCoord2d(tw.texPosition.x + texSize.x, texPosition.y + texSize.y)
        glVertex2i(position.x + size.x, position.y + size.y )
        glTexCoord2d(tw.texPosition.x + texSize.x, texPosition.y)
        glVertex2i(position.x + size.x, position.y          )

        glEnd()
        glDisable(GL_TEXTURE_2D)
      }
      if( widget.isInstanceOf[MaterialWidget] ) {
        val text = floor(Player.inventory.materials(widget.asInstanceOf[MaterialWidget].matId)).toInt
        val textSize = Vec2i(ConsoleFont.font.getWidth(text.toString) + 2, ConsoleFont.height)
        val textPos = widget.position + widget.size - textSize
        import org.newdawn.slick.Color.white
        GlDraw.drawString(textPos, text, white)
      }

      if( widget.isInstanceOf[GridPanel] && widget.asInstanceOf[GridPanel].border == LineBorder ) {
        glColor4d(c.r,c.g,c.b,c.a)
        drawLineGrid(widget.position, widget.size, widget.asInstanceOf[GridPanel].cellsize)
      }

      if( widget.isInstanceOf[Panel] )
        for( child <- widget.asInstanceOf[Panel].children )
          drawWidget(child)
    }

    def drawShapeWidget(widget:ShapeWidget) {
      glPushMatrix()
      glColor4f(1,1,1,1)
      glTranslate3dv(Vec3(widget.position+widget.size/2,0))
      glScalef(20,20,20)
      glRotatef(72,1,0,0)

      if( widget.mouseOver || (widget.degTime - widget.lastMouseOut + widget.outOffset) < 360.0 )
        glRotated(widget.degTime - widget.inOffset + widget.preferredAngle,0,0,1)
      else
        glRotated(widget.preferredAngle,0,0,1)

      glTranslatef(-0.5f,-0.5f,-0.5f)
      GlDraw.renderPolyeder(ConstructionTool.all(widget.shapeId)(0))
      glPopMatrix()
    }

    def drawColorBackGround(position:Vec2i, size:Vec2i, color:Vec4) {
      glColor4d(color.r, color.g, color.b, color.a)

      glBegin(GL_QUADS)
      glVertex2i(position.x         , position.y)
      glVertex2i(position.x         , position.y + size.y)
      glVertex2i(position.x + size.x, position.y + size.y)
      glVertex2i(position.x + size.x, position.y)
      glEnd()
    }

    def drawLineBorder(position:Vec2i, size:Vec2i) {
      glBegin(GL_LINE_LOOP)
      glVertex2i(position.x-1       , position.y)
      glVertex2i(position.x         , position.y + size.y)
      glVertex2i(position.x + size.x, position.y + size.y)
      glVertex2i(position.x + size.x, position.y)
      glEnd()
    }

    def drawLineGrid(position:Vec2i, size:Vec2i, cellSize:Int) {
      glBegin(GL_LINES)
      var x = 0
      while( x < size.x ) {
        glVertex2i(position.x + x, position.y + 0)
        glVertex2i(position.x + x, position.y + size.y)
        x += cellSize
      }
      var y = 0
      while( y < size.y ) {
        glVertex2i(position.x + 0     , position.y + y)
        glVertex2i(position.x + size.x, position.y + y)
        y += cellSize
      }
      glEnd()
    }

}
