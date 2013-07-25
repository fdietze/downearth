package downearth.rendering

import simplex3d.math._
import simplex3d.math.double._

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.Display
import org.lwjgl.BufferUtils
import org.newdawn.slick.Color

import java.nio.FloatBuffer
import downearth.worldoctree.{CuboidLike, Cuboid, PowerOfTwoCube, Polyeder}
import downearth.DisplayEvent
import downearth.util._
import glwrapper.util._
import simplex3d.math.floatx.{Vec4f, Vec2f, Vec3f}
import glwrapper.ArrayBuffer
import collection.mutable

object ConsoleFont {
	import org.newdawn.slick.UnicodeFont
	import org.newdawn.slick.font.effects._
	import java.awt.{Font,Color};
	import java.util.List
	val font = new UnicodeFont(new Font("Monospace", Font.BOLD, 14))
	font.addAsciiGlyphs()
	font.addGlyphs("äöüÄÖÜß")
	val effects = font.getEffects.asInstanceOf[List[Effect]]
	effects add new ShadowEffect
	effects add new ColorEffect(Color.WHITE)
	font.loadGlyphs
	
	def height = font.getLineHeight
}

trait Draw {
  def renderPolyeder(h:Polyeder, pos:Vec3i, color:Vec4)
  def drawLine(p1:Vec3, p2:Vec3, color:Vec4)
}

// useful primitive geometric objects
object GlDraw extends Draw {
  implicit class ExtendedBuffer( buffer: FloatBuffer ) {
    def putVertex(x:Float,y:Float,z:Float, w:Float = 1) {
      buffer put x
      buffer put y
      buffer put z
      buffer put w
    }
    def putColor(r:Float,g:Float,b:Float, a:Float = 1) {
      buffer put r
      buffer put g
      buffer put b
      buffer put a
    }
  }

  def drawLine(p1:Vec3, p2:Vec3, color:Vec4) {
    glColor4d(color.r, color.g, color.b, color.a)
    glBegin(GL_LINES)
      glVertex3d(p1.x, p1.y, p1.z)
      glVertex3d(p2.x, p2.y, p2.z)
    glEnd()
  }

  def renderAxis() {
    glBegin(GL_LINES)
    glColor3f(1,0,0)
    glVertex3f(0,0,0)
    glVertex3f(1,0,0)
    glColor3f(0,1,0)
    glVertex3f(0,0,0)
    glVertex3f(0,1,0)
    glColor3f(0,0,1)
    glVertex3f(0,0,0)
    glVertex3f(0,0,1)
    glEnd()
  }

	def renderCube(size:Double) {
		glPushMatrix()
			glScale1d(size)
			plainCube()
		glPopMatrix()
	}

	def renderCuboid(size:Vec3) {
		glPushMatrix()
			glScaled(size.x,size.y,size.z)
			plainCube()
		glPopMatrix()
	}

  lazy val texturedCubeBuffer = new {
    // GL_QUADS


    val normalsBuf   = new ArrayBuffer create()
    val texCoordsBuf = new ArrayBuffer create()
    val positionsBuf = new ArrayBuffer create()

    val normalsData = BufferUtils.createByteBuffer( sizeOf[Vec3f] * 24 )
    val texCoordsData = BufferUtils.createByteBuffer( sizeOf[Vec2f] * 24 )
    val positionsData = BufferUtils.createByteBuffer( sizeOf[Vec4f] * 24 )

    val positions = Seq.fill(24)(Vec4f(0))

    {
      val indices = Array(0,2, 3,1,  4,6,2,0, 0, 1,5,4, 1,3,7,5, 4,5,7,6, 6,7,3,2)
      val normals = Array(0,0,-1,0, -1,0,0,0, 0,-1,0,0, 1,0,0,0, 0,0,1,0, 0,1,0,0)

      var i = 0
      for(idx <- indices) {
        val x = (idx & 1) >> 0
        val y = (idx & 2) >> 1
        val z = (idx & 4) >> 2

        val j = i & 3
        val k = (i >> 2) << 2

        val u = ((j & 1) >> 0) ^ ((j & 2) >> 1)
        val v = (j & 2) >> 1

        putVec3f(normalsData, normals(k), normals(k+1), normals(k+2))
        putVec2f(texCoordsData, u, v)
        putVec4f(positionsData, x, y, z,1)
        positions(i) := Vec4f(x,y,z,1)

        i += 1
      }

      normalsData.flip
      texCoordsData.flip
      positionsData.flip

      normalsBuf.bind{
        normalsBuf.putData(normalsData)
      }
      texCoordsBuf.bind{
        texCoordsBuf.putData(texCoordsData)
      }
      positionsBuf.bind{
        positionsBuf.putData(positionsData)
      }
    }
  }

  def texturedCube() {
    glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT)

    glEnableClientState(GL_VERTEX_ARRAY)
    glEnableClientState(GL_NORMAL_ARRAY)
    glEnableClientState(GL_TEXTURE_COORD_ARRAY)

    glColor3f(1,1,1)

    import texturedCubeBuffer.{ positionsBuf, texCoordsBuf, normalsBuf }

    positionsBuf.bind{
      glVertexPointer(3, GL_FLOAT, 0, 0)
    }
    texCoordsBuf.bind{
      glTexCoordPointer(2, GL_FLOAT, 0, 0)
    }
    normalsBuf.bind{
      glNormalPointer(GL_FLOAT, 0, 0)
    }
    glDrawArrays(GL_QUADS, 0, 4*6)

    glPopClientAttrib()
  }

	def plainCube() {
		glBegin(GL_LINES)
		for(i <- 0 to 1;j <- 0 to 1;k <- 0 to 1)
			glVertex3f(i,j,k)
		for(i <- 0 to 1;j <- 0 to 1;k <- 0 to 1)
			glVertex3f(i,k,j)
		for(i <- 0 to 1;j <- 0 to 1;k <- 0 to 1)
			glVertex3f(k,i,j)
		glEnd()
	}

	def crossHair() {
		glPushMatrix()
      // TODO implement viewport
			glTranslated( Display.getWidth/2, Display.getHeight/2,0)
			glColor3f(1,1,1)
			glBegin(GL_LINES)
				glVertex2i(-15, 0)
				glVertex2i( -5, 0)
				glVertex2i(  5, 0)
				glVertex2i( 15, 0)

				glVertex2i(0, -15)
				glVertex2i(0,  -5)
				glVertex2i(0,   5)
				glVertex2i(0,  15)
			glEnd()
		glPopMatrix()
	}

	// rendert den Umriss eines Hexaeders, um ihn für die Selektion hervorheben zu können.
  override def renderPolyeder(h:Polyeder, pos:Vec3i, color:Vec4) {
    glColor4d(color.r, color.g, color.b, color.a)
    glPushMatrix()
    glTranslate3iv(pos)
    renderPolyeder(h)
    glPopMatrix()
  }

  def renderPolyeder(h:Polyeder) {
    val verts = h.vertices
    val indices = Seq(0,1,2,3,4,5,6,7,0,2,1,3,4,6,5,7,0,4,1,5,2,6,3,7)

    try {
      glBegin(GL_LINES)
      for(v <- indices map verts)
        glVertex3d(v.x,v.y,v.z)
      glEnd()
    }
    catch {
      case e:Exception =>
        println("cant draw Hexaeder: " + h + "\nvertices: " + h.vertices)
        throw e
    }
  }

	def highlight(pos:Vec3i, polyeder:Polyeder, transparent:Boolean = true) {
		glDisable(GL_LIGHTING)
		glDisable(GL_TEXTURE_2D)

		glPushMatrix()
			glTranslate3dv(Vec3(pos))

			// Transparent
			if( transparent ) {
				glDisable(GL_DEPTH_TEST)
				glEnable(GL_BLEND)
				glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
				glColor4f(1,1,1,0.25f)
				renderPolyeder(polyeder)
				glDisable(GL_BLEND)
			}

			// Not Transparent
			glEnable(GL_DEPTH_TEST)
			renderPolyeder(polyeder)
		glPopMatrix()
	}

	def drawNodeInfo(nodeinfo:PowerOfTwoCube) {
		glDisable(GL_LIGHTING)
		glDisable(GL_TEXTURE_2D)

		glPushMatrix()
			glTranslate3dv(nodeinfo.pos + 0.1)
			GlDraw.renderCube(nodeinfo.size - 0.2)
		glPopMatrix()
	}

	def drawCuboid(cuboid:CuboidLike) {
		glDisable(GL_LIGHTING)
		glDisable(GL_TEXTURE_2D)

		glPushMatrix()
			glTranslate3dv(cuboid.pos + 0.1)
			GlDraw.renderCuboid(cuboid.vsize - 0.2)
		glPopMatrix()
	}

	// Für den Debugdraw: alle Bereiche, die gesampled werden
//	var sampledNodes:List[Cube] = Nil
	var predictedCuboids = new mutable.ArrayBuffer[CuboidLike] with mutable.SynchronizedBuffer[CuboidLike]
//	def addSampledNode(toNodeinfo:Cube) { sampledNodes ::= toNodeinfo }
	def addPredictedCuboid(cuboid:CuboidLike) { predictedCuboids += cuboid }

	def drawSampledNodes() {
		for( cuboid <- predictedCuboids ) {
			if( cuboid.isCube )
				glColor3f(0,1,0.5f)
			else
				glColor3f(1,0,1)
			drawCuboid(cuboid)
		}
	}


	def drawString(pos:Vec2i, text:Any, color:Color = Color.white) {
		ConsoleFont.font.drawString( pos.x, pos.y, text.toString, color )
		glDisable( GL_LIGHTING )
		glDisable( GL_TEXTURE_2D )
	}

	// simuliert ein Konsolenähnliches verhalten, um Text auf dem Bildschirm darzustellen
	val textCache = collection.mutable.ArrayBuffer[String]()

	def addText(msg:Any) {
		textCache += msg.toString
	}

	def drawTexts {
		if( textCache.size > 0 ) {
			val pos = Vec2i(20)
			for( msg <- textCache ) {
				drawString(pos, msg)
				pos.y += ConsoleFont.height
			}
			textCache.clear
		}
	}

	def drawDisplayEvent(event:DisplayEvent, pos:Int) {

		val textPos = Vec2i( Display.getWidth - 20 - ConsoleFont.font.getWidth(event.textMessage),
			250 + ConsoleFont.height * pos)

		drawString(textPos, event.textMessage)
	}
}

