/**
 * User: arne
 * Date: 26.04.13
 * Time: 01:14
 */

package downearth.rendering

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.BufferUtils
import org.lwjgl.input.Mouse
import org.lwjgl.opengl.{ARBFragmentShader, ARBVertexShader, ARBShaderObjects}
import org.lwjgl.opengl.ARBBufferObject._
import org.lwjgl.opengl.ARBVertexBufferObject._
import org.lwjgl.opengl.Display

import downearth._
import downearth.util._
import downearth.gui._
import downearth.gui.Border._
import downearth.gui.Background._
import downearth.worldoctree._
import downearth.world.World
import downearth.entity.{Entity, SimpleEntity}

import java.nio.IntBuffer

import scala.collection.mutable.ArrayBuffer
import scala.Tuple2

import simplex3d.math.Vec2i
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import downearth.worldoctree.NodeInfo
import scala.Tuple2

object Renderer extends Logger {

  val lightPos = BufferUtils.createFloatBuffer(4)
  val ambientLight = BufferUtils.createFloatBuffer(4)
  ambientLight.put( Array(0.2f, 0.2f, 0.2f, 1f) )
  ambientLight.rewind()

  var shader = 0
  var vertShader = 0
  var fragShader = 0

  // this is occlusion querry from the last frame
  var query:Query = null

  class Query(val buffer:IntBuffer,val nodeInfos:Seq[NodeInfo]) extends IndexedSeq[(Int,NodeInfo)] {
    require(buffer.limit == nodeInfos.length)
    val length    = buffer.limit()
    def apply(i:Int) = Tuple2(buffer.get(i), nodeInfos(i))
  }

  def draw() {
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT )
    renderWorld( Player.camera )
    renderGui()
  }

  def initshaders {
    shader = ARBShaderObjects.glCreateProgramObjectARB
    if( shader != 0 ) {
      vertShader = ARBShaderObjects.glCreateShaderObjectARB(ARBVertexShader.GL_VERTEX_SHADER_ARB)
      fragShader=ARBShaderObjects.glCreateShaderObjectARB(ARBFragmentShader.GL_FRAGMENT_SHADER_ARB)
      if( vertShader != 0 ) {
        val vertexPath = getClass.getClassLoader.getResource("shaders/screen.vert").getPath
        val vertexCode = io.Source.fromFile(vertexPath).mkString
        ARBShaderObjects.glShaderSourceARB(vertShader, vertexCode)
        ARBShaderObjects.glCompileShaderARB(vertShader)
      }

      if( fragShader != 0 ) {
        val fragPath = getClass.getClassLoader.getResource("shaders/screen.frag").getPath
        val fragCode = io.Source.fromFile(fragPath).mkString
        ARBShaderObjects.glShaderSourceARB(fragShader, fragCode)
        ARBShaderObjects.glCompileShaderARB(fragShader)
      }

      if(vertShader !=0 && fragShader !=0) {
        ARBShaderObjects.glAttachObjectARB(shader, vertShader)
        ARBShaderObjects.glAttachObjectARB(shader, fragShader)
        ARBShaderObjects.glLinkProgramARB(shader)
        ARBShaderObjects.glValidateProgramARB(shader)
      }
    }

//    printLogInfo(shader)
//    printLogInfo(vertShader)
//    printLogInfo(fragShader)
  }

  def lighting( position:Vec3 ) {
    if( Config.wireframe ) {
      glPolygonMode( GL_FRONT_AND_BACK, GL_LINE )
      glDisable(GL_LIGHTING)
    }
    else {
      glPolygonMode( GL_FRONT_AND_BACK, GL_FILL )
      glEnable(GL_LIGHTING)

      //Add positioned light
      lightPos.put(0, position(0).toFloat)
      lightPos.put(1, position(1).toFloat)
      lightPos.put(2, position(2).toFloat)
      lightPos.put(3, 1)

      glLight(GL_LIGHT0, GL_POSITION, lightPos )
      glEnable(GL_LIGHT0)

      //Add ambient light
      glLightModel(GL_LIGHT_MODEL_AMBIENT, ambientLight)
    }
  }

  def renderGui() {
    
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL ) // no wireframes
    
    glDisable(GL_DEPTH_TEST)
    glDisable(GL_LIGHTING)
    
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(0, Display.getWidth, Display.getHeight, 0, -100, 100)
    
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
  
//  Draw.addText("%d fps" format Main.currentfps)
//  Draw.addText("drawcalls: " + World.drawcalls + ", empty: " + World.emptydrawcalls + "")
//  Draw.addText("frustum culled nodes: " + World.frustumculls)
//  Draw.addText("")
//  Draw.addText("Inventory: " + Player.inventory.materials)
//    if( !Player.isGhost ) {
//      Draw.addText("Player Position: " + round10(Player.position) )
//      Draw.addText("Player Velocity: " + round10(Player.velocity) )
//    }
    
    glDisable( GL_LIGHTING )
    glDisable( GL_TEXTURE_2D )
    glEnable( GL_BLEND )
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA )
    
    Draw.drawTexts
    DisplayEventManager.draw

    if( Mouse.isGrabbed )
      Draw.crossHair

    drawWidget(MainWidget)
    
    glDisable(GL_BLEND)
  }

  def drawWidget(widget:Widget) {
    widget.invokeAnimation
    widget.background match {
      case ColorBackGround =>
        drawColorBackGround(widget.position, widget.size, widget.backGroundColor)
      case NoBackground =>
    }

    // no pattern matching, because branching in non-exclusive
    if( widget.isInstanceOf[ShapeWidget] ){
      drawShapeWidget(widget.asInstanceOf[ShapeWidget])
    }
    if( widget.isInstanceOf[Label]) {
      Draw.drawString( widget.position, widget.asInstanceOf[Label].text)
    }
    if( widget.isInstanceOf[TextureWidget] ) {
      val tw = widget.asInstanceOf[TextureWidget]
      import tw._

      glColor4f(1,1,1,1)

      texture.bind
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
      Draw.drawString(textPos, text, white)
    }

    import widget.{lineBorderColor => c}

    if( widget.isInstanceOf[GridPanel] && widget.asInstanceOf[GridPanel].border == LineBorder ) {
      glColor4d(c.r,c.g,c.b,c.a)
      drawLineGrid(widget.position, widget.size, widget.asInstanceOf[GridPanel].cellsize)
    }

    if( widget.isInstanceOf[Panel] )
      for( child <- widget.asInstanceOf[Panel].children )
        drawWidget(child)

    widget.border match {
      case LineBorder =>
        glColor4d(c.r,c.g,c.b,c.a)
        drawLineBorder(widget.position, widget.size)
      case NoBorder =>
    }
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
    Draw.renderPolyeder(ConstructionTool.all(widget.shapeId)(0))
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

  def renderWorld(camera:Camera) {
    glViewport(0, 0, Main.width.toInt, Main.height.toInt)
    glEnable(GL_CULL_FACE)
    glEnable(GL_COLOR_MATERIAL)
    glEnable(GL_TEXTURE_2D)

    glMatrixMode( GL_PROJECTION )
    glLoadMatrix( camera.projectionBuffer )

    Skybox.render

    glMatrixMode( GL_MODELVIEW )
    glLoadMatrix( camera.viewBuffer )

    lighting( camera.position )

    glEnable(GL_DEPTH_TEST)

    val frustumTest:FrustumTest =
      if( Config.frustumCulling )
        new FrustumTestImpl(camera.projection, camera.view)
      else {
        new FrustumTest {
          def testNode( info:NodeInfo ) = true
        }
      }

    drawCalls = 0
    emptyDrawCalls = 0

    val order = WorldOctree.frontToBackOrder(camera.direction)

    drawOctree(World.octree, frustumTest, order)

    World.dynamicWorld.entities foreach {
      case simple:SimpleEntity => ()
        glPushMatrix()
        glTranslated( simple.pos.x, simple.pos.y, simple.pos.z )
        drawObjMesh( simple.mesh )
        glPopMatrix()
      case entity:Entity => ()
    }

    // TODO this is not strictly render code
    // here the occlusion query from the last frame is evaluated, and a new one is generated

    if( query != null )
      for( result <- evalQueries(query).visible )
        World.octree.generateNode(result)
    query = findUngeneratedNodes(World.octree, frustumTest, order)

    // Player.activeTool.draw

    if(Config.debugDraw) {
      drawDebugOctree(World.octree, order, frustumTest)
      BulletPhysics.debugDrawWorld
      Draw.drawSampledNodes
    }
  }

  var drawCalls = 0
  var emptyDrawCalls = 0

  def drawDebugOctree(octree:WorldOctree, order:Array[Int], test:FrustumTest) {

    glDisable(GL_LIGHTING)
    glDisable(GL_TEXTURE_2D)

    glPushMatrix()
    val pos2 = octree.worldWindowPos + 0.05
    glTranslated(pos2.x, pos2.y, pos2.z)
    glColor3f(0,1,0)
    Draw.renderCube(octree.worldWindowSize - 0.1)
    glPopMatrix()

    var maximumDrawCalls = Config.maxDebugDrawQubes

    octree.queryRegion(test)(order) {
    case (info,octant) =>

      if(! octant.hasChildren) {
        glPushMatrix()
        val p = info.pos
        glTranslatef(p.x, p.y, p.z)

        if(octant.isInstanceOf[MeshNode])
          glColor3f(1,0,0)
        else
          glColor3f(0,0,1)

        Draw.renderCube(info.size)
        glPopMatrix()

        maximumDrawCalls -= 1
      }
      maximumDrawCalls > 0
    }
  }

  def findUngeneratedNodes(octree:WorldOctree, test:FrustumTest, order:Array[Int]) = {
    TextureManager.box.bind

    val nodeInfoBufferGenerating  = ArrayBuffer[NodeInfo]()
    val nodeInfoBufferUngenerated = ArrayBuffer[NodeInfo]()

    // TODO hier weiter machen
    octree.queryRegion( test ) (order) {
      case (info, UngeneratedInnerNode) =>
        nodeInfoBufferUngenerated += info
        false
      case (info, GeneratingNode) =>
        nodeInfoBufferGenerating += info
        false
      case (info, node:MeshNode) =>
        false
      case _ =>
        true
    }

    for( info <- nodeInfoBufferGenerating ) {
      glPushMatrix()
      glTranslatef(info.pos.x, info.pos.y, info.pos.z)
      glScaled(info.size,info.size,info.size)
      Draw.texturedCube()
      glPopMatrix()
    }

    val buffer = BufferUtils.createIntBuffer( nodeInfoBufferUngenerated.size )
    glGenQueries( buffer )
    val queries = new Query(buffer, nodeInfoBufferUngenerated)

    for( (queryId, info) <- queries ) {
      glBeginQuery(GL_SAMPLES_PASSED, queryId )

      glPushMatrix()
      glTranslatef(info.pos.x, info.pos.y, info.pos.z)
      glScaled(info.size,info.size,info.size)
      Draw.texturedCube()
      glPopMatrix()

      glEndQuery(GL_SAMPLES_PASSED)
    }

    queries
  }

  case class QueryResult(visible:Seq[NodeInfo], occluded:Seq[NodeInfo])

  def evalQueries(queries:Query) = {
    val visible  = ArrayBuffer[NodeInfo]()
    val occluded = ArrayBuffer[NodeInfo]()

    for( (id,info) <- queries ) {
      while( glGetQueryObjectui(id, GL_QUERY_RESULT_AVAILABLE) == GL_FALSE ) {
        Thread.sleep(1)
      }
      if( glGetQueryObjectui(id, GL_QUERY_RESULT) > 0 )
        visible += info
      else
        occluded += info
    }

    //log.println( s"occlusion query result (${queries.buffer.limit}):\noccluded: ${occluded.size}, visible: ${visible.size}" )
    glDeleteQueries(queries.buffer)

    QueryResult(visible, occluded)
  }

  def drawOctree(octree:WorldOctree, test:FrustumTest, order:Array[Int]) {
    import org.lwjgl.opengl.GL11._
    glColor3f(1,1,1)

    TextureManager.materials.bind

    glEnableClientState(GL_VERTEX_ARRAY)
    glEnableClientState(GL_NORMAL_ARRAY)
    glEnableClientState(GL_TEXTURE_COORD_ARRAY)

    val objMeshes = ArrayBuffer[(NodeInfo,ObjMesh)]()

    octree.queryRegion( test ) (order) {
      case (info, node:MeshNode) =>
        drawTextureMesh(node.mesh)
        objMeshes ++= node.objMeshes
        false
      case _ => true
    }

    for((info,mesh) <- objMeshes) {
      glPushMatrix()
      glTranslated( info.pos.x, info.pos.y, info.pos.z )
      glScale1d( info.size )
      drawObjMesh( mesh )
      glPopMatrix()
    }

    glDisableClientState(GL_VERTEX_ARRAY)
    glDisableClientState(GL_NORMAL_ARRAY)
    glDisableClientState(GL_TEXTURE_COORD_ARRAY)
  }

  def drawObjMesh(mesh:ObjMesh) {
    TextureManager.box.bind

    drawCalls += 1;
    mesh.bind()

    glEnableClientState(GL_VERTEX_ARRAY)
    glEnableClientState(GL_NORMAL_ARRAY)
    glEnableClientState(GL_TEXTURE_COORD_ARRAY)

    glVertexPointer(mesh.posComponents, GL_FLOAT, mesh.stride, mesh.posOffset)
    glTexCoordPointer(mesh.texCoordComponents, GL_FLOAT, mesh.stride, mesh.normalOffset)
    glNormalPointer(GL_FLOAT, mesh.stride, mesh.normalOffset)

    glDrawElements(GL_TRIANGLES, mesh.size, GL_UNSIGNED_INT, 0);

    glDisableClientState(GL_VERTEX_ARRAY)
    glDisableClientState(GL_NORMAL_ARRAY)
    glDisableClientState(GL_TEXTURE_COORD_ARRAY)

    mesh.unbind()
  }

  def drawTextureMesh(mesh:TextureMesh) {
    if( !mesh.hasVbo )
      mesh.genvbo()

    if( mesh.size > 0 ) {
      drawCalls += 1
      mesh.bind()
      glVertexPointer(mesh.vertices.components, mesh.vertices.rawEnum, mesh.vertices.byteStride, mesh.vertices.byteOffset)
      glNormalPointer(mesh.normals.rawEnum, mesh.normals.byteStride, mesh.normals.byteOffset)
      glTexCoordPointer(mesh.texcoords.components, mesh.texcoords.rawEnum, mesh.texcoords.byteStride, mesh.texcoords.byteOffset)
      glDrawArrays(GL_TRIANGLES, 0, mesh.vertices.size)

      glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0)
    }

    else {
      emptyDrawCalls += 1
    }
  }

  def activateShader(func: => Unit) {
    import ARBShaderObjects._

    val useshaders = shader != 0 && vertShader != 0 && fragShader != 0

    if(useshaders) {
      glUseProgramObjectARB(shader)
    }

    func

    if(useshaders)
      glUseProgramObjectARB(0)
  }
}


