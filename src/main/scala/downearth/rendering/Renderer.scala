/**
 * User: arne
 * Date: 26.04.13
 * Time: 01:14
 */

package downearth.rendering

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.{Display, ARBFragmentShader, ARBVertexShader, ARBShaderObjects}
import org.lwjgl.opengl.ARBBufferObject._
import org.lwjgl.opengl.ARBVertexBufferObject._

import downearth._
import downearth.gui._
import downearth.util._
import downearth.tools._
import downearth.worldoctree._
import downearth.world.World
import downearth.entity.{Entity, SimpleEntity}

import java.nio.IntBuffer

import simplex3d.math.Vec2i
import simplex3d.math.double._
import simplex3d.math.doublex.functions._

import scala.Tuple2
import scala.collection.mutable.ArrayBuffer
import downearth.generation.MaterialManager


object Renderer extends Logger {

  val lightPos = BufferUtils.createFloatBuffer(4)
  val ambientLight = BufferUtils.createFloatBuffer(4)
  ambientLight.put( Array(0.2f, 0.2f, 0.2f, 1f) )
  ambientLight.rewind()

  lazy val shaderProgram = {
    val vertShader = Shader[VertexShader]( getClass.getResourceAsStream("simple.vsh") )
    val fragShader = Shader[FragmentShader]( getClass.getResourceAsStream("simple.fsh") )
    Program(vertShader)(fragShader)
  }

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

    MainWidget.drawCallLabel.text = s"drawcalls: $drawCalls, empty: $emptyDrawCalls"
    MainWidget.playerPositionLabel.text = "Player Position: " + round10(Player.pos)

    GuiRenderer.renderGui()
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
    GlDraw.renderCube(octree.worldWindowSize - 0.1)
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

        GlDraw.renderCube(info.size)
        glPopMatrix()

        maximumDrawCalls -= 1
      }
      maximumDrawCalls > 0
    }
  }

  def renderWorld(camera:Camera) {

    glViewport(0, 0, Display.getWidth, Display.getHeight)
    glEnable(GL_CULL_FACE)
    glEnable(GL_COLOR_MATERIAL)
    glEnable(GL_TEXTURE_2D)

    glMatrixMode( GL_PROJECTION )
    glLoadMatrix( camera.projectionBuffer )

    Skybox.render

    glMatrixMode( GL_MODELVIEW )
    glLoadMatrix( camera.viewBuffer )

    lighting( camera.position )

    glEnable( GL_DEPTH_TEST )

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

    render3dCursor()

    import Config._

    if( (debugDraw & DebugDrawOctreeBit) != 0 )
      drawDebugOctree(World.octree, order, frustumTest)
    if( (debugDraw & DebugDrawPhysicsBit) != 0 )
      BulletPhysics.debugDrawWorld()
    if( (debugDraw & DebugDrawSampledNodesBit) != 0 )
      GlDraw.drawSampledNodes()
  }

  def render3dCursor() {

    Player.activeTool match {
      case tool:EnvironmentTool =>
        tool.renderPreview(GlDraw)
    }

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

  var randomizer = 0

  def findUngeneratedNodes(octree:WorldOctree, test:FrustumTest, order:Array[Int]) = {
    TextureManager.box.bind()

    val nodeInfoBufferGenerating  = ArrayBuffer[NodeInfo]()
    val nodeInfoBufferUngenerated = ArrayBuffer[NodeInfo]()

    glColorMask(false,false,false,false)

    // TODO hier weiter machen
    // TODO next time add comment, what is still work in Progress
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
      GlDraw.texturedCube()
      glPopMatrix()
    }

//    val firstIndex = max(0, 100 - nodeInfoBufferGenerating.size)
//    val count = nodeInfoBufferUngenerated.size - firstIndex
//    if( count > 0)
//      nodeInfoBufferUngenerated.remove(firstIndex, count)

    val buffer = BufferUtils.createIntBuffer( nodeInfoBufferUngenerated.size )
    glGenQueries( buffer )

    val queries = new Query(buffer, nodeInfoBufferUngenerated)
    for( (queryId, info) <- queries ) {
      glBeginQuery(GL_SAMPLES_PASSED, queryId )

      glPushMatrix()
      glTranslatef(info.pos.x, info.pos.y, info.pos.z)
      glScaled(info.size,info.size,info.size)
      GlDraw.texturedCube()
      glPopMatrix()

      glEndQuery(GL_SAMPLES_PASSED)
    }

    glColorMask(true, true, true, true)

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

    MaterialManager.textureAtlas.bind()

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
    TextureManager.box.bind()

    drawCalls += 1
    mesh.bind()

    glEnableClientState(GL_VERTEX_ARRAY)
    glEnableClientState(GL_NORMAL_ARRAY)
    glEnableClientState(GL_TEXTURE_COORD_ARRAY)

    glVertexPointer(mesh.posComponents, GL_FLOAT, mesh.stride, mesh.posOffset)
    glTexCoordPointer(mesh.texCoordComponents, GL_FLOAT, mesh.stride, mesh.normalOffset)
    glNormalPointer(GL_FLOAT, mesh.stride, mesh.normalOffset)

    glDrawElements(GL_TRIANGLES, mesh.size, GL_UNSIGNED_INT, 0)

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

    if(Config.useShaders) {
      shaderProgram.use()
    }

    func

    if(Config.useShaders)
      glUseProgramObjectARB(0)
  }
}


