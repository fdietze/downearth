/**
 * User: arne
 * Date: 26.04.13
 * Time: 01:14
 */

package downearth.rendering

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._

import simplex3d.math.floatx.{Mat4f, Vec4f, Vec3f}
import downearth.worldoctree.NodeInfo
import scala.Tuple2

import org.lwjgl.BufferUtils
import org.lwjgl.opengl._
import org.lwjgl.opengl.ARBBufferObject._
import org.lwjgl.opengl.ARBVertexBufferObject._

import downearth._
import downearth.gui._
import downearth.util._
import downearth.tools._
import downearth.worldoctree._
import downearth.world.World
import downearth.entity.{Entity, SimpleEntity}
import downearth.resources.MaterialManager
import downearth.rendering.shader._

import java.nio.IntBuffer

import simplex3d.data._
import simplex3d.data.double._
import simplex3d.math.double._
import simplex3d.math.doublex.functions._
import simplex3d.math.float.functions.normalize

import scala.Tuple2
import scala.collection.mutable.ArrayBuffer
import downearth.worldoctree.NodeInfo
import scala.Tuple2

object Renderer extends Logger {

  val lightPos = BufferUtils.createFloatBuffer(4)
  val ambientLight = BufferUtils.createFloatBuffer(4)
  ambientLight.put( Array(0.2f, 0.2f, 0.2f, 1f) )
  ambientLight.rewind()

  var frameCount = 0

  lazy val transformFeedbackTest = {
    val vertShader = Shader[VertexShader]( getClass.getClassLoader.getResourceAsStream("shaders/simple.vsh") )
    val program = Program("transformFeedbackTest")(vertShader)()
    vertShader.delete()
    program
  }

  val tfbBinding = transformFeedbackTest.getBinding

  val tfb_a_instance_position = tfbBinding.attributeVec3f("a_instance_position")
  val tfb_instance_scale = tfbBinding.attributeFloat("a_instance_scale")

  val vaoTransformFeedbackTest = VertexArrayObject.create

  vaoTransformFeedbackTest.bind {
    tfb_instance_scale.divisor = 1
    tfb_a_instance_position.divisor = 1

    tfbBinding.setAttributePointers()
  }

  val tfb_a_position =  tfbBinding.attributeVec3f("tfb_a_position")
  val tfb_u_mvp =  tfbBinding.uniformMat4f("u_mvp")
  val gl_Position = tfbBinding.transformFeedbackVec4f("gl_Position")

  val testProgram = Program.auto("test")

  val testBinding = testProgram.getBinding

  val test_a_pos = testBinding.attributeVec4f("a_pos")
  val test_a_offset = testBinding.attributeVec4f("offset")
  test_a_pos    := Seq( Vec4f(-0.5f,-0.5f, 0,1), Vec4f(0.5f, -0.5f, 0, 1), Vec4f(0.5f, 0.5f, 0, 1), Vec4f(-0.5f, 0.5f, 0, 1) )
  test_a_offset := Seq( Vec4f(0), Vec4f(0.3f,0.3f,0.3f,0) )

  val vaoTestProgram = VertexArrayObject.create

  vaoTestProgram.bind {
    test_a_offset.divisor = 1
    testBinding.setAttributePointers()
  }

  lazy val test2Program = Program.auto("test2")

  val test2Binding = test2Program.getBinding

  val test2_a_pos = test2Binding.attributeVec4f("a_pos")
  val test2_offset = test2Binding.attributeVec4f("offset")
  val test2_scale  = test2Binding.attributeFloat("scale")

  val vaoTest2 = VertexArrayObject.create

  vaoTest2.bind {
    test2_offset.divisor = 1
    test2_scale.divisor = 1

    test2Binding.setAttributePointers()
  }

  test2_a_pos    := GlDraw.texturedCubeBuffer.positionsData
  test2_offset   := Seq( Vec4f(0,0,0,0), Vec4f(2,0,0,0), Vec4f(0,3,0,0), Vec4f(0,0,5,0)  )
  test2_scale    := Seq[Float]( 1,2,3,4 )

  val test2_matrix = test2Binding.uniformMat4f("matrix")

  lazy val occTestProgram = Program.auto("simple")

  val occTestBinding = occTestProgram.getBinding

  val occTest_matrix  = occTestBinding.uniformMat4f("matrix")
  val occTest_offset  = occTestBinding.attributeVec4f("offset")
  val occTest_scale   = occTestBinding.attributeFloat("scale")
  val occTest_u_tint  = occTestBinding.uniformVec4f("u_tint")
  val occTest_a_pos   = occTestBinding.attributeVec4f("a_pos")

  occTest_a_pos := GlDraw.texturedCubeBuffer.positions

  val vaoShaderProgram = VertexArrayObject.create

  vaoShaderProgram.bind {
    occTest_offset.divisor = 1
    occTest_scale.divisor = 1

    occTestBinding.setAttributePointers()
  }

  var query:Query = null

  class Query(val buffer:IntBuffer,val nodeInfos:Seq[NodeInfo]) extends IndexedSeq[(Int,NodeInfo)] {
    require(buffer.limit == nodeInfos.length)
    val length    = buffer.limit()
    def apply(i:Int) = Tuple2(buffer.get(i), nodeInfos(i))
  }

  def draw() {
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT )

    import org.lwjgl.opengl.GL30._
    import org.lwjgl.opengl.GL31._


    if(false) {
      val VertexCount = 23

      // Disable rasterisation, vertices processing only!
      glEnable(GL_RASTERIZER_DISCARD)
      val Query = glGenQueries()

      transformFeedbackTest.use {
        tfb_a_instance_position := Seq(Vec3f(0))
        tfb_instance_scale := Seq[Float]( 0.0f )
        tfb_u_mvp := Mat4f(1)

        tfbBinding.bind()

        glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 0, gl_Position.location)

        vaoTransformFeedbackTest.bind {
          glBeginQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, Query)
          glBeginTransformFeedback(GL_TRIANGLES)
          glDrawArraysInstanced(GL_TRIANGLES, 0, VertexCount, 1)
          glEndTransformFeedback()
          glEndQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN)
        }
      }
      glDisable(GL_RASTERIZER_DISCARD);
    }

    vaoTestProgram.bind {
      testProgram.use {
        testBinding.enableAttributes()
        testBinding.bind()
        glDrawArraysInstanced(GL_QUADS, 0, 4, 2)
        testBinding.disableAttributes()
      }
    }

    glEnable( GL_DEPTH_TEST )

    val projection = Mat4f(Player.camera.projection)
    val view = Mat4f(Player.camera.view)

    vaoTest2.bind {
      test2Program.use {
        test2Binding.enableAttributes()

        test2_matrix := projection * view

        test2Binding.bindChanges()
        glDrawArraysInstanced(GL_QUADS, 0, 24, 4)
        test2Binding.disableAttributes()
      }
    }

    vaoShaderProgram.bind {
      occTestProgram.use{
        occTest_matrix := projection * view

        occTest_offset := Seq( Vec4f(0,0,0,0) )
        occTest_scale  := Seq( 16.0f )

        occTestBinding.bindChanges()
        glDrawArrays(GL_QUADS, 0, 6*4)
        test2Binding.disableAttributes()
      }
    }

    renderWorld( Player.camera )

    MainWidget.drawCallLabel.text = s"draw calls: $drawCalls, empty: $emptyDrawCalls"
    MainWidget.playerPositionLabel.text = "Player Position: " + round10(Player.pos)

    GuiRenderer.renderGui()

    frameCount += 1
  }

  var drawCalls = 0
  var emptyDrawCalls = 0

  def drawDebugOctree(octree:WorldOctree, order:Array[Int], test:FrustumTest) {

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

    if( Config.occlusionTest ) {
      if( query != null )
        for( result <- evalQueries(query).visible )
          World.octree.generateNode(result)
      query = findUngeneratedNodes(camera, World.octree, frustumTest, order)
    } else { // perform frustum test only
      World.octree.queryRegion( frustumTest ) (order) {
        case (info, UngeneratedInnerNode) =>
          World.octree.generateNode(info)
          false
        case (info, GeneratingNode) =>
          false
        case (info, node:MeshNode) =>
          false
        case _ =>
          true
      }
    }


    import Config._

    glDisable(GL_LIGHTING)
    glDisable(GL_TEXTURE_2D)

    render3dCursor()

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

  def lighting( position:ReadVec3 ) {
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

  def findUngeneratedNodes(camera:Camera, octree:WorldOctree, test:FrustumTest, order:Array[Int]) = {
    TextureManager.box.bind()

    val nodeInfoBufferGenerating  = ArrayBuffer[NodeInfo]()
    val nodeInfoBufferUngenerated = ArrayBuffer[NodeInfo]()

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

    val model = Mat4(1)
    val view = Mat4(camera.view)
    val projection = Mat4(camera.projection)

    val magicNr = 8
    val (doTest,noTest) = (0 until nodeInfoBufferUngenerated.size).partition(i => i % magicNr == frameCount % magicNr)

    val renderNodeInfos1 = nodeInfoBufferGenerating
    val renderNodeInfos2 = noTest map nodeInfoBufferUngenerated
    val reducedNodeInfos = doTest map nodeInfoBufferUngenerated
    val buffer = BufferUtils.createIntBuffer( reducedNodeInfos.size )
    glGenQueries( buffer )
    val queries = new Query(buffer, reducedNodeInfos)

    occTestProgram.use {
      vaoShaderProgram.bind {
        occTestBinding.enableAttributes()

        if( !Config.visibleOcclusionTest )
          glColorMask(false,false,false,false)

        occTest_matrix := Mat4f(projection * view)

        for( (tint, renderNodeInfos) <- Seq[(Vec4f,Seq[NodeInfo])]( (Vec4f(1,1,0,1), renderNodeInfos1), (Vec4f(0,1,0,1), renderNodeInfos2) ) if renderNodeInfos.size > 0 ) {
          occTest_u_tint := tint

          val instance_positions = renderNodeInfos.map( info => Vec4f(info.pos,0) )
          val instance_scales = renderNodeInfos.map( info => info.size.toFloat )

          occTest_offset := instance_positions
          occTest_scale := instance_scales

          occTestBinding.bindChanges()

          GL31.glDrawArraysInstanced(GL_QUADS, 0, 24, renderNodeInfos.size)
        }

        for( (queryId, info) <- queries ) {
          glBeginQuery(GL_SAMPLES_PASSED, queryId )

          occTest_u_tint := Vec4f(1,0,0,1)
          occTest_offset := Seq( Vec4f( info.pos, 0) )
          occTest_scale := Seq( info.size.toFloat )

          occTestBinding.bindChanges()

          GL31.glDrawArraysInstanced(GL_QUADS, 0, 24, 1)

          glEndQuery(GL_SAMPLES_PASSED)
        }

        occTestBinding.disableAttributes()
      }

      glColorMask(true, true, true, true)
    }

    queries
  }

  case class QueryResult(visible:Seq[NodeInfo], occluded:Seq[NodeInfo])

  def evalQueries(queries:Query) = {
    val visible  = ArrayBuffer[NodeInfo]()
    val occluded = ArrayBuffer[NodeInfo]()

    for( (id,info) <- queries ) {
      while( glGetQueryObjectui(id, GL_QUERY_RESULT_AVAILABLE) == GL_FALSE ) {
        Thread.sleep(1) //TODO: Don't wait! Instead check only once every frame
      }
      if( glGetQueryObjectui(id, GL_QUERY_RESULT) > 0 ) //TODO: Configurable pixel threshold
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

    glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT)

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

    glPopClientAttrib()
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
}


