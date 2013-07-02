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

  val tfb_binding = transformFeedbackTest.getBinding

  val tfb_a_instance_position = tfb_binding.attributeVec3f("a_instance_position")
  val tfb_instance_scale = tfb_binding.attributeFloat("a_instance_scale")
  val tfb_a_position =  tfb_binding.attributeVec3f("tfb_a_position")
  val tfb_u_mvp =  tfb_binding.uniformMat4f("u_mvp")
  //val tfb_gl_Position = tfb_binding.transformFeedbackVec4f("gl_Position")

  val tfb_vao = VertexArrayObject.create

  tfb_vao.bind {
    tfb_instance_scale.divisor = 1
    tfb_a_instance_position.divisor = 1

    tfb_binding.enableAttributes()
    tfb_binding.setAttributePointers()
  }

  val test_program = Program.auto("test")
  val test_binding = test_program.getBinding

  val test_a_pos = test_binding.attributeVec4f("a_pos")
  val test_a_offset = test_binding.attributeVec4f("offset")
  test_a_pos    := Seq( Vec4f(-0.5f,-0.5f, 0,1), Vec4f(0.5f, -0.5f, 0, 1), Vec4f(0.5f, 0.5f, 0, 1), Vec4f(-0.5f, 0.5f, 0, 1) )
  test_a_offset := Seq( Vec4f(0), Vec4f(0.3f,0.3f,0.3f,0) )

  val test_vao = VertexArrayObject.create

  test_vao.bind {
    test_a_offset.divisor = 1

    test_binding.enableAttributes()
    test_binding.setAttributePointers()
  }

  lazy val test2Program = Program.auto("test2")

  val test2Binding = test2Program.getBinding

  val test2_a_pos = test2Binding.attributeVec4f("a_pos")
  val test2_offset = test2Binding.attributeVec4f("offset")
  val test2_scale  = test2Binding.attributeFloat("scale")

  val test2_vao = VertexArrayObject.create

  test2_vao.bind {
    test2_offset.divisor = 1
    test2_scale.divisor = 1

    test2Binding.enableAttributes()
    test2Binding.setAttributePointers()
  }

  test2_a_pos    := GlDraw.texturedCubeBuffer.positionsData
  test2_offset   := Seq( Vec4f(0,0,0,0), Vec4f(2,0,0,0), Vec4f(0,3,0,0), Vec4f(0,0,5,0)  )
  test2_scale    := Seq[Float]( 1,2,3,4 )

  val test2_matrix = test2Binding.uniformMat4f("matrix")

  def draw() {
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT )



//    if(false) {
//      val VertexCount = 23
//      import org.lwjgl.opengl.GL30._
//      import org.lwjgl.opengl.GL31._
//
//      // Disable rasterisation, vertices processing only!
//      glEnable(GL_RASTERIZER_DISCARD)
//      val Query = glGenQueries()
//
//      transformFeedbackTest.use {
//        tfb_a_instance_position := Seq(Vec3f(0))
//        tfb_instance_scale := Seq[Float]( 0.0f )
//        tfb_u_mvp := Mat4f(1)
//
//        tfb_binding.writeChangedUniforms()
//
//        glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 0, tfb_gl_Position.location)
//
//        tfb_vao.bind {
//          glBeginQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, Query)
//          glBeginTransformFeedback(GL_TRIANGLES)
//          glDrawArraysInstanced(GL_TRIANGLES, 0, VertexCount, 1)
//          glEndTransformFeedback()
//          glEndQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN)
//        }
//      }
//      glDisable(GL_RASTERIZER_DISCARD)
//    }

//    test_vao.bind {
//      test_program.use {
//        test_binding.writeAllUniforms()
//        glDrawArraysInstanced(GL_QUADS, 0, 4, 2)
//      }
//    }

//    val projection = Mat4f(Player.camera.projection)
//    val view = Mat4f(Player.camera.view)
//    val matrix = projection * view

//    test2_vao.bind {
//      test2Program.use {
//
//
//        test2_matrix := matrix
//        test2Binding.writeChangedUniforms()
//        glDrawArraysInstanced(GL_QUADS, 0, 24, 4)
//      }
//    }

//    occTest_vao.bind {
//      occTest_program.use {
//        occTest_matrix := matrix
//        occTest_offset := Seq( Vec4f(0,0,0,0) )
//        occTest_scale  := Seq( 16.0f )
//
//        occTest_u_tint := Vec4f( 0.5f, 0.5f, 0.5f, 1.0f )
//
//        occTest_binding.writeChangedUniforms()
//        glDrawArrays(GL_QUADS, 0, 6*4)
//      }
//    }


    val w = Display.getWidth
    val h = Display.getHeight

    def render(camera:Camera) {
      if( Config.skybox ) {
        Skybox.render( camera )
      }
      renderWorld( camera )
    }

    if( Config.stereoRender ) {
      if( ! Config.anaglyph ) {
        glViewport(0, 0, w/2, h)
        render( Player.leftEye )
        glViewport(w/2, 0, w/2, h)
        render( Player.rightEye )
      }
      else  {
        glViewport(0, 0, w, h)
        glColorMask(true,false,false,false)
        glClear(GL_DEPTH_BUFFER_BIT)
        render( Player.leftEye )
        glColorMask(false,true,true,false)
        glClear(GL_DEPTH_BUFFER_BIT)
        render( Player.rightEye )
      }
    }
    else {
      glViewport(0, 0, w, h)
      render( Player.camera )
    }




    glViewport(0, 0, w, h)
    MainWidget.drawCallLabel.text = s"draw calls: $drawCalls, empty: $emptyDrawCalls"
    MainWidget.playerPositionLabel.text = "Player Position: " + round10(Player.pos)

    GuiRenderer.renderGui()

    frameCount += 1
  }

  var drawCalls = 0
  var emptyDrawCalls = 0

  def drawDebugOctree(octree:WorldOctree, camera:ReadVec3, test:FrustumTest) {

    glPushMatrix()
    val pos2 = octree.worldWindowPos + 0.05
    glTranslated(pos2.x, pos2.y, pos2.z)
    glColor3f(0,1,0)
    GlDraw.renderCube(octree.worldWindowSize - 0.1)
    glPopMatrix()

    var maximumDrawCalls = Config.maxDebugDrawQubes

    octree.queryRegion(test,camera) {
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
    glEnable(GL_CULL_FACE)
    glEnable(GL_COLOR_MATERIAL)
    glEnable(GL_TEXTURE_2D)
    glEnable(GL_DEPTH_TEST)

    glMatrixMode( GL_PROJECTION )
    glLoadMatrix( camera.projectionBuffer )

    glMatrixMode( GL_MODELVIEW )
    glLoadMatrix( camera.viewBuffer )

    lighting( camera.position )

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

    drawOctree(World.octree, frustumTest, camera.position)

    World.dynamicWorld.entities foreach {
      case simple:SimpleEntity => ()
        glPushMatrix()
        glTranslated( simple.pos.x, simple.pos.y, simple.pos.z )
        drawObjMesh( simple.mesh )
        glPopMatrix()
      case entity:Entity => ()
    }

    if( Config.occlusionTest ) {
      OcclusionTest.doIt(camera, frustumTest)
    } else { // perform frustum test only
      World.octree.queryRegion( frustumTest, camera.position) {
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
      drawDebugOctree(World.octree, camera.position, frustumTest)
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

  def drawOctree(octree:WorldOctree, test:FrustumTest, camera:ReadVec3) {
    import org.lwjgl.opengl.GL11._
    glColor3f(1,1,1)

    MaterialManager.textureAtlas.bind()

    glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT)

    glEnableClientState(GL_VERTEX_ARRAY)
    glEnableClientState(GL_NORMAL_ARRAY)
    glEnableClientState(GL_TEXTURE_COORD_ARRAY)

    val objMeshes = ArrayBuffer[(NodeInfo,ObjMesh)]()

    octree.queryRegion( test, camera ) {
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


