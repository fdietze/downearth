/**
 * User: arne
 * Date: 26.04.13
 * Time: 01:14
 */

package downearth.rendering

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL14._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL21._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL31._
import org.lwjgl.opengl.GL32._
import org.lwjgl.opengl.ARBFramebufferObject
import simplex3d.math.floatx.Vec4f

import org.lwjgl.BufferUtils
import org.lwjgl.opengl._
import org.lwjgl.opengl.ARBBufferObject._
import org.lwjgl.opengl.ARBVertexBufferObject._

import downearth._
import downearth.gui._
import downearth.util._
import downearth.tools._
import downearth.worldoctree._
import downearth.entity.{Entity, SimpleEntity}

import simplex3d.math.double._

import scala.collection.mutable.ArrayBuffer
import downearth.worldoctree.PowerOfTwoCube
import glwrapper._
import java.nio.ByteBuffer
import downearth.world.DynamicWorld
import downearth.worldoctree.Node.Traverse

class Renderer(gameState:GameState) extends Logger {
  import gameState._

  val occlusionTest = new OcclusionTest(this, gameState)
  val guiRenderer = new GuiRenderer(gameState)
  val riftDistort = new RiftDistort(gameState)
  val worldRenderer = new WorldRenderer(gameState)

  var frameCount = 0

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

  var tmp = true

  def draw() {

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

//    if(tmp){
//      framebuffer.bind {
//        val color = glwrapper.util.sharedFloatBuffer(4)
//        glwrapper.util.putVec4f(color, 1.0f,0.5f,0.0f,1.0f)
//        color.flip
//
//        org.lwjgl.opengl.Util.checkGLError()
//        glClearBuffer(GL_COLOR, 0, color)
//        org.lwjgl.opengl.Util.checkGLError()
//        glClearBuffer(GL_DEPTH, 0, color)
//        org.lwjgl.opengl.Util.checkGLError()
//
//        test_vao.bind {
//          test_program.use {
//            test_binding.writeChangedUniforms()
//            glDrawArraysInstanced(GL_QUADS, 0, 4, 2)
//          }
//        }
//        downearth.util.screenShot("framebuffer-test")
//
//        // val sunew Surface(w,h,data)
//
//      }
//
//      tmp = false
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
      if( Config.skybox ) skybox.render( camera )
      worldRenderer.renderWorld( camera )
      renderDebugWorld( camera )
    }

    val clearColor = glwrapper.util.sharedFloatBuffer(4)
    glwrapper.util.putVec4f(clearColor, 1.0f,0.5f,0.0f,1.0f)
    clearColor.flip
    def clear() {
      glClearBuffer(GL_COLOR, 0, clearColor)
      glClearBuffer(GL_DEPTH, 0, clearColor)
    }

    import player.camera
    val eyeDist = 0.06f

    if( Config.stereoRender ) {

      if( Config.anaglyph ) {
        clear()

        glViewport(0, 0, w, h)
        glColorMask(true,false,false,false)
        glClear(GL_DEPTH_BUFFER_BIT)
        camera.projection := glwrapper.util.stereoPorjectionF(n = Config.nearPlane.toFloat, v = w.toFloat/h.toFloat, eyeDist = eyeDist, leftEye = true)
        camera.move( Vec3(-eyeDist/2,0,0) )
        render( camera )
        glColorMask(false,true,true,false)
        glClear(GL_DEPTH_BUFFER_BIT)
        camera.projection := glwrapper.util.stereoPorjectionF(n = Config.nearPlane.toFloat, v = w.toFloat/h.toFloat, eyeDist = eyeDist, leftEye = false)
        camera.move( Vec3(eyeDist,0,0) )
        render( camera )
        camera.move( Vec3(-eyeDist/2,0,0) )
      }
      else {
        // do the rendering for occulus rift here.
        // TODO still need correct frustum for rift

        riftDistort.framebuffer.bind {
          clear()
          camera.projection := glwrapper.util.simpleProjectionF(n = Config.nearPlane.toFloat, v = w * 0.5f / h )
          glViewport(0, 0, w/2, h)
          camera.move( Vec3(-eyeDist/2,0,0) )
          render( camera )
          glViewport(w/2, 0, w/2, h)
          camera.move( Vec3(eyeDist,0,0) )
          render( camera )
          camera.move( Vec3(-eyeDist/2,0,0) )
        }

        clear()
        glViewport(0, 0, w, h)
        riftDistort.draw()
      }
    }
    else {
      clear()
      glViewport(0, 0, w, h)
      camera.projection := glwrapper.util.simpleProjectionF( n = Config.nearPlane.toFloat, v = w.toFloat / h.toFloat, f = Config.farPlane.toFloat )
      render( player.camera )
    }


    glViewport(0, 0, w, h)

    guiRenderer.renderGui()

    glRasterPos2f(33f,33f)
    textureManager.box.draw()



    frameCount += 1
  }

  def renderDebugWorld(camera:Camera) {

    glDisable(GL_LIGHTING)
    glDisable(GL_TEXTURE_2D)

    render3dCursor()

    lazy val frustumCulling = new FrustumCulling(camera.frustum)

    if( (Config.debugDraw & Config.DebugDrawOctreeBit) != 0 )
      drawDebugOctree(octree, camera.position, frustumCulling)
    if( (Config.debugDraw & Config.DebugDrawPhysicsBit) != 0 )
      physics.debugDrawWorld()
    if( (Config.debugDraw & Config.DebugDrawSampledNodesBit) != 0 )
      GlDraw.drawPredictedNodes()

    if( Config.testUngenerated && !(Config.skipOcclusionTestWhenBusy && frameState.workersBusy) ) {
      if( Config.occlusionTest ) {
        occlusionTest.doIt(frustumCulling)
      } else { // perform frustum test only
        octree.traverse( frustumCulling, camera.position) {
          case Traverse(area, UngeneratedNode) =>
            octree.generateArea(area)
            false
          case Traverse(_, GeneratingNode) =>
            false
          case Traverse(_, node:MeshNode) =>
            true
          case Traverse(_, node:NodeUnderMesh) =>
            !node.finishedGeneration
          case _ =>
            true
        }
      }
    }
  }

  def render3dCursor() {
    player.activeTool match {
      case tool:EnvironmentTool =>
        tool.renderPreview(GlDraw)
    }
  }

  def drawDebugOctree(octree:WorldOctree, camera:ReadVec3, culling:Culling) {
//    glPushMatrix()
//    val pos2 = octree.worldWindowPos + 0.05
//    glTranslated(pos2.x, pos2.y, pos2.z)
//    glColor3f(0,1,0)
//    GlDraw.renderCube(octree.worldWindowSize - 0.1)
//    glPopMatrix()

    var maximumDrawCalls = Config.maxDebugDrawCubes
    val maxDepth = log2(octree.rootArea.size)

    octree.traverse(culling, camera) {
      case Traverse(area, octant) =>
        glPushMatrix()
        val depth = maxDepth - log2(area.size)
        val padding = depth*0.05f

        val p = area.pos + padding
        glTranslatef(p.x.toFloat, p.y.toFloat, p.z.toFloat)

        octant match {
          case n:InnerNodeOverMesh =>
            glColor3f(0, 0.5f, 0)

          case n:MeshNode =>
            glColor3f(0,1,0)

          case n:InnerNodeUnderMesh =>
            glColor3f(0, 0, 0.5f)

          case n:Leaf =>
            glColor3f(1, 1, 1)

          case _ =>
            glColor3f(0.5f, 0.5f, 0.5f)
        }

        GlDraw.renderCube(area.size - padding*2)
        glPopMatrix()

        maximumDrawCalls -= 1

        maximumDrawCalls > 0
    }
  }

  var randomizer = 0


}


