package downearth.rendering

import glwrapper.{VertexArrayObject, Program}
import org.lwjgl.opengl.ARBDrawInstanced
import ARBDrawInstanced._
import downearth.{FrustumTest, Camera, Config}
import downearth.worldoctree._
import downearth.GameState

import simplex3d.math.floatx.{Vec3f, Mat4f, Vec4f}
import simplex3d.backend.lwjgl.ArbEquivalents

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.BufferUtils

import scala.collection.mutable.ArrayBuffer
import java.nio.IntBuffer

/**
 * User: arne
 * Date: 05.07.13
 * Time: 18:54
 */
class OcclusionTest(renderer:Renderer, gameState:GameState) {
  import gameState._

  val generatingColor = Vec4f(0.6f,0.6f,0.3f,1)
  val skipColor = Vec4f(0.2f,0.2f,0.2f,1)
  val testColor = Vec4f(0.6f,0.3f,0.3f,1)

  val occTest_program = Program.auto("simple")

  val occTest_binding = occTest_program.getBinding

  val occTest_matrix  = occTest_binding.uniformMat4f("matrix")
  val occTest_offset  = occTest_binding.attributeVec4f("offset")
  val occTest_scale   = occTest_binding.attributeFloat("scale")
  val occTest_u_tint  = occTest_binding.uniformVec4f("u_tint")
  val occTest_a_pos   = occTest_binding.attributeVec4f("a_pos")

  occTest_a_pos := GlDraw.cubeTriangleStrip.positions

  val occTest_vao = VertexArrayObject.create

  occTest_vao.bind {
    occTest_offset.divisor = 1
    occTest_scale.divisor = 1

    occTest_binding.enableAttributes()
    occTest_binding.setAttributePointers()
  }

  def findUngeneratedNodes(frustumTest:FrustumTest) = {
    import gameState.player.camera
    import gameState.player
    // TextureManager.box.bind()

    val generatingAreas  = ArrayBuffer[PowerOfTwoCube]()
    val ungeneratedAreas = ArrayBuffer[PowerOfTwoCube]()
    val filter = (area:PowerOfTwoCube) => player.canSee(area) && frustumTest(area)

    octree.query(filter, camera.position) {
      case (info, UngeneratedNode) =>
        ungeneratedAreas += info
        false
      case (info, GeneratingNode) =>
        generatingAreas += info
        false
      case (info, node:MeshNode) =>
        true
      case (info, node:NodeUnderMesh) =>
        !node.finishedGeneration
      case _ =>
        true
    }

    val view = Mat4f(camera.view)
    val projection = Mat4f(camera.projection)

    val magicNr = Config.occlusionTestMagicNumber.toInt
    var i = -1
    val (ungeneratedAreasTest,ungeneratedAreasSkip) = ungeneratedAreas.partition (
      area => {i += 1; i % magicNr == renderer.frameCount % magicNr}
    )

    frameState.occlusionQueryCount += ungeneratedAreasTest.size
    frameState.renderedBoxCount    += ungeneratedAreasSkip.size


    val buffer = BufferUtils.createIntBuffer( ungeneratedAreasTest.size )
    glGenQueries( buffer )
    val queries = new Query(buffer, ungeneratedAreasTest)

    occTest_program.use {
      occTest_vao.bind {
        if( !Config.visibleOcclusionTest )
          glColorMask(false,false,false,false)

        occTest_matrix := projection * view
        for((renderAreas, color) <- Seq( (generatingAreas, generatingColor), (ungeneratedAreasSkip,skipColor) )
            if renderAreas.size > 0 ) {
          occTest_u_tint := color

          val instance_positions = renderAreas.map( info => Vec4f(info.pos,0) )
          val instance_scales = renderAreas.map( info => info.size.toFloat )

          occTest_offset := instance_positions
          occTest_scale := instance_scales

          occTest_binding.writeChangedUniforms()

          glDrawArraysInstancedARB(GL_TRIANGLE_STRIP, 0, GlDraw.cubeTriangleStrip.positions.size, renderAreas.size)
        }

        for( (queryId, info) <- queries ) {
          glBeginQuery(GL_SAMPLES_PASSED, queryId )

          occTest_u_tint := testColor
          occTest_offset := Seq( Vec4f( info.pos, 0) )
          occTest_scale := Seq( info.size.toFloat )

          occTest_binding.writeChangedUniforms()

          glDrawArraysInstancedARB(GL_TRIANGLE_STRIP, 0, GlDraw.cubeTriangleStrip.positions.size, 1)

          glEndQuery(GL_SAMPLES_PASSED)
        }
      }
      glColorMask(true, true, true, true)
    }

    queries
  }

  case class QueryResult(visible:Seq[PowerOfTwoCube], occluded:Seq[PowerOfTwoCube])

  var query:Query = null

  class Query(val buffer:IntBuffer,val nodeInfos:Seq[PowerOfTwoCube]) extends IndexedSeq[(Int,PowerOfTwoCube)] {
    require(buffer.limit == nodeInfos.length)
    val length    = buffer.limit()
    def apply(i:Int) = Tuple2(buffer.get(i), nodeInfos(i))
  }

  def evalQueries(queries:Query) = {
    val visible  = ArrayBuffer[PowerOfTwoCube]()
    val occluded = ArrayBuffer[PowerOfTwoCube]()

    for( (id,info) <- queries ) {
      if( frameState.workersBusy ) {
        if( glGetQueryObjectui(id, GL_QUERY_RESULT) > Config.occlusionTestThreshold )
          visible += info
      }
      else
        if( glGetQueryObjectui(id, GL_QUERY_RESULT) > 0 )
          visible += info
      //else
        //occluded += info
    }

    //log.println( s"occlusion query result (${queries.buffer.limit}):\noccluded: ${occluded.size}, visible: ${visible.size}" )
    glDeleteQueries(queries.buffer)

    QueryResult(visible, occluded)
  }


  def doIt(frustumTest:FrustumTest) {
    if( query != null )
      for( result <- evalQueries(query).visible )
        octree.generateArea(result)
    query = findUngeneratedNodes(frustumTest)
  }

}
