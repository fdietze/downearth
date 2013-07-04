package downearth.rendering

import downearth.rendering.shader.{VertexArrayObject, Program}
import downearth.{FrustumTest, Camera, Config}
import org.lwjgl.opengl.GL11._
import simplex3d.math.floatx.{Mat4f, Vec4f}
import downearth.worldoctree._
import org.lwjgl.opengl.GL15._
import org.lwjgl.BufferUtils
import scala.collection.mutable.ArrayBuffer
import java.nio.IntBuffer
import downearth.world.World
import simplex3d.backend.lwjgl.ArbEquivalents

/**
 * User: arne
 * Date: 05.07.13
 * Time: 18:54
 */
object OcclusionTest {

  lazy val occTest_program = Program.auto("simple")

  val occTest_binding = occTest_program.getBinding

  val occTest_matrix  = occTest_binding.uniformMat4f("matrix")
  val occTest_offset  = occTest_binding.attributeVec4f("offset")
  val occTest_scale   = occTest_binding.attributeFloat("scale")
  val occTest_u_tint  = occTest_binding.uniformVec4f("u_tint")
  val occTest_a_pos   = occTest_binding.attributeVec4f("a_pos")

  occTest_a_pos := GlDraw.texturedCubeBuffer.positions

  val occTest_vao = VertexArrayObject.create

  occTest_vao.bind {
    occTest_offset.divisor = 1
    occTest_scale.divisor = 1

    occTest_binding.enableAttributes()
    occTest_binding.setAttributePointers()
  }

  def findUngeneratedNodes(camera:Camera, octree:WorldOctree, test:FrustumTest) = {
    TextureManager.box.bind()

    val nodeInfoBufferGenerating  = ArrayBuffer[PowerOfTwoCube]()
    val nodeInfoBufferUngenerated = ArrayBuffer[PowerOfTwoCube]()

    octree.queryRegion( test, camera.position) {
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

    val view = Mat4f(camera.view)
    val projection = Mat4f(camera.projection)

    val magicNr = 8
    val (doTest,noTest) = (0 until nodeInfoBufferUngenerated.size).partition(i => i % magicNr == Renderer.frameCount % magicNr)

    val renderNodeInfos1 = nodeInfoBufferGenerating
    val renderNodeInfos2 = noTest map nodeInfoBufferUngenerated
    val reducedNodeInfos = doTest map nodeInfoBufferUngenerated

    val buffer = BufferUtils.createIntBuffer( reducedNodeInfos.size )
    glGenQueries( buffer )
    val queries = new Query(buffer, reducedNodeInfos)

    occTest_program.use {
      occTest_vao.bind {
        if( !Config.visibleOcclusionTest )
          glColorMask(false,false,false,false)

        occTest_matrix := projection * view

        for( (tint, renderNodeInfos) <- Seq[(Vec4f,Seq[PowerOfTwoCube])]( (Vec4f(1,1,0,1), renderNodeInfos1), (Vec4f(0,1,0,1), renderNodeInfos2) ) if renderNodeInfos.size > 0 ) {
          occTest_u_tint := tint

          val instance_positions = renderNodeInfos.map( info => Vec4f(info.pos,0) )
          val instance_scales = renderNodeInfos.map( info => info.size.toFloat )

          occTest_offset := instance_positions
          occTest_scale := instance_scales

          occTest_binding.writeChangedUniforms()

          ArbEquivalents.GL31.glDrawArraysInstanced(GL_QUADS, 0, 24, renderNodeInfos.size)
        }

        for( (queryId, info) <- queries ) {
          glBeginQuery(GL_SAMPLES_PASSED, queryId )

          occTest_u_tint := Vec4f(1,0,0,1)
          occTest_offset := Seq( Vec4f( info.pos, 0) )
          occTest_scale := Seq( info.size.toFloat )

          occTest_binding.writeChangedUniforms()

          ArbEquivalents.GL31.glDrawArraysInstanced(GL_QUADS, 0, 24, 1)

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


  def doIt(camera:Camera, frustumTest:FrustumTest) {
    if( query != null )
      for( result <- evalQueries(query).visible )
        World.octree.generateNode(result)
    query = findUngeneratedNodes(camera, World.octree, frustumTest)
  }

}
