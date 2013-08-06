package downearth.rendering

import glwrapper.{VertexArrayObject, Program}
import downearth._
import simplex3d.math.floatx.{Vec3f, Vec4f}
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL20._
import simplex3d.math.double._
import downearth.worldoctree._
import downearth.entity.{Entity, SimpleEntity}
import org.lwjgl.BufferUtils
import downearth.util._
import org.lwjgl.opengl.ARBBufferObject._
import scala.collection.mutable.ArrayBuffer
import org.lwjgl.opengl.GL15._
import downearth.worldoctree.PowerOfTwoCube
import org.lwjgl.opengl.Util
import downearth.worldoctree.PowerOfTwoCube
import downearth.rendering.TextureMesh._
import downearth.worldoctree.PowerOfTwoCube
import java.nio.ByteBuffer

/**
 * Created with IntelliJ IDEA.
 * User: felix
 * Date: 7/30/13
 * Time: 8:39 PM
 * To change this template use File | Settings | File Templates.
 */
class WorldRenderer(gameState:GameState) {
  import gameState.textureManager

  val program = Program.auto("world")

  val binding = program.getBinding
  println( binding )
  val vao = VertexArrayObject.create
  val position = binding.attributeVec3f("position")
  val normal_ws = binding.attributeVec3f("normal_ws")
  val texCoord = binding.attributeVec3f("texCoord")

  vao.bind {
    binding.enableAttributes()
  }

  val pvm = binding.uniformMat4f("pvm")
  val image = binding.uniformSampler2DArray("image")
  val ambient = binding.uniformVec3f("ambient")
  val sunColor = binding.uniformVec3f("sunColor")
  val sunDir_ws = binding.uniformVec3f("sunDir_ws")

  image := textureManager.materialsArray
  ambient := Vec3f(0.3f, 0.2f, 0.1f)
  sunColor := Vec3f(0.7f, 0.8f, 0.9f)
  sunDir_ws := Vec3f(0.0f, 0.0f, -1.0f)

  val lightPos = BufferUtils.createFloatBuffer(4)
  val ambientLight = BufferUtils.createFloatBuffer(4)
  ambientLight.put( Array(0.2f, 0.2f, 0.2f, 1f) )
  ambientLight.rewind()

  var drawCalls = 0
  var emptyDrawCalls = 0

  def renderWorld(camera:Camera) {

    val view = camera.view
    val projection = camera.projection
    pvm := projection * view

    if(Config.backFaceCulling) glEnable(GL_CULL_FACE) else glDisable(GL_CULL_FACE)

    val frustumTest = new FrustumTestImpl(Mat4(camera.projection), Mat4(camera.view))

    drawCalls = 0
    emptyDrawCalls = 0

    glEnable(GL_DEPTH_TEST)

    if( Config.frustumCulling )
      drawOctree(frustumTest)
    else
      drawOctree(new FrustumTest { def testNode( info:PowerOfTwoCube ) = true })

    glEnable(GL_COLOR_MATERIAL)
    glEnable(GL_TEXTURE_2D)

    glMatrixMode( GL_PROJECTION )
    glLoadMatrix( camera.projectionBuffer )

    glMatrixMode( GL_MODELVIEW )
    glLoadMatrix( camera.viewBuffer )

    gameState.dynamicWorld.entities foreach {
      case simple:SimpleEntity => ()
        glPushMatrix()
        glTranslated( simple.pos.x, simple.pos.y, simple.pos.z )
        drawObjMesh( simple.mesh )
        glPopMatrix()
      case entity:Entity => ()
    }

    gameState.mainWidget.drawCallLabel.text = s"draw calls: $drawCalls, empty: $emptyDrawCalls"
    gameState.mainWidget.playerPositionLabel.text = "Player Position: " + downearth.util.round10(gameState.player.pos)
  }



  def drawOctree(test:FrustumTest) {
    import org.lwjgl.opengl.GL11._


    val objMeshes = ArrayBuffer[(PowerOfTwoCube,ObjMesh)]()

    vao.bind {
      program.use {
        binding.writeChangedUniforms()

        gameState.octree.query( test, gameState.player.pos ) {
          case (info, node:MeshNode) =>
            drawTextureMesh(node.mesh)
            objMeshes ++= node.objMeshes
            false
          case _ => true
        }
      }
    }

    glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT)

    glEnableClientState(GL_VERTEX_ARRAY)
    glEnableClientState(GL_NORMAL_ARRAY)
    glEnableClientState(GL_TEXTURE_COORD_ARRAY)

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
    textureManager.box.bind {
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
  }

  def drawTextureMesh(mesh:TextureMesh) {
    if( !mesh.hasVbo )
      mesh.genvbo()

    if( mesh.nonEmpty ) {
      drawCalls += 1

      val buffer = new glwrapper.ArrayBuffer
      buffer.id = mesh.vbo

      buffer.bind{
        glVertexAttribPointer(position.location, vertexComponents, vertexType, false, byteStride, vertexOffset)
        //println(buffer.id)
        //println(position.boundString)
        if(normal_ws.location != -1){
          glVertexAttribPointer(normal_ws.location, normalComponents, normalType, false, byteStride, normalOffset)
          //println(normal_ws.boundString)
        }
        if(texCoord.location != -1) {
          texCoord.enable()
          glVertexAttribPointer(texCoord.location, texCoordComponents, texCoordType, false, byteStride, texCoordOffset)
          //println(texCoord.boundString)
        }

        /*val data:ByteBuffer = buffer.getData
        for( i <- 0 until mesh.vertexCount ) {
          println("checking Vertex for NaN")
          val v1 = glwrapper.util.getVec3f(data, Vec3f(0))
          val v2 = glwrapper.util.getVec3f(data, Vec3f(0))
          val v3 = glwrapper.util.getVec3f(data, Vec3f(0))

          assert( ! v1.x.isNaN && ! v1.y.isNaN && ! v1.z.isNaN )
          assert( ! v2.x.isNaN && ! v2.y.isNaN && ! v2.z.isNaN )
          assert( ! v3.x.isNaN && ! v3.y.isNaN && ! v3.z.isNaN )
        }*/

      }

      glDrawArrays(GL_TRIANGLES, 0, mesh.vertexCount)
      mesh.lastDraw = System.nanoTime
    }
    else {
      emptyDrawCalls += 1
    }

    Util.checkGLError()
  }

}
