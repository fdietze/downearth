package downearth.rendering

import glwrapper.{VertexArrayObject, Program}
import downearth._
import simplex3d.math.floatx.{Vec3f, Vec4f}
import org.lwjgl.opengl.GL11._
import simplex3d.math.double._
import downearth.worldoctree._
import downearth.entity.{Entity, SimpleEntity}
import downearth.worldoctree.PowerOfTwoCube
import downearth.tools.EnvironmentTool
import downearth.worldoctree.PowerOfTwoCube
import org.lwjgl.BufferUtils
import downearth.util._
import downearth.worldoctree.PowerOfTwoCube
import org.lwjgl.opengl.ARBBufferObject._
import downearth.worldoctree.PowerOfTwoCube
import org.lwjgl.opengl.ARBVertexBufferObject._
import downearth.worldoctree.PowerOfTwoCube
import downearth.worldoctree.PowerOfTwoCube
import scala.collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: felix
 * Date: 7/30/13
 * Time: 8:39 PM
 * To change this template use File | Settings | File Templates.
 */
class WorldRenderer(gameState:GameState) {
  val program = Program.auto("simple")

  val binding = program.getBinding

  val vao = VertexArrayObject.create

  vao.bind {
    binding.enableAttributes()
  }

  val position = binding.attributeVec3f("position")
  val normal_ws = binding.attributeVec2f("normal_ws")
  val texCoord = binding.attributeVec3f("texCoord")

  val pvm = binding.uniformMat4f("pvm")

  val image = binding.uniformSampler2DArray("image")
  val ambient = binding.uniformVec3f("ambient")
  val sunColor = binding.uniformVec3f("sunColor")
  val sunDir_ws = binding.uniformVec3f("sunDir_ws")

  image := TextureManager.textureArrayTest

  ambient := Vec3f(0.3f, 0.2f, 0.1f)
  sunColor := Vec3f(0.7f, 0.8f, 0.9f)
  sunDir_ws := Vec3f(0.0f, 0.0f, -1.0f)

  val lightPos = BufferUtils.createFloatBuffer(4)
  val ambientLight = BufferUtils.createFloatBuffer(4)
  ambientLight.put( Array(0.2f, 0.2f, 0.2f, 1f) )
  ambientLight.rewind()

  def render( camera:Camera ) {
    val view = camera.view
    val projection = camera.projection
    pvm := projection * view

    program.use{
      binding.writeChangedUniforms()

      



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

  var drawCalls = 0
  var emptyDrawCalls = 0

  def renderWorld(camera:Camera) {
    if(Config.backFaceCulling) glEnable(GL_CULL_FACE) else glDisable(GL_CULL_FACE)
    glEnable(GL_COLOR_MATERIAL)
    glEnable(GL_TEXTURE_2D)
    glEnable(GL_DEPTH_TEST)

    glMatrixMode( GL_PROJECTION )
    glLoadMatrix( camera.projectionBuffer )

    glMatrixMode( GL_MODELVIEW )
    glLoadMatrix( camera.viewBuffer )

    lighting( camera.position )

    val frustumTest = new FrustumTestImpl(Mat4(camera.projection), Mat4(camera.view))

    drawCalls = 0
    emptyDrawCalls = 0

    if( Config.frustumCulling )
      drawOctree(frustumTest)
    else
      drawOctree(new FrustumTest { def testNode( info:PowerOfTwoCube ) = true })

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

  def drawObjMesh(mesh:ObjMesh) {
    TextureManager.box.bind {
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

  def drawOctree(test:FrustumTest) {
    import org.lwjgl.opengl.GL11._
    glColor3f(1,1,1)

    gameState.materialManager.textureAtlas.bind {
      glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT)

      glEnableClientState(GL_VERTEX_ARRAY)
      glEnableClientState(GL_NORMAL_ARRAY)
      glEnableClientState(GL_TEXTURE_COORD_ARRAY)

      val objMeshes = ArrayBuffer[(PowerOfTwoCube,ObjMesh)]()

      gameState.octree.query( test, gameState.player.pos ) {
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
  }

  def drawTextureMesh(mesh:TextureMesh) {
    if( !mesh.hasVbo )
      mesh.genvbo()

    if( mesh.nonEmpty ) {
      drawCalls += 1
      mesh.bind()
      import TextureMesh._
      glVertexPointer(  3,  vertexType, byteStride,  vertexOffset)
      glNormalPointer(      normalType, byteStride,  normalOffset)
      glTexCoordPointer(2,  texCoordType, byteStride,   texCoordOffset)
      glDrawArrays(GL_TRIANGLES, 0, mesh.vertexCount)
      glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0)

      mesh.lastDraw = System.nanoTime
    }

    else {
      emptyDrawCalls += 1
    }
  }

}
