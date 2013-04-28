/**
 * User: arne
 * Date: 26.04.13
 * Time: 01:14
 */

package downearth

import org.lwjgl.opengl.GL11._
import openworld.Config._
import simplex3d.math.double._
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.{ARBFragmentShader, ARBVertexShader, ARBShaderObjects}
import openworld.Util._
import org.lwjgl.opengl.ARBBufferObject._
import org.lwjgl.opengl.ARBVertexBufferObject._
import openworld.gui.GUI

object Renderer {

  val lightPos = BufferUtils.createFloatBuffer(4)
  val ambientLight = BufferUtils.createFloatBuffer(4)
  ambientLight.put( Array(0.2f, 0.2f, 0.2f, 1f) )
  ambientLight.rewind()

  var shader = 0
  var vertShader = 0
  var fragShader = 0

  def draw() {
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT )

    renderScene( Player.camera )

    GUI.renderScene
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
    printLogInfo(shader)
    printLogInfo(vertShader)
    printLogInfo(fragShader)
  }

  def lighting( position:Vec3 ) {
    if( wireframe ) {
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

  def renderScene(camera:Camera) {
    glViewport(0, 0, JavaFxMain.width.toInt, JavaFxMain.height.toInt)
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

    drawWorld(frustumTest)
    Player.activeTool.draw

    if(Config.debugDraw) {
      BulletPhysics.debugDrawWorld
      Draw.drawSampledNodes
    }
  }

  var drawcalls = 0
  var emptydrawcalls = 0

  def drawWorld(test:FrustumTest) {
    drawcalls = 0
    emptydrawcalls = 0

    drawOctree(World.octree,test)
    // TODO this is not render code
    if(Config.streamWorld)
      World.octree stream Player.position

  }

  def drawOctree(octree:WorldOctree, test:FrustumTest) {
    // TODO this is not render code
    octree.makeUpdates

    import org.lwjgl.opengl.GL11._
    glColor3f(1,1,1)

    octree.rootB.foreachMeshNode( octree.rootNodeInfo, test , (node:MeshNode) => drawTextureMesh(node.mesh) )

    if(Config.debugDraw) {
      glDisable(GL_LIGHTING)
      glDisable(GL_TEXTURE_2D)

      glPushMatrix
      glTranslate3dv(Vec3(octree.rootNodePos))
      glColor3f(1,0,0)
      Draw.renderCube(octree.rootNodeSize)
      glPopMatrix

      glPushMatrix
      glTranslate3dv(octree.worldWindowPos + 0.05)
      glColor3f(0,1,0)
      Draw.renderCube(worldWindowSize - 0.1)
      glPopMatrix
    }
  }

  def drawTextureMesh(mesh:TextureMesh) {
    //TextureManager.box.bind
    TextureManager.materials.bind

    if(mesh.vertexBufferObject == 0)
      mesh.genvbo

    if( mesh.size > 0 ) {
      drawcalls += 1
      glBindBufferARB(GL_ARRAY_BUFFER_ARB, mesh.vertexBufferObject)

      glEnableClientState(GL_VERTEX_ARRAY)
      glEnableClientState(GL_NORMAL_ARRAY)
      glEnableClientState(GL_TEXTURE_COORD_ARRAY)
      //glEnableClientState(GL_COLOR_ARRAY)

      glVertexPointer(mesh.vertices.components, mesh.vertices.rawEnum, mesh.vertices.byteStride, mesh.vertices.byteOffset)
      glNormalPointer(mesh.normals.rawEnum, mesh.normals.byteStride, mesh.normals.byteOffset)
      glTexCoordPointer(mesh.texcoords.components, mesh.texcoords.rawEnum, mesh.texcoords.byteStride, mesh.texcoords.byteOffset)
      //glColorPointer(colors.components, colors.rawType, colors.byteStride, colors.byteOffset)

      glDrawArrays(GL_TRIANGLES, 0, mesh.vertices.size)

      glDisableClientState(GL_VERTEX_ARRAY)
      glDisableClientState(GL_NORMAL_ARRAY)
      glDisableClientState(GL_TEXTURE_COORD_ARRAY)
      //glDisableClientState(GL_COLOR_ARRAY)

      glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0)
    }
    else {
      emptydrawcalls += 1
    }
  }

  def activateShader(func: => Unit) {
    import ARBShaderObjects._

    val useshaders = shader != 0 && vertShader != 0 && fragShader != 0

    if(useshaders) {
      glUseProgramObjectARB(shader)
      // glUniform1fARB( glGetUniformLocationARB( shader, "time" ), ( time / 1000.0 ).toFloat )
    }

    func

    if(useshaders)
      glUseProgramObjectARB(0)
  }
}


