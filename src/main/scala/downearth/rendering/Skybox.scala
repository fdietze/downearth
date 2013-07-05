package downearth.rendering

import org.lwjgl.opengl._
import GL11._

import simplex3d.math.floatx._

import downearth.Camera
import downearth.rendering.shader._

object Skybox {
  val skybox_program = Program.auto("skybox")
  val skybox_binding = skybox_program.getBinding
  val skybox_vao = VertexArrayObject.create

  val skybox_position = skybox_binding.attributeVec4f("position")
  val skybox_matrix   = skybox_binding.uniformMat4f("matrix")
  val skybox_cubemap  = skybox_binding.uniformSamplerCube("cubemap")

  skybox_cubemap := TextureManager.skybox2

  skybox_vao.bind {
    skybox_binding.enableAttributes()
    skybox_binding.setAttributePointers()
  }

  skybox_position := Seq(
    Vec4f(1,1,1,1), Vec4f(-1,1,1,1), Vec4f(-1,1,-1,1), Vec4f(1,1,-1,1),
    Vec4f(-1,1,1,1), Vec4f(-1,-1,1,1), Vec4f(-1,-1,-1,1), Vec4f(-1,1,-1,1),
    Vec4f(-1,-1,1,1), Vec4f(1,-1,1,1), Vec4f(1,-1,-1,1), Vec4f(-1,-1,-1,1),
    Vec4f(1,-1,1,1), Vec4f(1,1,1,1), Vec4f(1,1,-1,1), Vec4f(1,-1,-1,1),
    //top
    Vec4f(1,-1,1,1), Vec4f(-1,-1,1,1), Vec4f(-1,1,1,1), Vec4f(1,1,1,1),
    //bottom
    Vec4f(1,1,-1,1), Vec4f(-1,1,-1,1), Vec4f(-1,-1,-1,1), Vec4f(1,-1,-1,1)
  )
	
	def render( camera:Camera ) {
    glDisable(GL_DEPTH_TEST)

    val projection = Mat4f(camera.projection)
    val view = Mat4f(camera.noTranslate)

    val matrix = projection * view

    skybox_vao.bind {
      skybox_program.use {
        skybox_matrix := matrix
        skybox_binding.writeChangedUniforms()

        glDrawArrays(GL_QUADS, 0, 6*4)
      }
    }
	}
}

