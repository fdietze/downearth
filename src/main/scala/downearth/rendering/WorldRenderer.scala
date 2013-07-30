package downearth.rendering

import glwrapper.Program
import downearth.{Camera, Camera3D}
import simplex3d.math.floatx.{Vec3f, Vec4f}

/**
 * Created with IntelliJ IDEA.
 * User: felix
 * Date: 7/30/13
 * Time: 8:39 PM
 * To change this template use File | Settings | File Templates.
 */
object WorldRenderer {
  val occTest_program = Program.auto("simple")

  val binding = occTest_program.getBinding

  val position = binding.attributeVec3f("position")
  val normal_ws = binding.attributeVec2f("normal_ws")
  val texCoord = binding.attributeVec3f("texCoord")

  val pvm = binding.uniformMat4f("pvm")

  val image = binding.uniformSampler2DArray("image")
  val ambient = binding.uniformVec3f("ambient")
  val sunColor = binding.uniformVec3f("sunColor")
  val sunDir_ws = binding.uniformVec3f("sunDir_ws")

  image :=

  ambient := Vec3f(0.3f, 0.2f, 0.1f)
  sunColor := Vec3f(0.7f, 0.8f, 0.9f)
  sunDir_ws := Vec3f(0,0,-1)

  def render( camera:Camera ) {
    val view = camera.view
    val projection = camera.projection
    pvm := projection * view;





  }

}
