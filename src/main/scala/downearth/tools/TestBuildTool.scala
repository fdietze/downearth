package downearth.tools

import downearth.worldoctree._
import downearth.world.World
import downearth.Player

import simplex3d.math.double._
import simplex3d.math.Vec3i
import downearth.rendering._

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 5/27/13
 * Time: 4:59 PM
 * To change this template use File | Settings | File Templates.
 */
object TestBuildTool extends EnvironmentTool {
  val texturePos = Vec2(0,0.5)
  val textureSize = Vec2(0.5)

  def ray = Player

  val drawTransparent = true

  def top = true

  val testThing = new ObjLeaf( ObjManager.testMesh )

  override def renderPreview(pos:Vec3i, di:Draw) {
    di.renderPolyeder(FullHexaeder, pos, Vec4(1) )
  }

  override def action(pos:Vec3i) {
    println("inserting at selectPos")
    selectPos foreach ( pos =>
      World(pos) = testThing
    )
  }
}
