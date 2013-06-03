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

  val badColor  =  Vec4(1,0,0,1)
  val goodColor = Vec4(0,1,0,1)

  override def renderPreview(pos:Vec3i, di:Draw) {
    val color = if( testPosition(pos) ) goodColor else badColor
    di.renderPolyeder(FullHexaeder, pos, color)

  }

  def testPosition(pos:Vec3i) = {
    World(pos) == EmptyLeaf &&
    World(pos - Vec3i.UnitZ) == FullLeaf
  }

  override def action(pos:Vec3i) {
    if( testPosition(pos) )
      World(pos) = testThing
  }
}
