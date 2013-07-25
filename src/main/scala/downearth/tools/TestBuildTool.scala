package downearth.tools

import downearth.worldoctree._
import downearth.Player
import downearth.GameState

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
class TestBuildTool(val gameState:GameState) extends EnvironmentTool {
  import gameState._

  val texturePos = Vec2(0,0.5)
  val textureSize = Vec2(0.5)

  def ray = player

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
    octree(pos) == EmptyLeaf &&
    octree(pos - Vec3i.UnitZ) == FullLeaf
  }

  override def action(pos:Vec3i) {
    if( testPosition(pos) )
      octree(pos) = testThing
  }
}
