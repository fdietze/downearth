package downearth.tools

import downearth.worldoctree._
import downearth.{GameState, Player}
import downearth.generation.WorldDefinition
import downearth.rendering.Draw

import simplex3d.math.double._
import simplex3d.math.Vec3i

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 5/27/13
 * Time: 4:59 PM
 * To change this template use File | Settings | File Templates.
 */
class Shovel(val gameState:GameState) extends EnvironmentTool {
  import gameState._

  // removes a block
  val texturePos = Vec2(0.5, 0)
  val textureSize = Vec2(0.5)

  val drawTransparent = true

  def ray = player
  def top = false

  override def renderPreview(pos:Vec3i, di:Draw) {
    val leaf = octree(pos)
    di.renderPolyeder( leaf.h , pos, Vec4(1) )
  }

  override def action(pos:Vec3i) {
    val block = octree(pos)
    // TODO: the evaluation of the materialfunction should be in the Leaf itself
    val material = if( block.material == -1 ) WorldDefinition.materialAtBlock(pos).id else block.material
    player.inventory.materials(material) += block.h.volume
    octree(pos) = EmptyLeaf
  }

  override def toString = getClass.getName.split('.').last
}
