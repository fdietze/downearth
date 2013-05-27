package downearth.tools

import downearth.worldoctree._
import downearth.Player
import downearth.world.World
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
object Shovel extends EnvironmentTool {
  // removes a block
  val texturePos = Vec2(0.5, 0)
  val textureSize = Vec2(0.5)

  val drawTransparent = true

  def ray = Player
  def top = false

  override def renderPreview(pos:Vec3i, di:Draw) {
    val leaf = World.octree(pos)
    di.renderPolyeder( leaf.h , pos, Vec4(1) )
  }

  override def action(pos:Vec3i) {
    val block = World.octree(pos)
    // TODO: the evaluation of the materialfunction should be in the Leaf itself
    val material = if( block.material == -1 ) WorldDefinition.material(Vec3(pos) + 0.5).id else block.material
    Player.inventory.materials(material) += block.h.volume
    World(pos) = EmptyLeaf
  }

  override def toString = getClass.getName.split('.').last
}
