package downearth.tools

import downearth.worldoctree._
import downearth.{DisplayEventManager, Player}
import downearth.world.World
import downearth.generation.WorldDefinition
import downearth.rendering.Draw

import simplex3d.math.double._
import simplex3d.math.double.functions._
import simplex3d.math.Vec3i

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 5/27/13
 * Time: 4:59 PM
 * To change this template use File | Settings | File Templates.
 */

object ConstructionTool extends EnvironmentTool {
  val texturePos = Vec2(0)
  val textureSize = Vec2(0.5)

  // adds a block
  override def top = true
  override def ray = Player

  val full = FullHexaeder
  val half = new Hexaeder(Z = 0x44440000)
  val quarter = new Hexaeder(Z = 0x44440000, Y = 0x44004400)
  val eighth = new Hexaeder(0x40404040,0x44004400,0x44440000)
  val rampA = new Hexaeder(Z = 0x00440000)
  val rampB = new Hexaeder(Z = 0x44880000)
  val rampC = new Hexaeder(Z = 0x00880000)
  val test = new Polyeder10

  def makeRotations(h:Polyeder) = {
    val h1 = h.rotateZ
    val h2 = h1.rotateZ
    val h3 = h2.rotateZ
    Vector(h,h1,h2,h3)
  }

  val all = Vector(full,half,quarter,eighth,rampA,rampB,rampC) map makeRotations
  private var m_id = 0
  def id_= ( new_id:Int ) { m_id = clamp(new_id,0,all.size-1) }
  def id = m_id

  def rotation = {
    (math.atan2(ray.dir.y, ray.dir.x) * 2 / math.Pi + 5.5).toInt % 4
  }

  def current = all(id)(rotation)

  var selectedMaterial = 3

  override def action(pos:Vec3i) = {
    if( Player.inventory.materials(selectedMaterial) >= current.volume ) {
      val block = World.octree(pos)
      //TODO: the evaluation of the materialfunction should be in the Leaf itself
      val material = if( block.material == -1 ) WorldDefinition.material(Vec3(pos) + 0.5).id else block.material
      Player.inventory.materials(material) += block.h.volume
      Player.inventory.materials(selectedMaterial) -= current.volume
      World(pos) = Leaf(current, selectedMaterial)
    }
    else {
      DisplayEventManager.showEventText("Not enough Material " + selectedMaterial + ".")
    }
  }

  override def renderPreview(pos:Vec3i, di:Draw) {
    di.renderPolyeder(current, pos, Vec4(1) )
  }
}
