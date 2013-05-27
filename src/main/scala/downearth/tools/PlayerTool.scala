package downearth.tools

import simplex3d.math.Vec3i
import simplex3d.math.double.{Vec2, Vec3}

import downearth.worldoctree.Polyeder
import downearth.gui.MainWidget
import downearth.world.World
import downearth.Player
import downearth.util.Ray
import downearth.rendering.Draw

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 5/27/13
 * Time: 4:57 PM
 * To change this template use File | Settings | File Templates.
 */

trait PlayerTool {
  def action()
  def ray:Ray

  def texturePos:Vec2
  def textureSize:Vec2
}

trait EnvironmentTool extends PlayerTool {
  def action( pos:Vec3i )

  override def action() {
    val selected = selectPos
    if( selected.isDefined )
      action(selected.get)
  }

  def renderPreview(pos:Vec3i, di:Draw)

  final def renderPreview(di:Draw) {
    val selected = selectPos
      if( selected.isDefined )
        renderPreview(selected.get, di)
  }

  def top:Boolean
  def range = 10
  def selectPos = {
    if( MainWidget.mouseOver )
      World.octree.raytracer(ray, top, range)
    else
      None
  }
}

