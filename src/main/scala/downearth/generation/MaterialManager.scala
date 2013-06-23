package downearth.generation

import downearth.rendering.{Texture2D, TextureManager}
import simplex3d.math.double._

case class Material(id:Int, texture:Texture2D, texPos:Vec2, texSize:Vec2)
object Material {
  def apply(id:Int, r:Double, g:Double, b:Double):Material = MaterialManager.materials(id)
}

object MaterialManager {
  val inset = 1 // material separation texture atlas border for texture coordinates
  val textureAtlas = TextureManager.materials
  val materialCount = textureAtlas.width / textureAtlas.height
  val materials = Array.tabulate(materialCount){ id =>
    Material(id,
      texture = textureAtlas,
      texPos  = Vec2(id.toDouble/materialCount + inset,0 + inset),
      texSize = Vec2(1.0/materialCount - 2*inset,1 - 2*inset))
  }
}
