package downearth.generation

import downearth.rendering.{Texture, TextureManager}
import simplex3d.math.double._

case class Material(id:Int, texture:Texture, texPos:Vec2, texSize:Vec2)

object MaterialManager {
  val textureAtlas = TextureManager.materials
  val materialCount = textureAtlas.width / textureAtlas.height
  val materials = Array.tabulate(materialCount){ id =>
    Material(id,
      texture = textureAtlas,
      texPos  = Vec2(id.toDouble/materialCount,0),
      texSize = Vec2(1.0/materialCount,1))
  }
}
