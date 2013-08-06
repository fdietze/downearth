package downearth.rendering

import glwrapper.{Texture, TextureLoader}
import downearth.GameState

class TextureManager(gameState:GameState) {
  import gameState._

  private val loader = new TextureLoader
  import loader._
	
	lazy val box              = loadAsTexture("box.png")
  lazy val skybox          = loadAsSkybox("stormydays","jpg")
	lazy val tools            = loadAsTexture("werkzeug.png")
  lazy val materials = resources.textures map loadAsTexture
  lazy val materialsArray = {
    val surfaces = resources.textures map readImageRaster
    Texture.create2DArray(surfaces)
  }


  def delete() {
    // TODO resourcen wieder freigeben
  }
}

