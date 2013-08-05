package downearth.rendering

import glwrapper.{Texture, TextureLoader}

object TextureManager {
  private val loader = new TextureLoader
  import loader._
	
	lazy val box              = loadAsTexture("box.png")
	lazy val skybox           = loadAsTexture("stormydays_rearrange_lowres.jpg")
  lazy val skybox2          = loadAsSkybox("stormydays","jpg")
	lazy val tools            = loadAsTexture("werkzeug.png")
  lazy val materials = {
    val surfaces = downearth.resources.Resources.textures map readImageRaster
    Texture.create2DArray(surfaces)
  }


  def delete() {
    // TODO resourcen wieder freigeben
  }
}

