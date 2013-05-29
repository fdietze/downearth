package downearth.rendering

object TextureManager {
  private val loader = new TextureLoader
  import loader.loadAsTexture
	
	lazy val box       = loadAsTexture("box.png")
	lazy val skybox    = loadAsTexture("stormydays_rearrange_lowres.jpg")
	lazy val materials = loadAsTexture("materials.png")
	lazy val tools     = loadAsTexture("werkzeug.png")

  def delete() {
    // TODO resourcen wieder freigeben
  }
}

