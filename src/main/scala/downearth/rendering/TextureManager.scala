package downearth.rendering

import glwrapper.TextureLoader

object TextureManager {
  private val loader = new TextureLoader
  import loader._
	
	lazy val box              = loadAsTexture("box.png")
	lazy val skybox           = loadAsTexture("stormydays_rearrange_lowres.jpg")
  lazy val skybox2          = loadAsSkybox("stormydays","jpg")
	lazy val materials        = loadAsTexture("materials.png")
	lazy val tools            = loadAsTexture("werkzeug.png")
  lazy val textureArrayTest = {


    val s1 = readImageRaster("stormydays_negativeX.jpg")
    val s2 = readImageRaster("stormydays_negativeY.jpg")
    val s3 = readImageRaster("stormydays_negativeZ.jpg")
    val s4 = readImageRaster("stormydays_positiveX.jpg")
    val s5 = readImageRaster("stormydays_positiveY.jpg")
    val s6 = readImageRaster("stormydays_positiveZ.jpg")
  }


  def delete() {
    // TODO resourcen wieder freigeben
  }
}

