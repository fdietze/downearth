package downearth.rendering

import glwrapper.{Texture, TextureLoader}

object TextureManager {
  private val loader = new TextureLoader
  import loader._
	
	lazy val box              = loadAsTexture("box.png")
	lazy val skybox           = loadAsTexture("stormydays_rearrange_lowres.jpg")
  lazy val skybox2          = loadAsSkybox("stormydays","jpg")
	lazy val materials        = loadAsTexture("materials.png")
	lazy val tools            = loadAsTexture("werkzeug.png")
  lazy val textureArrayTest = {


    Array("stormydays_negativeX.jpg","stormydays_negativeY.jpg","stormydays_negativeZ.jpg","stormydays_positiveX.jpg","stormydays_positiveY.jpg")
    val s6 = readImageRaster("stormydays_positiveZ.jpg")

    val surfaces = Array("stormydays_negativeX.jpg","stormydays_negativeY.jpg","stormydays_negativeZ.jpg",
                         "stormydays_positiveX.jpg","stormydays_positiveY.jpg","stormydays_positiveZ.jpg") map
      readImageRaster _

    Texture.create2DArray(surfaces)
  }


  def delete() {
    // TODO resourcen wieder freigeben
  }
}

