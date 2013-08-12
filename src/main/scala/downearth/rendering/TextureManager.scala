package downearth.rendering

import glwrapper.{Texture, TextureLoader}
import downearth.GameState
import java.nio.file._

class TextureManager(gameState:GameState) {
  import gameState._

  private val loader = new TextureLoader
  import loader._
	
	lazy val box              = readImageRaster("box.png")
  lazy val skybox2          = loadAsSkybox("skybox/stormydays","jpg")
  lazy val skybox           = loadAsSkybox("skybox/Above_The_Sea","jpg")
  lazy val materials        = resources.textures map loadAsTexture
  lazy val materialsArray   = Texture.create2DArray(resources.textures map readImageRaster)
	lazy val tools            = loadAsTexture("werkzeug.png")

  import java.nio.file.StandardWatchEventKinds._
  try {
    val watcher = FileSystems.getDefault.newWatchService()
      val uri = getClass.getClassLoader.getResource("mat_2.png").toURI
      val dir = Paths.get(uri)
      dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY)

      var result = watcher.poll()

      while(result != null) {
        println(result)
        result = watcher.poll()
      }

      println("watcher")
  }
  catch {
    case e:Throwable => println(e)
  }

  def delete() {
    // TODO resourcen wieder freigeben
  }
}

