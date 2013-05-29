package downearth.rendering

import java.io.{File, FileInputStream}

/**
 * User: arne
 * Date: 12.05.13
 * Time: 14:49
 */
object ObjManager {
  val loader = new ObjLoader

  lazy val testMesh = loader.load(new FileInputStream(new File("thing.obj")))
  lazy val monkey = loader.load(new FileInputStream(new File("monkey.obj")))

  def delete() {
    // TODO resourcen wieder freigeben
  }
}
