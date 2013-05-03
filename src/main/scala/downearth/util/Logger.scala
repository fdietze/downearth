package downearth.util

import java.io.PrintStream

/**
 * User: arne
 * Date: 02.05.13
 * Time: 23:56
 */
trait Logger {
  def logStream:PrintStream = System.out

  val prefix = getClass.getName.split('.').last

  def log = {
    logStream.print(prefix)
    logStream.print(": ")
    logStream
  }
}
