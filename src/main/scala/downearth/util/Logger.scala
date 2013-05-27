package downearth.util

import java.io.{OutputStream, PrintStream}

/**
 * User: arne
 * Date: 02.05.13
 * Time: 23:56
 */

class NullOutputStream extends OutputStream {
  override def write(b:Int) {
  }

  override def write(b:Array[Byte]) {
  }

  override def write(b:Array[Byte], off:Int, len:Int) {
  }
}

trait Logable {
  def log:PrintStream
}

trait NullLogger extends Logable {
  val logStream = new PrintStream(new NullOutputStream)
  def log = logStream
}

trait Logger extends Logable {
  def logStream:PrintStream = System.out

  val prefix = getClass.getName.split('.').last

  def log = {
    logStream.print(prefix)
    logStream.print(": ")
    logStream
  }
}
