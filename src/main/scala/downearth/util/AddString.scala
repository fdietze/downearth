package downearth.util

/**
 * User: arne
 * Date: 03.06.13
 * Time: 00:07
 */
trait AddString {
  override def toString = addString(new StringBuilder).result

  def addString(sb:StringBuilder):StringBuilder
}
