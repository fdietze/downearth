package downearth.rendering.shader

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 7/4/13
 * Time: 2:01 PM
 * To change this template use File | Settings | File Templates.
 */
trait GlObject {
  var id:Int
  def create():this.type
  def delete()
}
