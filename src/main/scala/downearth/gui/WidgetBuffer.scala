package downearth.gui

import collection.mutable.Buffer
import scala.collection.TraversableOnce

/**
 * User: arne
 * Date: 26.05.13
 * Time: 19:54
 */

trait WidgetBuffer extends Buffer[Widget] {
  def parent:Panel

  abstract override def update(n: Int, newelem: Widget) {
    newelem.parent = parent
    super.update(n,newelem)
  }

  abstract override def +=(elem: Widget) = {
    elem.parent = parent
    super.+=(elem)
  }

  abstract override def ++= (xs: TraversableOnce[Widget]) = {
    xs.foreach( w => this += w )
    this
  }

  abstract override def ++=: (xs: TraversableOnce[Widget]) = {
    xs.foreach( _ +=: this )
    this
  }

  abstract override def clear() {
    foreach( _.parent = null )
    super.clear()
  }

  abstract override def +=:(elem: Widget) = {
    elem.parent = parent
    super.+=:(elem)
  }

  abstract override def insertAll(n: Int, elems: Traversable[Widget]) {
    elems.foreach(_.parent = parent)
    super.insertAll(n,elems)
  }

  abstract override def remove(n: Int): Widget = {
    apply(n).parent = null
    super.remove(n)
  }
}
