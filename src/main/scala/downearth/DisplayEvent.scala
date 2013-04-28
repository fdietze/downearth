package downearth

import scala.collection.mutable._
import Config._

object DisplayEventManager{
	val eventTime =  5000L // TODO in die Config
	val events = new Array[DisplayEvent](10)
	var pos = 0
	
	def showEventText(str:String){
		events(pos) = DisplayEvent(str, System.currentTimeMillis)
		pos = (pos+1) % events.length
		println(str)
	}
	
	def draw {
		var hasEvent = false 
		val currentTime = System.currentTimeMillis
		for(i <- 0 until events.length) {
			if(events(i) ne null){
				if( events(i).startTime + eventTime >= currentTime ){
					Draw.drawDisplayEvent(events(i),i)
					hasEvent = true
				}
				else {
					events(i) = null
				}
			}
		}
		
		if( !hasEvent )
			pos = 0
	}
}

case class DisplayEvent(textMessage:String,startTime:Long)
