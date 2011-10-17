package openworld

import xml.XML
import org.lwjgl.input.Keyboard.getKeyIndex

object ConfigLoader {
	val config = try {
		Some( XML.load( getClass.getClassLoader.getResourceAsStream("config.xml") ) )
	}
	catch {
		case _ => 
			System.err.println("keine config.xml angegeben, oder konnte nicht gefunden werden")
			None
	}
	
	def loadKey(name:String):Option[Int] = {
		config match {
		case Some(config) =>
			config \ "keys" \ "key" find ( node => (node \ "@name").text == name) match {
			case Some(node) => 
				val key = getKeyIndex(node.text)
				if(key != 0)
					Some(key)
				else {
					System.err.println("Wrong Format in config.xml for key " + name)
					None
				}
			case None =>
				None
			}
		case None =>
			None
		}
	}
	
	def loadValue(name:String):Option[String] = {
		if(config ne None)
			config.get \ "value" find ( node => (node \ "@name").text == name ) map ( _.text )
		else
			None
	}
	
	def loadBoolean(name:String):Option[Boolean] = 
		loadValue(name) match {
		case Some("false") => Some(false)
		case Some("true")  => Some(true)
		case Some(s)       => System.err.println("can't parse " + s + " as Boolean for key " + name); None
		case _ => None
	}
	
	def loadInt(name:String):Option[Int] = {
		val option = loadValue(name)
		try {
			loadValue(name) map ( _.toInt )
		}
		catch {
			case _ => 
				System.err.println("can't parse " + option.get + " as Int for key " + name)
				None
		}
	}
}

