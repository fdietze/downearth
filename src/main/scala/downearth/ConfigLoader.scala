package downearth

import org.lwjgl.input.Keyboard.getKeyIndex
import org.lwjgl.input.Keyboard
import java.util.prefs.Preferences

class ConfigLoader( configObject:AnyRef ) {
  val preferences = Preferences.userNodeForPackage(configObject.getClass)

  def load() {
    val fields = configObject.getClass.getDeclaredFields
    val keyFields = fields.filter( _.getName.startsWith("key") )

    for( field <- keyFields ) {
      val name = field.getName
      val key = loadKey(name)
      if( key.isDefined ) {
        field.setAccessible(true)
        field.setInt(configObject, key.get)
      }
    }
  }

  def save() {
    val fieleds = configObject.getClass.getDeclaredFields
    val (keyFields,nonKeyFields) = fieleds.partition( _.getName.startsWith("key") )

    for( field <- keyFields ) yield {
      field.setAccessible(true)
      val fieldName = field.getName
      val keyName = Keyboard.getKeyName(field.getInt(configObject))
      preferences.put(fieldName, keyName)
    }

    preferences.sync()
  }
	
	def loadKey(name:String):Option[Int] = {
    val keyName = preferences.get(name,"")
    if( keyName != "" ) {
      val key = getKeyIndex(keyName)
      if(key != Keyboard.KEY_NONE)
        Some(key)
      else {
        System.err.println("Wrong Format in preferences for key " + name)
        None
      }
    }
    else
      None
	}

}

