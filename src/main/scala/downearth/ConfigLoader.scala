package downearth

import org.lwjgl.input.Keyboard.getKeyIndex
import org.lwjgl.input.Keyboard
import java.util.prefs.Preferences

class ConfigLoader( configObject:AnyRef ) {
  val preferences = Preferences.userNodeForPackage(configObject.getClass)

  def load() {
    val fields = configObject.getClass.getDeclaredFields
    val (keyFields,nonKeyFields) = fields.partition( _.getName.startsWith("key") )

    for( field <- keyFields ) {
      val name = field.getName
      val key = loadKey(name)
      if( key.isDefined ) {
        field.setAccessible(true)
        field.setInt(configObject, key.get)
      }
    }

    for( field <- nonKeyFields ) {
      field.setAccessible(true)
      if( field.get(configObject).isInstanceOf[Boolean] && !field.getName.contains("bitmap$") ) {
        val b = preferences.getBoolean(field.getName, field.getBoolean(configObject))
        field.setBoolean(configObject, b)
      }
    }
  }

  def save() {
    val fields = configObject.getClass.getDeclaredFields
    val (keyFields,nonKeyFields) = fields.partition( _.getName.startsWith("key") )

    for( field <- keyFields ) yield {
      field.setAccessible(true)
      val fieldName = field.getName
      val keyName = Keyboard.getKeyName(field.getInt(configObject))
      preferences.put(fieldName, keyName)
    }

    for( field <- nonKeyFields ) {
      field.setAccessible(true)
      if( field.get(configObject).isInstanceOf[Boolean] && !field.getName.contains("bitmap$") ) {
        preferences.putBoolean(field.getName, field.getBoolean(configObject))
      }
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

