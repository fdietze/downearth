package openworld

import java.io._
import Config.saveWorld

object WorldSerializer{
	val file = new File("worldoctree")
	
	def save(src:WorldOctree){
		if(saveWorld){
			try{
				val fos = new FileOutputStream(file)
				val oos = new ObjectOutputStream(fos)
				oos writeObject src
			}
			catch{
			case e:Exception =>
				println("couldn't save file: "+file)
			}
		}
	}
	
	def load:Option[WorldOctree] = {
		if(saveWorld){
			try {
				val fis = new FileInputStream(file)
				val ois = new ObjectInputStream(fis)
				Some(ois.readObject.asInstanceOf[WorldOctree])
			}
			catch{
			case e:Exception =>
				println("couldn't open file: "+file)
				None
			}
		}
		else
			None
	}
}

