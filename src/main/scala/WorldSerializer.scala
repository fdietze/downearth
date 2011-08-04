package xöpäx

import java.io._

object WorldSerializer{
	val file = new File("worldoctree")
	
	def save(src:WorldOctree){
		try{
			val fos = new FileOutputStream(file)
			val oos = new ObjectOutputStream(fos)
			oos writeObject src
		}
		catch{
		case e:Exception =>
			println("couldn't save file: "+file)
			e.printStackTrace
		}
	}
	
	def load:Option[WorldOctree] = {
		try {
			val fis = new FileInputStream(file)
			val ois = new ObjectInputStream(fis)
			Some(ois.readObject.asInstanceOf[WorldOctree])
		}
		catch{
		case e:Exception =>
			println("couldn't open file: "+file)
			e.printStackTrace
			None
		}
	}
}
