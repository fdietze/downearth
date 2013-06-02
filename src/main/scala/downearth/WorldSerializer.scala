package downearth

import java.io._
import Config.saveWorld
import downearth.worldoctree.WorldOctree

object WorldSerializer{
	val file = new File("worldoctree")
	
	def save(src:WorldOctree){
		if(saveWorld){
			try{
				val fos = new FileOutputStream(file)
				fos.write(src.toMessage.toByteArray)
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
				//val fis = new FileInputStream(file)
				//Some( message.Octree().mergeFrom(fis) )
        None
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

