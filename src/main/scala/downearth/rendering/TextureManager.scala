package downearth.rendering

import org.newdawn.slick.opengl._
import org.lwjgl.opengl.GL11._

object TextureManager{
	private def loadImage(filename:String):Texture = {
		val classLoader = getClass.getClassLoader
		val format = filename.split("\\.").last.toUpperCase
		val is = classLoader.getResourceAsStream(filename)
		
		if(is == null)
			throw new java.io.FileNotFoundException("that resource is not available: " + filename)
		
		val texture = TextureLoader.getTexture( format, is)
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
		
		
		texture
	}
	
	lazy val box       = loadImage("box2.png")
	lazy val skybox    = loadImage("stormydays_rearrange_lowres.jpg")
	lazy val materials = loadImage("materials.png")
	lazy val tools     = loadImage("werkzeug.png")
}

