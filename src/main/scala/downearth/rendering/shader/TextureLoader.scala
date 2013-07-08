package downearth.rendering.shader

import javax.imageio.ImageIO
import java.awt.image.DataBufferByte
import org.lwjgl.BufferUtils

import simplex3d.math.double._

/**
 * Created with IntelliJ IDEA.
 * User: doering
 * Date: 7/8/13
 * Time: 6:18 PM
 * To change this template use File | Settings | File Templates.
 */
class TextureLoader {

  private def createBuffer( pixels:Array[Byte], hasAlphaChannel:Boolean ) = {
    val buffer = BufferUtils.createByteBuffer(pixels.length)
    if( hasAlphaChannel ) {
      // the default pixel format has its alpha channel at the wrong position for OpenGl Loading. So we need to convert the pixels
      for( Array(a,b,g,r) <- pixels.grouped(4) ) {
        buffer put b
        buffer put g
        buffer put r
        buffer put a
      }
    }
    else {
      buffer.put(pixels)
    }
    buffer.rewind()
    buffer
  }

  private def makeIntArray( pixels:Array[Byte], hasAlphaChannel:Boolean ) = {
    val bpp = if( hasAlphaChannel ) 4 else 3
    val array = new Array[Int](pixels.length / bpp)
    if( hasAlphaChannel ) {
      var i = 0
      for( Array(a,b,g,r) <- pixels.grouped(4) ) {
        array(i) =
        (b & 0xff) << 0 |
        (g & 0xff) << 8  |
        (r & 0xff) << 16 |
        (a & 0xff) << 24
        i += 1
      }
    }
    else {
      var i = 0
      for( Array(b,g,r) <- pixels.grouped(3) ) {
        array(i) =
          (b & 0xff) << 0 |
            (g & 0xff) << 8  |
            (r & 0xff) << 16 |
            (0xff) << 24
        i += 1
      }
    }
    array
  }

  case class QuadTexCoords(v1:Vec2, v2:Vec2, v3:Vec2, v4:Vec2)


  def createTextureAtlas( names:Seq[String] ) : Surface = {

    var maxWidth = 0
    var maxHeight = 0

    val images =
    for(name <- names) yield {
      val image = readImage(name)
      val width = image.getWidth
      val height = image.getHeight

      if(width > maxWidth)
        maxWidth = width
      if(height > maxHeight)
        maxHeight = height

      val hasAlphaChannel = image.getAlphaRaster != null
      val pixels = makeIntArray(image.getRaster.getDataBuffer.asInstanceOf[DataBufferByte].getData, hasAlphaChannel)

      new Surface( width, height, pixels )
    }

    require( ((maxWidth - 1) & maxWidth) == 0 )
    require( ((maxHeight - 1) & maxHeight) == 0 )

    val numImages = images.size

    var flip = true
    var sizeX, sizeY = 1

    while( numImages > sizeX * sizeY ) {
      if(flip)
        sizeX *= 2
      else
        sizeY *= 2
      flip = !flip
    }

    Surface.concatVertical( images.grouped(sizeX).map( Surface.concatHorizontal _ ).toSeq )
  }

  private def readImage(filename:String) = {
    val is = getClass.getClassLoader.getResourceAsStream(filename)
    if(is == null)
      throw new java.io.FileNotFoundException("that resource is not available: " + filename)
    ImageIO.read(is)
  }

  def readImageRaster( filename:String ) : Surface = {
    val image = readImage(filename)
    val data = image.getRaster.getDataBuffer.asInstanceOf[DataBufferByte].getData
    val pixels = makeIntArray(data, image.getAlphaRaster != null)
    new Surface(image.getWidth, image.getHeight, pixels)
  }

  def loadAsSkybox(filename:String, fileEnding:String):TextureCube = {
    val posX = readImageRaster(filename+"_positiveX." + fileEnding)
    val negX = readImageRaster(filename+"_negativeX." + fileEnding)
    val posY = readImageRaster(filename+"_positiveY." + fileEnding)
    val negY = readImageRaster(filename+"_negativeY." + fileEnding)
    val posZ = readImageRaster(filename+"_positiveZ." + fileEnding)
    val negZ = readImageRaster(filename+"_negativeZ." + fileEnding)

    require( posX.width == posX.height )

    val size = posX.width
    require( List(negX,posY,negY,posZ,negZ).find( surface => size != surface.width || size != surface.height ) == None )

    val d1 = BufferUtils.createByteBuffer(size*size*4)
    val d2 = BufferUtils.createByteBuffer(size*size*4)
    val d3 = BufferUtils.createByteBuffer(size*size*4)
    val d4 = BufferUtils.createByteBuffer(size*size*4)
    val d5 = BufferUtils.createByteBuffer(size*size*4)
    val d6 = BufferUtils.createByteBuffer(size*size*4)

    d1.asIntBuffer().put( posX.data )
    d2.asIntBuffer().put( negX.data )
    d3.asIntBuffer().put( posY.data )
    d4.asIntBuffer().put( negY.data )
    d5.asIntBuffer().put( posZ.data )
    d6.asIntBuffer().put( negZ.data )

    Texture.createCube(size,d1,d2,d3,d4,d5,d6)
  }

  def loadAsTexture(filename:String):Texture2D = {
    import downearth.util.time
    time("load: " + filename) {
      val raster = readImageRaster(filename:String)
      val buffer = BufferUtils.createByteBuffer(raster.width*raster.height*4)
      buffer.asIntBuffer.put(raster.data)
      Texture.create2D(raster.width, raster.height, buffer)
    }
  }
}
