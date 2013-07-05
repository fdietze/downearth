package downearth.rendering

import javax.imageio.ImageIO
import java.awt.image.DataBufferByte
import org.lwjgl.BufferUtils
import simplex3d.math.double._
import simplex3d.math.{ConstVec2i, ReadVec2i}

/**
 * User: arne
 * Date: 29.04.13
 * Time: 21:56
 */

object Surface {
  def concatHorizontal(images:Seq[Surface]):Surface = {
    val height = images.head.height
    require( (true /: images) ( _ && _.height == height ) )
    val width = (0 /: images) ( _ + _.width )

    val data = new Array[Int](width*height)

    var offset = 0
    for(line <- 0 until height) {
      for(img <- images) {
        Array.copy(img,line*img.width, data, offset, img.width)
        offset += img.width
      }
    }

    new Surface(width, height, data)
  }

  def concatVertical(images:Seq[Surface]):Surface = {
    val width = images.head.width
    require( (true /: images) ( _ && _.height == width ) )
    val height = (0 /: images) ( _ + _.height )

    val data = Array.concat( images.map(_.data):_* )

    new Surface(width, height, data)
  }

  def blit(src:Surface, screen:Surface) {
    blit(src, screen, new Rect(0,0,-1,-1) )
  }

  def blit(src:Surface, screen:Surface, dstRect:Rect) {
    blit(src, new Rect(0,0,src.width,src.height), screen, dstRect)
  }

  // same as SDL_BlitSurface but in software and overwriting alpha at the moment
  def blit(src:Surface, srcRect:Rect, dst:Surface, dstRect:Rect) {
    require( srcRect.x >= 0 && srcRect.y >= 0)
    require( srcRect.x >= 0 && srcRect.y >= 0)

    dstRect.w = srcRect.w
    dstRect.h = srcRect.h

    // out of target

    if ( dstRect.x >= dst.width )
      return
    if ( dstRect.y >= dst.height )
      return
    if ( dstRect.x + dstRect.w <= 0 )
      return
    if ( dstRect.y + dstRect.h <= 0 )

    // clipping

    if( dstRect.x < 0 ) {
      srcRect.x - dstRect.x
      srcRect.w + dstRect.x
      dstRect.x = 0
    }

    if( dstRect.y < 0 ) {
      srcRect.y - dstRect.y
      srcRect.h + dstRect.y
      dstRect.y = 0
    }

    if( dstRect.x + srcRect.w >= dst.width ) {
      dstRect.w = dst.width - dstRect.x
      srcRect.w = dstRect.w
    }

    if( dstRect.y + srcRect.h >= dst.height ) {
      dstRect.h = dst.height - dstRect.y
      srcRect.h = dstRect.h
    }

    // some checks that need to be true now

    assert( src checkBounds srcRect )
    assert( dst checkBounds dstRect )
    assert( srcRect.w == dstRect.w && srcRect.h == dstRect.h)

    for( y <- 0 until srcRect.h ) {
      val srcY = srcRect.y + y
      val dstY = dstRect.y + y
      System.arraycopy( src.data, src.indexOf(srcRect.x, srcY), dst.data, dst.indexOf(dstRect.x, dstY), srcRect.w )
    }
  }
}

class Rect(var x:Int, var y:Int, var w:Int, var h:Int) {
  def this(pos:ReadVec2i, size:ReadVec2i) = this(pos.x, pos.y, size.x, size.y)

  def pos:ReadVec2i = ConstVec2i(x,y)
  def pos_=(v:ReadVec2i) {
    x = v.x
    y = v.y
  }

  def size:ReadVec2i = ConstVec2i(w,h)
  def size_=(v:ReadVec2i) {
    w = v.x
    y = v.y
  }

}

final class Surface(val width:Int, val height:Int, val data:Array[Int] ) {
  require( ((width - 1) & width) == 0 )
  require( ((height - 1) & height) == 0 )
  require( data.length == width * height, "data length is %d , but should be %d, width: %d, height: %d".format(data.length,width*height,width,height) )

  def this(width:Int,height:Int) = this(width,height, new Array[Int](width*height))

  def apply(x:Int,y:Int) = data( indexOf(x,y) )

  def update(x:Int,y:Int, color:Int) {
    data( indexOf(x,y) ) = color
  }

  def checkBounds( rect:Rect ) = {
    0 <= rect.x &&
    0 <= rect.y &&
    rect.x + rect.w <= width &&
    rect.y + rect.h <= height
  }

  def indexOf(x:Int, y:Int) = {
    require(0 <= x && x < width)
    require(0 <= y && y < height)
    y * width + x
  }

}

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

    new TextureCube(size,d1,d2,d3,d4,d5,d6)
  }

  def loadAsTexture(filename:String):Texture2D = {
    import downearth.util.time
    time("load: " + filename) {
      val raster = readImageRaster(filename:String)
      val buffer = BufferUtils.createByteBuffer(raster.width*raster.height*4)
      buffer.asIntBuffer.put(raster.data)
      new Texture2D(raster.width, raster.height, buffer)
    }
  }
}
