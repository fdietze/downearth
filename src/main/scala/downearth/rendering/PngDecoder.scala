package downearth.rendering

import java.io.EOFException
import java.io.IOException
import java.io.InputStream
import java.nio.ByteBuffer
import java.util.zip.CRC32
import java.util.zip.DataFormatException
import java.util.zip.Inflater
import java.util

/**
 * User: arne
 * Date: 01.06.13
 * Time: 00:34
 */

object PngDecoder {
  private def checkSignature(buffer: Array[Byte]): Boolean = {

    for (i <- 0 until SIGNATURE.length) {
      if (buffer(i) != SIGNATURE(i)) {
        return false
      }
    }

    true
  }

  @inline implicit def int2byte(i:Int) = i.toByte

  private final val SIGNATURE: Array[Byte] = Array[Byte](137, 80, 78, 71, 13, 10, 26, 10)
  private final val IHDR: Int = 0x49484452
  private final val PLTE: Int = 0x504C5445
  private final val TRNS: Int = 0x74524E53
  private final val IDAT: Int = 0x49444154
  private final val IEND: Int = 0x49454E44
  private final val COLOR_GREYSCALE: Byte = 0
  private final val COLOR_TRUECOLOR: Byte = 2
  private final val COLOR_INDEXED: Byte = 3
  private final val COLOR_GREYALPHA: Byte = 4
  private final val COLOR_TRUEALPHA: Byte = 6

  object Format {
    final val ALPHA = Format(1, hasAlpha = true)
    final val LUMINANCE = Format(1,hasAlpha = false)
    final val LUMINANCE_ALPHA = Format(2,hasAlpha = true)
    final val RGB = Format(3,hasAlpha = false)
    final val RGBA = Format(4,hasAlpha = true)
    final val BGRA = Format(4,hasAlpha = true)
    final val ABGR = Format(4,hasAlpha = true)
  }

  case class Format(numComponents: Int, hasAlpha: Boolean)
}

import PngDecoder._
import PngDecoder.Format._

class PngDecoder(val input: InputStream) {

  val crc = new CRC32
  val buffer = new Array[Byte](4096)

  readFully(buffer, 0, SIGNATURE.length)
  if (!checkSignature(buffer)) {
    throw new IOException("Not a valid PNG file")
  }

  openChunk(IHDR)
  readIHDR()
  closeChunk()


  private def foo() {
    while(true) {
      openChunk()
      chunkType match {
        case IDAT =>
          return
        case PLTE =>
          readPLTE()
        case TRNS =>
          readtRNS()
      }
      closeChunk()
    }
  }
  foo()

  if (colorType == COLOR_INDEXED && palette == null) {
    throw new IOException("Missing PLTE chunk")
  }

  def getHeight: Int = {
    height
  }

  def getWidth: Int = {
    width
  }

  /**
   * Checks if the image has a real alpha channel.
   * This method does not check for the presence of a tRNS chunk.
   *
   * @return true if the image has an alpha channel
   * @see #hasAlpha()
   */
  def hasAlphaChannel: Boolean = {
    colorType == COLOR_TRUEALPHA || colorType == COLOR_GREYALPHA
  }

  /**
   * Checks if the image has transparency information either from
   * an alpha channel or from a tRNS chunk.
   *
   * @return true if the image has transparency
   * @see #hasAlphaChannel()
   * @see #overwriteTRNS(byte, byte, byte)
   */
  def hasAlpha: Boolean = {
    hasAlphaChannel || paletteA != null || transPixel != null
  }

  def isRGB: Boolean = {
    colorType == COLOR_TRUEALPHA || colorType == COLOR_TRUECOLOR || colorType == COLOR_INDEXED
  }

  /**
   * Overwrites the tRNS chunk entry to make a selected color transparent.
   * <p>This can only be invoked when the image has no alpha channel.</p>
   * <p>Calling this method causes {@link #hasAlpha()} to return true.</p>
   *
   * @param r the red component of the color to make transparent
   * @param g the green component of the color to make transparent
   * @param b the blue component of the color to make transparent
   * @throws UnsupportedOperationException if the tRNS chunk data can't be set
   * @see #hasAlphaChannel()
   */
  def overwriteTRNS(r: Byte, g: Byte, b: Byte) {
    if (hasAlphaChannel) {
      throw new UnsupportedOperationException("image has an alpha channel")
    }
    val pal: Array[Byte] = this.palette
    if (pal == null) {
      transPixel = Array[Byte](0, r, 0, g, 0, b)
    }
    else {
      paletteA = new Array[Byte](pal.length / 3)

      var i: Int = 0
      var j: Int = 0
      while (i < pal.length) {
        if (pal(i) != r || pal(i + 1) != g || pal(i + 2) != b) {
          paletteA(j) = 0xFF.toByte
        }

        i += 3
        j += 1
      }
    }
  }

  /**
   * Computes the implemented format conversion for the desired format.
   *
   * @param fmt the desired format
   * @return format which best matches the desired format
   * @throws UnsupportedOperationException if this PNG file can't be decoded
   */
  def decideTextureFormat(fmt: Format): Format = {
    colorType match {
      case COLOR_TRUECOLOR =>
        fmt match {
          case ABGR |  RGBA | BGRA | RGB => fmt
          case _ => Format.RGB
        }
      case COLOR_TRUEALPHA =>
        fmt match {
          case ABGR | RGBA | BGRA | RGB => fmt
          case _ => Format.RGBA
        }
      case COLOR_GREYSCALE =>
        fmt match {
          case LUMINANCE | ALPHA => fmt
          case _ => Format.LUMINANCE
        }
      case COLOR_GREYALPHA => Format.LUMINANCE_ALPHA
      case COLOR_INDEXED =>
        fmt match {
          case ABGR | RGBA | BGRA => fmt
          case _ => Format.RGBA
        }
      case _ => throw new UnsupportedOperationException("Not yet implemented")
    }
  }

  /**
   * Decodes the image into the specified buffer. The first line is placed at
   * the current position. After decode the buffer position is at the end of
   * the last line.
   *
   * @param buffer the buffer
   * @param stride the stride in bytes from start of a line to start of the next line, can be negative.
   * @param fmt the target format into which the image should be decoded.
   * @throws IOException if a read or data error occurred
   * @throws IllegalArgumentException if the start position of a line falls outside the buffer
   * @throws UnsupportedOperationException if the image can't be decoded into the desired format
   */
  def decode(buffer: ByteBuffer, stride: Int, fmt: Format) {
    val offset: Int = buffer.position
    val lineSize: Int = ((width * bitdepth + 7) / 8) * bytesPerPixel
    var curLine: Array[Byte] = new Array[Byte](lineSize + 1)
    var prevLine: Array[Byte] = new Array[Byte](lineSize + 1)
    var palLine: Array[Byte] = if ((bitdepth < 8)) new Array[Byte](width + 1) else null
    val inflater: Inflater = new Inflater
    try {
      var y: Int = 0
      while (y < height) {
        readChunkUnzip(inflater, curLine, 0, curLine.length)
        unfilter(curLine, prevLine)
        buffer.position(offset + y * stride)
        colorType match {
          case COLOR_TRUECOLOR =>
            fmt match {
              case ABGR =>
                copyRGBtoABGR(buffer, curLine)
              case RGBA =>
                copyRGBtoRGBA(buffer, curLine)
              case BGRA =>
                copyRGBtoBGRA(buffer, curLine)
              case RGB =>
                copy(buffer, curLine)
              case _ =>
                throw new UnsupportedOperationException("Unsupported format for this image")
            }
          case COLOR_TRUEALPHA =>
            fmt match {
              case ABGR =>
                copyRGBAtoABGR(buffer, curLine)
              case RGBA =>
                copy(buffer, curLine)
              case BGRA =>
                copyRGBAtoBGRA(buffer, curLine)
              case RGB =>
                copyRGBAtoRGB(buffer, curLine)
              case _ =>
                throw new UnsupportedOperationException("Unsupported format for this image")
            }
          case COLOR_GREYSCALE =>
            fmt match {
              case LUMINANCE | ALPHA =>
                copy(buffer, curLine)
              case _ =>
                throw new UnsupportedOperationException("Unsupported format for this image")
            }
          case COLOR_GREYALPHA =>
            fmt match {
              case LUMINANCE_ALPHA =>
                copy(buffer, curLine)
              case _ =>
                throw new UnsupportedOperationException("Unsupported format for this image")
            }
          case COLOR_INDEXED =>
            bitdepth match {
              case 8 =>
                palLine = curLine
              case 4 =>
                expand4(curLine, palLine)
              case 2 =>
                expand2(curLine, palLine)
              case 1 =>
                expand1(curLine, palLine)
              case _ =>
                throw new UnsupportedOperationException("Unsupported bitdepth for this image")
            }
            fmt match {
              case ABGR =>
                copyPALtoABGR(buffer, palLine)
              case RGBA =>
                copyPALtoRGBA(buffer, palLine)
              case BGRA =>
                copyPALtoBGRA(buffer, palLine)
              case _ =>
                throw new UnsupportedOperationException("Unsupported format for this image")
            }
          case _ =>
            throw new UnsupportedOperationException("Not yet implemented")
        }
        val tmp: Array[Byte] = curLine
        curLine = prevLine
        prevLine = tmp

        y += 1
      }
    }
    finally {
      inflater.end()
    }
  }

  /**
   * Decodes the image into the specified buffer. The last line is placed at
   * the current position. After decode the buffer position is at the end of
   * the first line.
   *
   * @param buffer the buffer
   * @param stride the stride in bytes from start of a line to start of the next line, must be positive.
   * @param fmt the target format into which the image should be decoded.
   * @throws IOException if a read or data error occurred
   * @throws IllegalArgumentException if the start position of a line falls outside the buffer
   * @throws UnsupportedOperationException if the image can't be decoded into the desired format
   */
  def decodeFlipped(buffer: ByteBuffer, stride: Int, fmt: Format) {
    if (stride <= 0) {
      throw new IllegalArgumentException("stride")
    }
    val pos: Int = buffer.position
    val posDelta: Int = (height - 1) * stride
    buffer.position(pos + posDelta)
    decode(buffer, -stride, fmt)
    buffer.position(buffer.position + posDelta)
  }

  private def copy(buffer: ByteBuffer, curLine: Array[Byte]) {
    buffer.put(curLine, 1, curLine.length - 1)
  }

  private def copyRGBtoABGR(buffer: ByteBuffer, curLine: Array[Byte]) {
    if (transPixel != null) {
      val tr: Byte = transPixel(1)
      val tg: Byte = transPixel(3)
      val tb: Byte = transPixel(5)

      var i: Int = 1
      val n: Int = curLine.length
      while (i < n) {
        val r: Byte = curLine(i)
        val g: Byte = curLine(i + 1)
        val b: Byte = curLine(i + 2)
        var a: Byte = 0xFF.toByte
        if (r == tr && g == tg && b == tb) {
          a = 0
        }
        buffer.put(a).put(b).put(g).put(r)

        i += 3
      }
    }
    else {
      var i: Int = 1
      val n: Int = curLine.length
      while (i < n) {
        buffer.put(0xFF.toByte).put(curLine(i + 2)).put(curLine(i + 1)).put(curLine(i))
        i += 3
      }
    }
  }

  private def copyRGBtoRGBA(buffer: ByteBuffer, curLine: Array[Byte]) {
    if (transPixel != null) {
      val tr: Byte = transPixel(1)
      val tg: Byte = transPixel(3)
      val tb: Byte = transPixel(5)

      var i: Int = 1
      val n: Int = curLine.length
      while (i < n) {
        val r: Byte = curLine(i)
        val g: Byte = curLine(i + 1)
        val b: Byte = curLine(i + 2)
        var a: Byte = 0xFF.toByte
        if (r == tr && g == tg && b == tb) {
          a = 0
        }
        buffer.put(r).put(g).put(b).put(a)

        i += 3
      }
    }
    else {
      var i: Int = 1
      val n: Int = curLine.length
      while (i < n) {
        buffer.put(curLine(i)).put(curLine(i + 1)).put(curLine(i + 2)).put(0xFF.toByte)

        i += 3
      }
    }
  }

  private def copyRGBtoBGRA(buffer: ByteBuffer, curLine: Array[Byte]) {
    if (transPixel != null) {
      val tr: Byte = transPixel(1)
      val tg: Byte = transPixel(3)
      val tb: Byte = transPixel(5)

      var i: Int = 1
      val n: Int = curLine.length
      while (i < n) {
        val r: Byte = curLine(i)
        val g: Byte = curLine(i + 1)
        val b: Byte = curLine(i + 2)
        var a: Byte = 0xFF.toByte
        if (r == tr && g == tg && b == tb) {
          a = 0
        }
        buffer.put(b).put(g).put(r).put(a)

        i += 3
      }
    }
    else {
      var i: Int = 1
      val n: Int = curLine.length
      while (i < n) {
        buffer.put(curLine(i + 2)).put(curLine(i + 1)).put(curLine(i)).put(0xFF.toByte)

        i += 3
      }
    }
  }

  private def copyRGBAtoABGR(buffer: ByteBuffer, curLine: Array[Byte]) {
    var i: Int = 1
    val n: Int = curLine.length
    while (i < n) {
      buffer.put(curLine(i + 3)).put(curLine(i + 2)).put(curLine(i + 1)).put(curLine(i))

      i += 4
    }
  }

  private def copyRGBAtoBGRA(buffer: ByteBuffer, curLine: Array[Byte]) {
    var i: Int = 1
    val n: Int = curLine.length

    while (i < n) {
      buffer.put(curLine(i + 2)).put(curLine(i + 1)).put(curLine(i)).put(curLine(i + 3))
      i += 4
    }
  }

  private def copyRGBAtoRGB(buffer: ByteBuffer, curLine: Array[Byte]) {
    var i: Int = 1
    val n: Int = curLine.length
    while (i < n) {
      buffer.put(curLine(i)).put(curLine(i + 1)).put(curLine(i + 2))

      i += 4
    }
  }

  private def copyPALtoABGR(buffer: ByteBuffer, curLine: Array[Byte]) {
    if (paletteA != null) {
      var i: Int = 1
      val n: Int = curLine.length
      while (i < n) {
        val idx: Int = curLine(i) & 255
        val r: Byte = palette(idx * 3 + 0)
        val g: Byte = palette(idx * 3 + 1)
        val b: Byte = palette(idx * 3 + 2)
        val a: Byte = paletteA(idx)
        buffer.put(a).put(b).put(g).put(r)

        i += 1
      }
    }
    else {
      var i: Int = 1
      val n: Int = curLine.length
      while (i < n) {
        val idx: Int = curLine(i) & 255
        val r: Byte = palette(idx * 3 + 0)
        val g: Byte = palette(idx * 3 + 1)
        val b: Byte = palette(idx * 3 + 2)
        val a: Byte = 0xFF.toByte
        buffer.put(a).put(b).put(g).put(r)

        i += 1
      }
    }
  }

  private def copyPALtoRGBA(buffer: ByteBuffer, curLine: Array[Byte]) {
    if (paletteA != null) {
      var i: Int = 1
      val n: Int = curLine.length
      while (i < n) {
        val idx: Int = curLine(i) & 255
        val r: Byte = palette(idx * 3 + 0)
        val g: Byte = palette(idx * 3 + 1)
        val b: Byte = palette(idx * 3 + 2)
        val a: Byte = paletteA(idx)
        buffer.put(r).put(g).put(b).put(a)

        i += 1
      }
    }
    else {
      var i: Int = 1
      val n: Int = curLine.length
      while (i < n) {
        val idx: Int = curLine(i) & 255
        val r: Byte = palette(idx * 3 + 0)
        val g: Byte = palette(idx * 3 + 1)
        val b: Byte = palette(idx * 3 + 2)
        val a: Byte = 0xFF.toByte
        buffer.put(r).put(g).put(b).put(a)

        i += 1
      }
    }
  }

  private def copyPALtoBGRA(buffer: ByteBuffer, curLine: Array[Byte]) {
    if (paletteA != null) {
      var i: Int = 1
      val n: Int = curLine.length
      while (i < n) {
        val idx: Int = curLine(i) & 255
        val r: Byte = palette(idx * 3 + 0)
        val g: Byte = palette(idx * 3 + 1)
        val b: Byte = palette(idx * 3 + 2)
        val a: Byte = paletteA(idx)

        buffer.put(b).put(g).put(r).put(a)
        i += 1
      }
    }
    else {
      var i: Int = 1
      val n: Int = curLine.length
      while (i < n) {
        val idx: Int = curLine(i) & 255
        val r: Byte = palette(idx * 3 + 0)
        val g: Byte = palette(idx * 3 + 1)
        val b: Byte = palette(idx * 3 + 2)
        val a: Byte = 0xFF.toByte
        buffer.put(b).put(g).put(r).put(a)

        i += 1
      }
    }
  }

  private def expand4(src: Array[Byte], dst: Array[Byte]) {
    {
      var i: Int = 1
      val n: Int = dst.length
      while (i < n) {
        val value: Int = src(1 + (i >> 1)) & 255
        n - i match {
          case 1 => dst(i) = (value >> 4).toByte
          case _ => dst(i + 1) = (value & 15).toByte
        }

        i += 2
      }
    }
  }

  private def expand2(src: Array[Byte], dst: Array[Byte]) {
    var i: Int = 1
    val n: Int = dst.length
    while (i < n) {
      val value: Int = src(1 + (i >> 2)) & 255
      n - i match {
        case 3 => dst(i + 2) = ((value >> 2) & 3).toByte
        case 2 => dst(i + 2) = ((value >> 2) & 3).toByte
        case 1 => dst(i) = ((value >> 6)).toByte
        case _ => dst(i + 3) = ((value) & 3).toByte

      }
      i += 4
    }
  }

  private def expand1(src: Array[Byte], dst: Array[Byte]) {
    var i: Int = 1
    val n: Int = dst.length
    while (i < n) {
      val `val`: Int = src(1 + (i >> 3)) & 255
      n - i match {
        case 7 =>
          dst(i + 6) = ((`val` >> 1) & 1).toByte
        case 6 =>
          dst(i + 5) = ((`val` >> 2) & 1).toByte
        case 5 =>
          dst(i + 4) = ((`val` >> 3) & 1).toByte
        case 4 =>
          dst(i + 3) = ((`val` >> 4) & 1).toByte
        case 3 =>
          dst(i + 2) = ((`val` >> 5) & 1).toByte
        case 2 =>
          dst(i + 1) = ((`val` >> 6) & 1).toByte
        case 1 =>
          dst(i) = ((`val` >> 7)).toByte
        case _ =>
          dst(i + 7) = ((`val`) & 1).toByte
      }
      i += 8
    }
  }

  private def unfilter(curLine: Array[Byte], prevLine: Array[Byte]) {
    curLine(0) match {
      case 0 =>
        // none
      case 1 =>
        unfilterSub(curLine)
      case 2 =>
        unfilterUp(curLine, prevLine)
      case 3 =>
        unfilterAverage(curLine, prevLine)
      case 4 =>
        unfilterPaeth(curLine, prevLine)
      case _ =>
        throw new IOException("invalide filter type in scanline: " + curLine(0))
    }
  }

  private def unfilterSub(curLine: Array[Byte]) {
    val bpp: Int = this.bytesPerPixel
    for( i <- bpp+1 until curLine.length) {
      curLine(i) = (curLine(i) + curLine(i - bpp)).toByte
    }
  }

  private def unfilterUp(curLine: Array[Byte], prevLine: Array[Byte]) {
    for( i <- 1 until curLine.length ) {
      curLine(i) += prevLine(i)
    }
  }

  private def unfilterAverage(curLine: Array[Byte], prevLine: Array[Byte]) {
    val bpp: Int = this.bytesPerPixel
    var i: Int = 1

    while (i <= bpp) {
      curLine(i) += ((prevLine(i) & 0xFF) >>> 1).toByte
      i += 1
    }
    val n: Int = curLine.length
    while (i < n) {
      curLine(i) += (((prevLine(i) & 0xFF) + (curLine(i - bpp) & 0xFF)) >>> 1).toByte
      i += 1
    }
  }

  private def unfilterPaeth(curLine: Array[Byte], prevLine: Array[Byte]) {
    val bpp: Int = this.bytesPerPixel
    var i: Int = 1
    while (i <= bpp) {
      curLine(i) += prevLine(i)
      i += 1
    }

    val n: Int = curLine.length
    while (i < n) {
      val a: Int = curLine(i - bpp) & 255
      val b: Int = prevLine(i) & 255
      var c: Int = prevLine(i - bpp) & 255
      val p: Int = a + b - c
      var pa: Int = p - a
      if (pa < 0) pa = -pa
      var pb: Int = p - b
      if (pb < 0) pb = -pb
      var pc: Int = p - c
      if (pc < 0) pc = -pc
      if (pa <= pb && pa <= pc) c = a
      else if (pb <= pc) c = b
      curLine(i) += c.toByte

      i += 1
    }
  }

  private def readIHDR() {
    checkChunkLength(13)
    readChunk(buffer, 0, 13)
    width = readInt(buffer, 0)
    height = readInt(buffer, 4)
    bitdepth = buffer(8) & 255
    colorType = buffer(9) & 255
    colorType match {
      case COLOR_GREYSCALE =>
        if (bitdepth != 8) {
          throw new IOException("Unsupported bit depth: " + bitdepth)
        }
        bytesPerPixel = 1
      case COLOR_GREYALPHA =>
        if (bitdepth != 8) {
          throw new IOException("Unsupported bit depth: " + bitdepth)
        }
        bytesPerPixel = 2
      case COLOR_TRUECOLOR =>
        if (bitdepth != 8) {
          throw new IOException("Unsupported bit depth: " + bitdepth)
        }
        bytesPerPixel = 3
      case COLOR_TRUEALPHA =>
        if (bitdepth != 8) {
          throw new IOException("Unsupported bit depth: " + bitdepth)
        }
        bytesPerPixel = 4
      case COLOR_INDEXED =>
        bitdepth match {
          case 1 | 2 | 4 | 8 =>
            bytesPerPixel = 1
          case _ =>
            throw new IOException("Unsupported bit depth: " + bitdepth)
        }
      case _ =>
        throw new IOException("unsupported color format: " + colorType)
    }
    if (buffer(10) != 0) {
      throw new IOException("unsupported compression method")
    }
    if (buffer(11) != 0) {
      throw new IOException("unsupported filtering method")
    }
    if (buffer(12) != 0) {
      throw new IOException("unsupported interlace method")
    }
  }

  private def readPLTE() {
    val paletteEntries: Int = chunkLength / 3
    if (paletteEntries < 1 || paletteEntries > 256 || (chunkLength % 3) != 0) {
      throw new IOException("PLTE chunk has wrong length")
    }
    palette = new Array[Byte](paletteEntries * 3)
    readChunk(palette, 0, palette.length)
  }

  private def readtRNS() {
    colorType match {
      case COLOR_GREYSCALE =>
        checkChunkLength(2)
        transPixel = new Array[Byte](2)
        readChunk(transPixel, 0, 2)
      case COLOR_TRUECOLOR =>
        checkChunkLength(6)
        transPixel = new Array[Byte](6)
        readChunk(transPixel, 0, 6)
      case COLOR_INDEXED =>
        if (palette == null) {
          throw new IOException("tRNS chunk without PLTE chunk")
        }
        paletteA = new Array[Byte](palette.length / 3)
        util.Arrays.fill(paletteA, 0xFF.toByte)
        readChunk(paletteA, 0, paletteA.length)
      case _ =>
    }
  }

  private def closeChunk() {
    if (chunkRemaining > 0) {
      skip(chunkRemaining + 4)
    }
    else {
      readFully(buffer, 0, 4)
      val expectedCrc: Int = readInt(buffer, 0)
      val computedCrc: Int = crc.getValue.asInstanceOf[Int]
      if (computedCrc != expectedCrc) {
        throw new IOException("Invalid CRC")
      }
    }
    chunkRemaining = 0
    chunkLength = 0
    chunkType = 0
  }

  private def openChunk() {
    readFully(buffer, 0, 8)
    chunkLength = readInt(buffer, 0)
    chunkType = readInt(buffer, 4)
    chunkRemaining = chunkLength
    crc.reset()
    crc.update(buffer, 4, 4)
  }

  private def openChunk(expected: Int) {
    openChunk()
    if (chunkType != expected) {
      throw new IOException("Expected chunk: " + Integer.toHexString(expected))
    }
  }

  private def checkChunkLength(expected: Int) {
    if (chunkLength != expected) {
      throw new IOException("Chunk has wrong size")
    }
  }

  private def readChunk(buffer: Array[Byte], offset: Int, _length: Int): Int = {
    val length = if( _length > chunkRemaining )
      chunkRemaining
    else
      _length

    readFully(buffer, offset, length)
    crc.update(buffer, offset, length)
    chunkRemaining -= length

    length
  }

  private def refillInflater(inflater: Inflater) {
    while (chunkRemaining == 0) {
      closeChunk()
      openChunk(IDAT)
    }
    val read: Int = readChunk(buffer, 0, buffer.length)
    inflater.setInput(buffer, 0, read)
  }

  private def readChunkUnzip(inflater: Inflater, buffer: Array[Byte], _offset: Int, _length: Int) {
    var offset = _offset
    var length = _length
    assert(buffer ne this.buffer)
    try {
      do {
        val read: Int = inflater.inflate(buffer, offset, length)
        if (read <= 0) {
          if (inflater.finished) {
            throw new EOFException
          }
          if (inflater.needsInput) {
            refillInflater(inflater)
          }
          else {
            throw new IOException("Can't inflate " + length + " bytes")
          }
        }
        else {
          offset += read
          length -= read
        }
      } while (length > 0)
    }
    catch {
      case ex: DataFormatException => {
        throw (new IOException("inflate error").initCause(ex)).asInstanceOf[IOException]
      }
    }
  }

  private def readFully(buffer: Array[Byte], _offset: Int, _length: Int) {
    var offset = _offset
    var length = _length
    do {
      val read: Int = input.read(buffer, offset, length)
      if (read < 0) {
        throw new EOFException
      }
      offset += read
      length -= read
    } while (length > 0)
  }

  private def readInt(buffer: Array[Byte], offset: Int): Int = {
    ((buffer(offset)) << 24) | ((buffer(offset + 1) & 255) << 16) | ((buffer(offset + 2) & 255) << 8) | ((buffer(offset + 3) & 255))
  }

  private def skip(_amount: Long) {
    var amount = _amount
    while (amount > 0) {
      val skipped: Long = input.skip(amount)
      if (skipped < 0) {
        throw new EOFException
      }
      amount -= skipped
    }
  }

  private var chunkLength: Int = 0
  private var chunkType: Int = 0
  private var chunkRemaining: Int = 0
  private var width: Int = 0
  private var height: Int = 0
  private var bitdepth: Int = 0
  private var colorType: Int = 0
  private var bytesPerPixel: Int = 0
  private var palette: Array[Byte] = null
  private var paletteA: Array[Byte] = null
  private var transPixel: Array[Byte] = null
}