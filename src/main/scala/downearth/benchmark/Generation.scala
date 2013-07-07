package downearth.benchmark

import downearth.util.Timer
import simplex3d.math._
import simplex3d.math.double._
import downearth.worldoctree.PowerOfTwoCube
import downearth.generation.WorldFunction
import downearth.generation.WorldGenerator.genWorldAt
import downearth.util._

object Generation {

  def dummyOpenGLContext() {
    import org.lwjgl.opengl._
    import org.lwjgl.input._
    val ca = new ContextAttribs(3,2).withProfileCompatibility(true)
    val alpha = 8
    val depth = 16
    val stencil = 0
    val pf = new PixelFormat(alpha, depth, stencil)
    
    Display.setDisplayMode( new DisplayMode(100,100) )
    Display.setResizable(true)
    Display.create(pf, ca)
  }

  def FullGeneration() {
    println("Full 32x32x32 Generation")
    val timer = new Timer
    
    //warmup:
    dummyOpenGLContext()
    genWorldAt(PowerOfTwoCube(pos=Vec3i(0),size=8),
               worldFunction = TestingWorldDefinition)

    val positions = Vec3i(0) until Vec3i(3)
    for( size <- Seq(1,2,4,8,16) ) {
      timer.restart()
      for( pos <- positions ) {
        genWorldAt(PowerOfTwoCube(pos*size,size),
          worldFunction = TestingWorldDefinition,
          prediction = false)
      }
      timer.stop()
      println(s"Size $size: ${timer.read/(positions.size)}s/node")

      timer.restart()
      for( pos <- positions ) {
        genWorldAt(PowerOfTwoCube(pos*size,size),
          worldFunction = TestingWorldDefinition,
          prediction = true)
      }
      timer.stop()
      println(s"Size $size: ${timer.read/(positions.size)}s/node (prediction)")
    }

  }
}
