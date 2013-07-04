package downearth.benchmark

import downearth.util.Timer
import simplex3d.math._
import simplex3d.math.double._
import downearth.worldoctree.PowerOfTwoCube
import downearth.generation.WorldFunction
import downearth.generation.WorldGenerator.genWorldAt

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
    println("Full 16x16x16 Generation")
    val timer = new Timer
    
    dummyOpenGLContext()
    genWorldAt(PowerOfTwoCube(pos=Vec3i(0),size=16),
               worldFunction = TestingWorldDefinition)
    
    Thread.sleep(500)
    
    timer.benchmark(50) {
      genWorldAt(PowerOfTwoCube(pos=Vec3i(0),size=16),
                 worldFunction = TestingWorldDefinition)
    }

    println(s"${timer.read}s")
  }
}
