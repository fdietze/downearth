package downearth.benchmark

import downearth.util.Timer
import interval.Interval3
import simplex3d.math.double._
import noise.perlin.prediction.{bezierSimple, bezierImproved}
import noise.perlin

object Noise {
  def perlinNoisePredictionSpeedImproved() {
    println("perlin noise prediction speed (improved)")
    val noisetimer = new Timer
    val predictiontimer = new Timer
    val n = 20000

    val rng = new scala.util.Random(0)
    import rng.{nextDouble => r}

    for( i <- 0 until n )
    {
      val x0 = 1/r
      val y0 = 1/r
      val z0 = 1/r
      val x1 = x0 + r/30
      val y1 = y0 + r/30
      val z1 = z0 + r/30

      val volume = Interval3(Vec3(x0,y0,z0), Vec3(x1,y1,z1))
      predictiontimer.measure { bezierImproved(volume) }
      noisetimer.measure{ perlin.improved(x0,y0,z0) }
    }
    println("noise: " + noisetimer.read/n + "s, prediction: " + predictiontimer.read/n + "s, ratio: " + predictiontimer.read/noisetimer.read)
  }


  def perlinNoisePredictionSpeedSimple() {
    println("perlin noise prediction speed (simple)")

    val noisetimer = new Timer
    val predictiontimer = new Timer
    val n = 20000

    val rng = new scala.util.Random(0)
    import rng.{nextDouble => r}

    for( i <- 0 until n )
    {
      val x0 = 1/r
      val y0 = 1/r
      val z0 = 1/r
      val x1 = x0 + r/30
      val y1 = y0 + r/30
      val z1 = z0 + r/30

      val volume = Interval3(Vec3(x0,y0,z0), Vec3(x1,y1,z1))
      predictiontimer.measure { bezierSimple(volume) }
      noisetimer.measure{ perlin.simple(x0,y0,z0) }
    }
    println("noise: " + noisetimer.read/n + "s, prediction: " + predictiontimer.read/n + "s, ratio: " + predictiontimer.read/noisetimer.read)
  }
}
