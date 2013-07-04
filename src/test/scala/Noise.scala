import noise.perlin.prediction.{bezierImproved, bezierSimple}
import org.scalatest.FunSuite

import downearth._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise._

import util.Timer

import interval.{Interval, Interval3}

class Noise extends FunSuite {

  test("perlin noise range (improved)") {
    val n = 1000
    val rng = new scala.util.Random(0)
    import rng.{nextDouble => r}
    var low = Double.MaxValue
    var high = Double.MinValue
    for( _ <- 0 until n ) {
      val result = perlin.improved(r*289, r*289, r*289)
      low = low min result
      high = high max result
    }
    val bounds = perlin.prediction.boundsImproved(Interval3(0))
    println("["+low+", "+high+"] <= " + bounds)
    assert(bounds(low) && bounds(high))
  }

  test("perlin noise range (simple)") {
    val n = 1000
    val rng = new scala.util.Random(0)
    import rng.{nextDouble => r}
    var low = Double.MaxValue
    var high = Double.MinValue
    for( _ <- 0 until n ) {
      val result = perlin.simple(r*289, r*289, r*289)
      low = low min result
      high = high max result
    }
    val bounds = perlin.prediction.boundsSimple(Interval3(0))
    println("["+low+", "+high+"] <= " + bounds)
    assert(bounds(low) && bounds(high))
  }

  test("worley noise range") {
    val n = 100
    val rng = new scala.util.Random(0)
    import rng.{nextDouble => r}
    var low = Vec4(Double.MaxValue)
    var high = Vec4(Double.MinValue)
    for( _ <- 0 until n ) {
      val result = worley.reference(r*289, r*289, r*289)
      low = min(low,result)
      high = max(high,result)
    }
    val bounds = worley.prediction.bounds(Interval3(0))
    println("["+low+", "+high+"] <= " + bounds)
    assert(bounds(low) && bounds(high))
  }

  test("perlin noise prediction correctness by sampling (improved)") {
    val n = 10
    val samples = 10

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

      val prediction = bezierImproved(Interval3(Vec3(x0,y0,z0), Vec3(x1,y1,z1)))

      // Sample Interval
      for( u <- 1 until samples; v <- 1 until samples; w <- 1 until samples ){
        val x = x0 + u / samples.toDouble * (x1 - x0)
        val y = y0 + v / samples.toDouble * (y1 - y0)
        val z = z0 + w / samples.toDouble * (z1 - z0)
        val noise = perlin.improved(x,y,z)
        assert(prediction(noise),"Wrong Prediction:\n" + prediction + ", \nInterval: " + (x0,y0,z0) + " - " + (x1,y1,z1) + "\nPosition: " + (x,y,z) + "Value: " + noise)
      }
    }
  }

  test("perlin noise prediction correctness by sampling (simple)") {
    val n = 10
    val samples = 10

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

      val prediction = bezierSimple(Interval3(Vec3(x0,y0,z0), Vec3(x1,y1,z1)))

      // Sample Interval
      for( u <- 1 until samples; v <- 1 until samples; w <- 1 until samples ){
        val x = x0 + u / samples.toDouble * (x1 - x0)
        val y = y0 + v / samples.toDouble * (y1 - y0)
        val z = z0 + w / samples.toDouble * (z1 - z0)
        val noise = perlin.simple(x,y,z)
        assert(prediction(noise),"Wrong Prediction:\n" + prediction + ", \nInterval: " + (x0,y0,z0) + " - " + (x1,y1,z1) + "\nPosition: " + (x,y,z) + "Value: " + noise)
      }
    }
  }
}
