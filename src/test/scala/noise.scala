import noise.perlin.prediction.bezierImproved
import org.scalatest.FunSuite

import downearth._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise._

import util.Timer

import interval.{Interval, Interval3}

class Noise extends FunSuite {

	//TODO: add accuracy test
	//TODO: add composition / prediction speed test
  test("perlin noise range (improved)") {
    val n = 1000000
    import scala.util.Random.{nextDouble => r}
    var low = Double.MaxValue
    var high = Double.MinValue
    for( _ <- 0 until n ) {
      val result = perlin.improved(r*289, r*289, r*289)
      low = low min result
      high = high max result
    }
    val bounds = perlin.prediction.trivialImproved(Interval3(0))
    println("["+low+", "+high+"] <= " + bounds)
    assert(bounds(low) && bounds(high))
  }

  test("perlin noise range (simple)") {
    val n = 1000000
    import scala.util.Random.{nextDouble => r}
    var low = Double.MaxValue
    var high = Double.MinValue
    for( _ <- 0 until n ) {
      val result = perlin.simple(r*289, r*289, r*289)
      low = low min result
      high = high max result
    }
    val bounds = perlin.prediction.trivialSimple(Interval3(0))
    println("["+low+", "+high+"] <= " + bounds)
    assert(bounds(low) && bounds(high))
  }

  test("perlin noise range (worley)") {
    val n = 1000000
    import scala.util.Random.{nextDouble => r}
    var low = Vec4(Double.MaxValue)
    var high = Vec4(Double.MinValue)
    for( _ <- 0 until n ) {
      val result = worley.reference(r*289, r*289, r*289)
      low = min(low,result)
      high = max(high,result)
    }
    val bounds = worley.prediction.trivial(Interval3(0))
    println("["+low+", "+high+"] <= " + bounds)
    assert(bounds(low) && bounds(high))
  }




  test("perlin noise prediction speed (improved)") {
		
		val noisetimer = new Timer
		val predictiontimer = new Timer
		val n = 2000
		for( i <- 0 until n )
		{
			import scala.util.Random.{nextDouble => r}
	
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
	
	
	test("perlin noise prediction correctness by sampling (improved)") {
		val n = 100
		val samples = 10
		for( i <- 0 until n )
		{
			import scala.util.Random.{nextDouble => r}
	
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
}
