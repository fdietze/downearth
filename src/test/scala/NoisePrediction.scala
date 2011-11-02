import org.scalatest.FunSuite

import openworld._
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._

import noise.Noise._
import noise.interval.{Interval, Volume}

class NoisePrediction extends FunSuite {

	//TODO: add accuracy test
	test("speed") {
		class Timer {
			var starttime = 0L
			var passedtime = 0L

			def getTime = System.nanoTime

			def start  { starttime = getTime }
			def stop   { passedtime += getTime - starttime }
			def measure[A](function: => A) = {
				start
				val returnvalue = function
				stop
				returnvalue
			}
			def reset  { passedtime = 0 }
			def read =   passedtime/1000000000.0
		}
		
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
	
			val volume = Volume(Vec3(x0,y0,z0), Vec3(x1,y1,z1))
			predictiontimer.measure { noise3_prediction(volume) }
			noisetimer.measure{ noise3(x0,y0,z0) }
		}
		println("noise: " + noisetimer.read/n + "s, prediction: " + predictiontimer.read/n + "s, ratio: " + predictiontimer.read/noisetimer.read)
	}
	
	
	test("correctness by sampling") {
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
	
			val prediction = noise3_prediction(Volume(Vec3(x0,y0,z0), Vec3(x1,y1,z1)))
	
			// Sample Interval
			for( u <- 1 until samples; v <- 1 until samples; w <- 1 until samples ){ 
				val x = x0 + u / samples.toDouble * (x1 - x0)
				val y = y0 + v / samples.toDouble * (y1 - y0)
				val z = z0 + w / samples.toDouble * (z1 - z0)
				val noise = noise3(x,y,z)
				assert(prediction(noise),"Wrong Prediction:\n" + prediction + ", \nInterval: " + (x0,y0,z0) + " - " + (x1,y1,z1) + "\nPosition: " + (x,y,z) + "Value: " + noise)
			}
		}
	}
}
