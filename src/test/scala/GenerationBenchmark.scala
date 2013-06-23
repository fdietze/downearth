import akka.util.Timeout
import org.scalatest.FunSuite

import downearth._
import generation._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import util._
import simplex3d.math.double._
import collection.mutable
import akka.pattern.ask

class GenerationBenchmark extends FunSuite {

  // kdTreePrediction is in current code base without usage

	test("parameter combinations") {
		val configs = Map[Symbol,Seq[AnyVal]](
			'minMeshNodeSize -> Seq(16,8,4,2,1),
			'minPredictionSize -> Seq(16,8,4,2,1),
			'kdTreePrediction -> Seq(false,true),
			'worldWindowSize -> Seq(64)
		)
		
		val timer = new Timer
		val combinations = configs.values.map(_.size).product
		var currentcombination = 1
		var besttime = Double.MaxValue
    implicit val timeout = Timeout(30000)
		for(
			minMeshNodeSize <- configs('minMeshNodeSize);
			minPredictionSize <- configs('minPredictionSize);
			kdTreePrediction <- configs('kdTreePrediction);
			worldWindowSize <- configs('worldWindowSize)
		) {
			println("Configuration %d of %d...".format(currentcombination, combinations))

			//overwrite global Config
			Config.minMeshNodeSize = minMeshNodeSize.asInstanceOf[Int]
			Config.minPredictionSize = minPredictionSize.asInstanceOf[Int]
			Config.worldWindowSize = worldWindowSize.asInstanceOf[Int]
			WorldNodeGenerator.master ! GetFinishedJobs
			
      assert(Await.result((WorldNodeGenerator.master ? AllJobsEmpty).mapTo[Boolean],Duration.Inf))
			
			timer.reset
			timer.start
			
			// start a fresh generation
			WorldGenerator.genWorld

			// figure out, when the generation is finished
			var running = true
			while( running )
			{
				Thread.sleep(20)
				if( Await.result((WorldNodeGenerator.master ? ActiveJobsEmpty).mapTo[Boolean],Duration.Inf) )
					running = false
			}
			
			timer.stop
			if( timer.read < besttime ) {
				println("minMeshNodeSize:   " + minMeshNodeSize )
				println("minPredictionSize: " + minPredictionSize )
				println("kdTreePrediction:  " + kdTreePrediction )
				println("worldWindowSize:   " + worldWindowSize )
				println("---- FASTEST CONFIGURATION so far! ----")
				besttime = timer.read
			}
			println(">> Time: " + timer.read + "s")
			println
			currentcombination += 1
		}
	}
}
