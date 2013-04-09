import org.scalatest.FunSuite

import openworld._
import simplex3d.math.double._
import scala.collection.mutable.Stack

class GenerationBenchmark extends FunSuite {
	test("parameter combinations") {
		val configs = Map[Symbol,Seq[AnyVal]](
			'minMeshNodeSize -> Seq(16,8,4),
			'minPredictionSize -> Seq(16,8,4),
			'kdTreePrediction -> Seq(false,true),
			'worldWindowSize -> Seq(64)
		)
		
		val timer = new Util.Timer
		val combinations = configs.values.map(_.size).product
		var currentcombination = 1
		var besttime = Double.MaxValue
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
			Config.kdTreePrediction = kdTreePrediction.asInstanceOf[Boolean]
			Config.worldWindowSize = worldWindowSize.asInstanceOf[Int]
			WorldNodeGenerator.Master.done.dequeueAll( _ => true)
			
			assert( WorldNodeGenerator.Master.done.isEmpty )
			assert( WorldNodeGenerator.Master.activeJobs.isEmpty )
			assert( WorldNodeGenerator.Master.jobqueue.isEmpty )
			
			timer.reset
			timer.start
			
			// start a fresh generation
			WorldGenerator.genWorld

			// figure out, when the generation is finished
			var running = true
			while( running )
			{
				Thread.sleep(20)
				if( WorldNodeGenerator.Master.activeJobs.isEmpty )
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
