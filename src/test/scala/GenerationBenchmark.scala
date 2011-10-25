import org.scalatest.FunSuite

import openworld._
import simplex3d.math.float._
import scala.collection.mutable.Stack

class GenerationBenchmark extends FunSuite {
	test("test1") {
		val configs = Map(
			'minMeshNodeSize -> Seq(4,8,16,32,64),
			'minPredictionSize -> Seq(4,8,16,32,64),
			'worldWindowSize -> Seq(128)
		)
		
		val timer = new Util.Timer
		val combinations = configs.values.map(_.size).product
		var currentcombination = 1
		var besttime = Double.MaxValue
		for(
			minMeshNodeSize <- configs('minMeshNodeSize);
			minPredictionSize <- configs('minPredictionSize);
			worldWindowSize <- configs('worldWindowSize)
		) {
			println("Running Config combination %d of %d...".format(currentcombination, combinations))
			println("minMeshNodeSize: " + minMeshNodeSize )
			println("minPredictionSize: " + minPredictionSize )
			println("worldWindowSize: " + worldWindowSize )

			//overwrite global Config
			Config.minMeshNodeSize = minMeshNodeSize
			Config.minPredictionSize = minPredictionSize
			Config.worldWindowSize = worldWindowSize
			WorldNodeGenerator.Master.done.dequeueAll( _ => true)
			
			assert( WorldNodeGenerator.Master.done.isEmpty )
			assert( WorldNodeGenerator.Master.activeJobs.isEmpty )
			assert( WorldNodeGenerator.Master.jobqueue.isEmpty )
			
			timer.reset
			timer.start
			
			//TODO: start a really fresh generation
			WorldGenerator.genWorld

			var running = true
			//TODO: figure out, when the generation is finished
			while( running )
			{
				Thread.sleep(100)
				if( WorldNodeGenerator.Master.activeJobs.isEmpty )
					running = false
			}
			
			timer.stop
			if( timer.read < besttime ) {
				println("FASTEST COMBINATION so far!")
				besttime = timer.read
			}
			println(">> Time: " + timer.read + "s")
			println
			currentcombination += 1
		}
	}
}