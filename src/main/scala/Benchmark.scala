package openworld

object Benchmark {
	
	def generation {
		val configs = Map(
			'minMeshNodeSize -> Seq(16),
			'minPredictionSize -> Seq(16),
			'worldWindowSize -> Seq(32,64,128)
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
			println("Running Config combination %d of %d..." format(currentcombination, combinations))
			println("minMeshNodeSize: " + minMeshNodeSize )
			println("minPredictionSize: " + minPredictionSize )
			println("worldWindowSize: " + worldWindowSize )

			//overwrite global Config
			Config.minMeshNodeSize = minMeshNodeSize
			Config.minPredictionSize = minPredictionSize
			Config.worldWindowSize = worldWindowSize

			timer.reset
			timer.start
			
			//TODO: start a really fresh generation
			openworld.WorldGenerator.genWorld

			println("jobs:   " + WorldNodeGenerator.Master.jobqueue)
			println("active: " + WorldNodeGenerator.Master.activeJobs)
			println("idle:   " + WorldNodeGenerator.Master.idleWorkers)
			
			var running = true
			//TODO: figure out, when the generation is finished
			while( running )
			{
				print( WorldNodeGenerator.Master.idleWorkers.size + " " )
				Thread.sleep(100)
				if( WorldNodeGenerator.Master.idleWorkers.size == Config.numWorkingThreads
				  WorldNodeGenerator.Master.jobqueue.isEmpty
				 && WorldNodeGenerator.Master.activeJobs.isEmpty
				)
					running = false
			}
			println
			println("jobs:   " + WorldNodeGenerator.Master.jobqueue)
			println("active: " + WorldNodeGenerator.Master.activeJobs)
			println("idle:   " + WorldNodeGenerator.Master.idleWorkers)
			
			timer.stop
			if( timer.read < besttime ) {
				println("FASTEST COMBINATION so far!")
				besttime = timer.read
			}
			println("Time: " + timer.read + "s")
			println
			currentcombination += 1
		}
	}
}
