package xöpäx

object PredictionStats {
	var predictioncalls = 0
	var samplingcalls = 0
	// var predictiontime = new Timer()
	// var sampligtime = new Timer()
	override def toString = "PredictionStats: %d / %d".format(predictioncalls, samplingcalls)
}
