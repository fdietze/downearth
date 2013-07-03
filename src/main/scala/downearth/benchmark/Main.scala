package downearth.benchmark


object Main extends App {
  Noise.perlinNoisePredictionSpeedSimple()
  Noise.perlinNoisePredictionSpeedImproved()
  
  Octree.nodeInfoTraversal()
  
  Generation.FullGeneration()
}
