package downearth.benchmark


object Main extends App {
  Noise.perlinNoisePredictionSpeedSimple()
  Noise.perlinNoisePredictionSpeedImproved()
  
  Octree.nodeInfoTraversal()
  
  Generation.data3DExtract()
  Generation.FullGeneration()
}
