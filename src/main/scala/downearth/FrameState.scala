package downearth

import downearth.util.Timer
import org.lwjgl.opengl.Display

class FrameState(gameState:GameState) {
  import gameState._

  def timeBetweenFrames = 1000000000 / downearth.Config.fpsLimit

  def now = System.nanoTime
  val starttime = now
  //def uptime = now - starttime

  var lastFrame = now
  var currentFps = 0
  var lastFrameCounterReset = starttime
  var frameCounter = 0
  var updateCounter = 0

  // for measuring last frame
  val frameTimer = new Timer
  var lastFrameDuration:Long = 0
  var frameDurationSum:Long = 0
  var frameDurationMax:Long = 0

  var occlusionQueryCount = 0
  var renderedBoxCount = 0


  var generationQueueSize = 0
  def workersBusy = generationQueueSize > 10*Config.numWorkingThreads // threads * actor throughput


  def beforeFrame() {
    lastFrame = now
    frameTimer.restart()
    if(now - lastFrameCounterReset > 1000000000)
      everySecond()
  }

  def everySecond() {
    if( Config.freeOldMeshes ) octree.freeOldMeshNodes()
    displayStats()
  }

  def render() {
    beforeFrame()
      input.handleInput()
      if( Config.streaming ) octree stream player
      if(Config.physics) physics.update()
      renderer.draw()
      Display.update()
    afterFrame()
  }

  def afterFrame() {
    lastFrameDuration = frameTimer.readNanos
    frameDurationSum += lastFrameDuration
    frameDurationMax = frameDurationMax max lastFrameDuration
    frameCounter += 1
  }

  def displayStats() {
    currentFps = frameCounter
    val frameDuration = if(frameCounter > 0) frameDurationSum / frameCounter else frameDurationSum
    val queriesPerFrame = if(frameCounter > 0) occlusionQueryCount / frameCounter else 0
    val renderedBoxesPerFrame = if(frameCounter > 0) renderedBoxCount / frameCounter else 0
    val insertsPerFrame = if(frameCounter > 0) updateCounter / frameCounter else 0
    val mib = 1024 * 1024
    def usedMemory = (sys.runtime.totalMemory - sys.runtime.freeMemory) / mib
    def heapSize = sys.runtime.totalMemory / mib

    Display.setTitle(
      s"$currentFps/${Config.fpsLimit} fps, " +
      s"frame: ${frameDuration/1000000}<${frameDurationMax/1000000}/${timeBetweenFrames/1000000}ms, " +
      s"OccQueries/f: $queriesPerFrame, " +
      s"boxes/f: $renderedBoxesPerFrame, " +
      s"genQ: $generationQueueSize, " +
      s"inserts/f: $insertsPerFrame, " +
      s"heap: ${usedMemory}/${heapSize}MiB"
    )

    lastFrameCounterReset = now
    frameCounter = 0
    frameDurationSum = 0
    frameDurationMax = 0
    occlusionQueryCount = 0
    renderedBoxCount = 0
    updateCounter = 0
  }
}
