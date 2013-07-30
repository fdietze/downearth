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

  // for measuring last octree update
  val updateTimer = new Timer
  var lastUpdateDuration:Long = 0

  var generationQueueSize = 0
  def workersBusy = generationQueueSize > 2*Config.numWorkingThreads

  var occlusionQueryCount = 0

  def beforeFrame() {
    lastFrame = now
    frameTimer.restart()
    if(now - lastFrameCounterReset > 1000000000)
      everySecond()
  }

  def everySecond() {
    octree.freeOldMeshNodes()

    if( Config.streamWorld )
      octree stream player

    frameRateCalculations()
  }

  def render() {
    beforeFrame()
      input.handleInput()
      physics.update()
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

  def frameRateCalculations() {
    currentFps = frameCounter
    val frameDuration = if(frameCounter > 0) frameDurationSum / frameCounter else frameDurationSum
    Display.setTitle(
      s"$currentFps/${Config.fpsLimit} fps, " +
      s"frame: ${frameDuration/1000000}<${frameDurationMax/1000000}/${timeBetweenFrames/1000000}ms, " +
      s"queries: $occlusionQueryCount, " +
      s"generating: $generationQueueSize, " +
      s"update: ${lastUpdateDuration/1000000}ms (${updateCounter}/s)"
    )

    lastFrameCounterReset = now
    frameCounter = 0
    frameDurationSum = 0
    frameDurationMax = 0
    occlusionQueryCount = 0
    updateCounter = 0
  }
}
