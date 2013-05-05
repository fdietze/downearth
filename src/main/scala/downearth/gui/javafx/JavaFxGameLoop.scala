package downearth.gui.javafx

import scala.collection.mutable

import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL11.glGetInteger
import org.lwjgl.opengl.AMDDebugOutput._
import org.lwjgl.opengl.ARBDebugOutput._
import org.lwjgl.opengl._
import org.lwjgl.util.stream.StreamUtil.RenderStreamFactory
import org.lwjgl.util.stream.{StreamUtil, StreamHandler}

import downearth.{Player, GameLoop}
import downearth.rendering.Renderer
import downearth.util.round10

/**
 * User: arne
 * Date: 29.04.13
 * Time: 00:20
 */
class JavaFxGameLoop(val readHandler: StreamHandler, guiController:HudController) extends GameLoop {
  /* adopted from Gears.java */
  val pendingRunnables = new mutable.SynchronizedQueue[() => Unit]

  if ((Pbuffer.getCapabilities & Pbuffer.PBUFFER_SUPPORTED) == 0)
    throw new UnsupportedOperationException("Support for pbuffers is required.")

  val pbuffer = new Pbuffer(1, 1, new PixelFormat(), null, null, new ContextAttribs())
  pbuffer.makeCurrent()

  val drawable = pbuffer // wofür auch immer

  private var m_transfersToBuffer = 3
  def transfersToBuffer = m_transfersToBuffer
  def transfersToBuffer_=(transfersToBuffer: Int) {
    if (m_transfersToBuffer != transfersToBuffer) {
      m_transfersToBuffer = transfersToBuffer
    }
    resetStreams
  }

  private var m_samples = 1
  def samples = m_samples
  def samples_=(samples:Int) {
    if (m_samples != samples) {
      m_samples = samples
      resetStreams
    }
  }

  private var m_renderStreamFactory = StreamUtil.getRenderStreamImplementation
  var renderStream = m_renderStreamFactory.create(readHandler, 1, transfersToBuffer)
  def renderStreamFactory = m_renderStreamFactory
  def renderStreamFactory_=(renderStreamFactory: RenderStreamFactory){
    pendingRunnables.enqueue(() => {
      if (renderStream ne null)
        renderStream.destroy
      m_renderStreamFactory = renderStreamFactory
      renderStream = renderStreamFactory.create(renderStream.getHandler, samples, transfersToBuffer)
    })
  }

  private def resetStreams {
    pendingRunnables.enqueue(() => {
      renderStream.destroy
      renderStream = renderStreamFactory.create(renderStream.getHandler, samples, transfersToBuffer)
      updateSnapshot()
    })
  }

  def updateSnapshot() {
    snapshotRequest += 1
  }


  override def swapBuffers() {
    renderStream.swapBuffers()
  }

  override def extraLoopOperation() {

    while ( !pendingRunnables.isEmpty ) {
      val runable = pendingRunnables.dequeue()
      runable()
    }

    renderStream.bind()

    // these updates need to be done from the javaFx thread
    guiController.runLater {
      guiController.playerPosition.setText( "Player Position: " + round10(Player.position) )
      guiController.drawcalls.setText( "drawcalls: " + Renderer.drawCalls + ", empty: " + Renderer.emptyDrawCalls + "" )
      //guiController.frustumCulledNodes.setText( "frustum culled nodes: " + World.frustumculls )
    }
  }

  override def destroy() {
    super.destroy()
    renderStream.destroy()
    pbuffer.destroy()
  }
}
