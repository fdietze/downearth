/**
 * User: arne
 * Date: 23.04.13
 * Time: 16:12
 */

package openworld

import java.nio.ByteBuffer
import java.util.concurrent.Semaphore
import javafx.application.{Platform, Application}
import javafx.event.{EventHandler, ActionEvent}
import javafx.scene.control.{Label, Button}
import javafx.scene.image.{PixelFormat, WritableImage, ImageView}
import javafx.scene.layout.AnchorPane
import javafx.scene.Scene
import javafx.stage.{WindowEvent, Stage}
import org.lwjgl.util.stream.{StreamUtil, StreamHandler}
import org.lwjgl.opengl.GL11._

object JavaFxMain {
  var application:JavafxTest = null

  def width = {
    if( (application eq null) || (application.renderImage eq null) )
      640.0
    else
      application.renderImage.getWidth
  }

  def height = {
    if( (application eq null) || (application.renderImage eq null) )
      480.0
    else
      application.renderImage.getHeight
  }

  def main(args: Array[String]) {
    Application.launch(classOf[JavafxTest],args:_*)
  }
}

class JavafxTest extends Application {
  var renderImage: WritableImage = null

  override def start(primaryStage:Stage) {
    JavaFxMain.application = this

    primaryStage.setTitle("Hello World!")
    val root = new AnchorPane
    val children = root.getChildren

    val btn = new Button
    btn.setText("Say 'Hello World'")
    btn.setOnAction(new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent) {
        System.out.println("Hello World!")
      }
    })
    AnchorPane.setLeftAnchor(btn, 10)
    AnchorPane.setTopAnchor(btn, 10)

    val gameView = new ImageView;
    gameView.setFitHeight(256)
    gameView.setFitWidth(256)
    gameView.setPickOnBounds(true)
    gameView.setPreserveRatio(false)
    gameView.setScaleY(-1)
    gameView.setSmooth(false)

    AnchorPane.setBottomAnchor(gameView, 0.0)
    AnchorPane.setLeftAnchor(gameView, 0.0)
    AnchorPane.setRightAnchor(gameView, 0.0)
    AnchorPane.setTopAnchor(gameView, 0.0)

    val text = new Label()
    text.setText("Hallo Welt")
    AnchorPane.setRightAnchor(text, 10.0)
    AnchorPane.setBottomAnchor(text, 10.0)

    children.addAll(gameView, btn, text)



    def readHandler = new StreamHandler {
      override def getWidth = gameView.getFitWidth.toInt
      override def getHeight = gameView.getFitHeight.toInt

      override def process(width: Int, height:Int, data:ByteBuffer, stride:Int, signal:Semaphore) {
        // This method runs in the background rendering thread
        // TODO: Run setPixels on the PlatformImage in this thread, run pixelsDirty on JFX application thread with runLater.
        Platform.runLater(new Runnable() {
          override def run() {
            try {
              // If we're quitting, discard update
              if ( !gameView.isVisible )
                return

              // Detect resize and recreate the image
              if ( renderImage == null || renderImage.getWidth.toInt != width || renderImage.getHeight.toInt != height ) {
                renderImage = new WritableImage(width, height)
                gameView.setImage(renderImage)
              }
              // Upload the image to JavaFX
              renderImage.getPixelWriter.setPixels(0, 0, width, height, PixelFormat.getByteBgraPreInstance, data, stride)
            } finally {
              // Notify the render thread that we're done processing
              signal.release()
            }
          }
        })
      }
    }

    gameView.fitWidthProperty.bind(root.widthProperty)
    gameView.fitHeightProperty.bind(root.heightProperty)

    var game:GameLoop = null

    def runGame() {
      game = new GameLoop( readHandler )

      val renderStreamFactories = StreamUtil.getRenderStreamImplementation
      val textureStreamFactoris = StreamUtil.getTextureStreamImplementations

      val vendor = glGetString(GL_VENDOR)
      val version = glGetString(GL_VERSION)

      game.run()
    }

    val renderer = new Thread("LWJGL Renderer") {
      override def run() {
        runGame()
        Platform.runLater(new Runnable {
          def run() {
            primaryStage.close()
          }
        })
      }
    }

    primaryStage.setOnCloseRequest( new EventHandler[WindowEvent] {
      def handle(p1: WindowEvent) {
        p1.consume()
        game.finished = true
      }
    })

    renderer.start()

    primaryStage.setScene(new Scene(root, 640, 480))
    primaryStage.show()
  }
}
