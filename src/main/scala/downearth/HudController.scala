/**
 * User: arne
 * Date: 23.04.13
 * Time: 16:12
 */

package downearth

import java.nio.ByteBuffer
import java.util.concurrent.Semaphore
import javafx.application.{Platform, Application}
import javafx.event.{EventHandler, ActionEvent}
import javafx.scene.control.{Label, Button}
import javafx.scene.image.{PixelFormat, WritableImage, ImageView}
import javafx.scene.layout.{TilePane, GridPane, Pane, AnchorPane}
import javafx.scene.Scene
import javafx.stage.{WindowEvent, Stage}
import org.lwjgl.util.stream.{StreamUtil, StreamHandler}
import org.lwjgl.opengl.GL11._
import javafx.fxml.{Initializable, FXMLLoader, FXML}
import java.util.ResourceBundle
import java.net.URL
import javafx.scene.input.{KeyEvent, KeyCombination, KeyCode, KeyCodeCombination}
import javafx.collections.ObservableMap

object JavaFxMain  {
  var controller:HudController = null

  def width = {
    if( (controller eq null) || (controller.renderImage eq null) )
      640.0
    else
      controller.renderImage.getWidth
  }

  def height = {
    if( (controller eq null) || (controller.renderImage eq null) )
      480.0
    else
      controller.renderImage.getHeight
  }

  def main(args: Array[String]) {
    Application.launch(classOf[JavaFxApplication],args:_*)
  }
}

class JavaFxApplication extends Application {
  override def start(primaryStage:Stage) {
    primaryStage.setTitle("Hello World!")


    val url = getClass.getResource("hud.fxml")
    val loader  = new FXMLLoader(url)
    val content = loader.load().asInstanceOf[Pane]
    val controller = loader.getController[HudController]

    val renderer = new Thread("LWJGL Renderer") {
      override def run() {
        controller.runGame()
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
        controller.game.finished = true
      }
    })

    renderer.start()

    primaryStage.setScene(new Scene(content, 640, 480))
    primaryStage.show()
  }
}

class HudController extends Initializable {
  var renderImage: WritableImage = null

  def handleButtonAction(event: ActionEvent) {
    println("Hello World!")
  }

  @FXML var root:AnchorPane = _
  @FXML var gameView:ImageView = _

  @FXML var drawcalls:Label = _
  @FXML var frustumCulledNodes:Label = _
  @FXML var playerPosition:Label = _

  @FXML var forward:Button = _
  @FXML var backward:Button = _
  @FXML var left:Button = _
  @FXML var right:Button = _

  @FXML var inventory:GridPane = _

  override def initialize(url: URL, resourceBundle: ResourceBundle ) {
    JavaFxMain.controller = this
    gameView.fitWidthProperty.bind(root.widthProperty)
    gameView.fitHeightProperty.bind(root.heightProperty)

    val  keyEventHandler = new EventHandler[KeyEvent] {
      println("constructor")
      override def handle(keyEvent: KeyEvent) {

        keyEvent.getEventType match {
        case KeyEvent.KEY_PRESSED =>
          keyEvent.getCode match {
          case KeyCode.L =>
            println( "up pressed" )
          case KeyCode.A =>
            println( "down pressed" )
          case KeyCode.I =>
            println( "left pressed" )
          case KeyCode.E =>
            println( "right pressed" )
          case _ =>
            println( "unknown key pressed" )
          }
        case KeyEvent.KEY_RELEASED =>
          keyEvent.getCode match {
          case KeyCode.L =>
            println( "up released" )
          case KeyCode.A =>
            println( "down released" )
          case KeyCode.I =>
            println( "left released" )
          case KeyCode.E =>
            println( "right released" )
          case _ =>
            println( "unknown key released" )
          }
        case _ =>
          println("unknown key event")
        }

        keyEvent.consume()
      }
    }

//    gameView.setOnKeyPressed(keyEventHandler)
//    gameView.setOnKeyReleased(keyEventHandler)

    root.addEventHandler( KeyEvent.KEY_PRESSED, keyEventHandler )
    root.addEventHandler( KeyEvent.KEY_RELEASED, keyEventHandler )

  }

  def runLater( arg: => Unit ) {
    Platform.runLater( new Runnable {
      def run() {
        arg
      }
    })
  }

  lazy val readHandler = new StreamHandler {
    override def getWidth = gameView.getFitWidth.toInt
    override def getHeight = gameView.getFitHeight.toInt

    override def process(width: Int, height:Int, data:ByteBuffer, stride:Int, signal:Semaphore) {
      // This method runs in the background rendering thread
      // TODO: Run setPixels on the PlatformImage in this thread, run pixelsDirty on JFX application thread with runLater.
      runLater {
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
    }
  }

  lazy val game = new GameLoop( readHandler, this )

  def runGame() {

//    val renderStreamFactories = StreamUtil.getRenderStreamImplementation
//    val textureStreamFactoris = StreamUtil.getTextureStreamImplementations

//    val vendor = glGetString(GL_VENDOR)
//    val version = glGetString(GL_VERSION)

    game.run()
  }


}
