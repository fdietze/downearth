package downearth.gui.javafx

import javafx.stage.{WindowEvent, Stage}
import javafx.fxml.FXMLLoader
import javafx.scene.layout.Pane
import javafx.application.Platform
import javafx.event.EventHandler
import javafx.scene.Scene
import javafx.application.Application

/**
 * User: arne
 * Date: 29.04.13
 * Time: 00:49
 */
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
