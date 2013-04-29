package downearth.gui.javafx

import javafx.application.Application

/**
 * User: arne
 * Date: 29.04.13
 * Time: 00:49
 */
class JavaFxMain {
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
