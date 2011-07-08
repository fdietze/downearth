import sbt._
import java.io.File

class Project(info: ProjectInfo) extends LWJGLProject(info) with IdeaProject{
	lazy val testlib = "org.scalatest" % "scalatest_2.9.0" % "1.4.1"
	
	override def localScala = List(
		defineScala("2.9.0.1-local", new File("/home/arne/local/scala-2.9.0.1") ),
		defineScala("current-local", new File("/home/arne/local/scala") )
	)
	
	
	override def fork = {
		forkRun( ("-Djava.library.path=" + nativeLWJGLPath) :: Nil)
	}
	//TODO testen
	//override def compileOptions = super.compileOptions ++ compileOptions("–Xdisable-assertions")
	//override def compileOptions = super.compileOptions ++ compileOptions("-optimise") ++ compileOptions("–Xdisable-assertions")
	//override def compileOptions = super.compileOptions ++ compileOptions("-Ylog:icode -Ydebug")
}

