import sbt._
import scalabuff.ScalaBuffPlugin._

object build extends Build {
  lazy val root = Project("main", file("."), settings = Defaults.defaultSettings ++ scalabuffSettings).configs(ScalaBuff)
}
