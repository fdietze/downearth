import sbt._
import scalabuff.ScalaBuffPlugin._
import Keys._

object build extends Build {

  lazy val glWrapper = RootProject(uri("git://github.com/krux02/scalagl.git#stable"))
  // git@github.com:krux02/scalagl.git

  val benchmark = TaskKey[Unit]("benchmark")
  benchmark <<= benchmark.dependsOn(compile)
  val benchmarkTask = fullRunTask(benchmark, Test, "downearth.benchmark.Main")


  lazy val root = Project(
    "main",
    file("."),
    settings = Defaults.defaultSettings ++ scalabuffSettings ++ Seq[Setting[_]](benchmarkTask)
  ).dependsOn(glWrapper).configs(ScalaBuff)
}

