import sbt._
import Keys._

object build extends Build {

  lazy val glWrapper = RootProject(uri("git://github.com/krux02/scalagl.git#stable"))

  lazy val root = Project(
    "main",
    file("."),
    settings = Defaults.defaultSettings ++ (benchmarkTask)
  ).dependsOn(glWrapper)

  val benchmark = TaskKey[Unit]("benchmark")
  benchmark <<= benchmark.dependsOn(compile)
  val benchmarkTask = fullRunTask(benchmark, Test, "downearth.benchmark.Main")
}

