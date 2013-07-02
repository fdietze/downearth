import sbt._
import scalabuff.ScalaBuffPlugin._
import Keys._

object build extends Build {
  lazy val root = Project(
    "main",
    file("."),
    settings = Defaults.defaultSettings ++ scalabuffSettings ++ Seq(benchmarkTask)
    ).configs(ScalaBuff)
  

  val benchmark = TaskKey[Unit]("benchmark")
  benchmark <<= benchmark.dependsOn(compile)
  val benchmarkTask = fullRunTask(benchmark, Test, "downearth.benchmark.Main")
}
