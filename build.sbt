import sbtprotobuf.{ProtobufPlugin => PB}

// Was `TaskKey[Unit]("benchmark")` + `fullRunTask` in the old build.scala;
// both APIs are gone in sbt 1.x. Reimplemented with the modern `.toTask`
// pattern to invoke Test's `runMain` with a fixed main class.
lazy val benchmark = taskKey[Unit]("Run the downearth benchmark suite")

// glWrapper (github.com/krux02/scalagl) used to be a git-based sbt
// sub-project dependency, but its own build also depended on the dead
// sbt-lwjgl-plugin and on the retired git:// protocol. Vendored instead
// under src/main/scala/glwrapper/ -- see the README.md there for why.
lazy val root = (project in file("."))
  .enablePlugins(PB)
  .settings(
    name := "downearth",
    organization := "fdad",
    version := "0.2.0",
    scalaVersion := "2.10.4",

    Compile / run / mainClass := Some("downearth.Main"),

    benchmark := (Test / runMain).toTask(" downearth.benchmark.Main").value,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.2" % "test",
      "com.typesafe.akka" %% "akka-actor" % "2.2.4",
      "org.simplex3d" %% "simplex3d-math-double" % "2.4.7",
      "org.simplex3d" %% "simplex3d-math-float" % "2.4.7",
      "java3d" % "vecmath" % "1.3.1",

      // LWJGL: sbt-lwjgl-plugin is unmaintained/sbt-1.x-incompatible, so we
      // depend on the LWJGL maven artifacts directly instead (same jars the
      // plugin used to pull in under the hood). Native extraction below
      // replaces the plugin's automatic unpacking step.
      "org.lwjgl.lwjgl" % "lwjgl" % "2.9.1",
      "org.lwjgl.lwjgl" % "lwjgl_util" % "2.9.1",
      "org.lwjgl.lwjgl" % "lwjgl-platform" % "2.9.1" classifier "natives-linux" classifier "natives-windows" classifier "natives-osx"
    ),

    ProtobufConfig / version := "2.6.1",

    scalacOptions ++= Seq(
      "-unchecked",
      "-deprecation",
      "-feature",
      "-Yinline", "-Yinline-warnings",
      "-language:_"
    ),

    // fork is required for javaOptions (incl. java.library.path for LWJGL
    // natives below) to actually reach the running JVM
    run / fork := true,

    run / javaOptions ++= Seq(
      "-XX:+ExplicitGCInvokesConcurrent",
      "-Xmx512m",
      "-Dorg.lwjgl.util.Debug=true",
      "-Dmyprocessname"
    ),

    // Extract the OS-specific LWJGL native libraries (.so/.dll/.dylib) from
    // the natives jar into target/natives, and point java.library.path at
    // them. This replaces what sbt-lwjgl-plugin used to do automatically.
    Compile / resourceGenerators += Def.task {
      val dir = (Compile / target).value / "natives"
      val nativesJars = (Compile / dependencyClasspath).value
        .map(_.data)
        .filter(_.getName.contains("natives"))
      IO.createDirectory(dir)
      nativesJars.foreach(jar => IO.unzip(jar, dir))
      Nil // no resources added to the classpath; natives are loaded via java.library.path below
    }.taskValue,
    run / javaOptions += s"-Djava.library.path=${(Compile / target).value / "natives"}",

    initialCommands := """
      import simplex3d.math._
      import simplex3d.math.double._
      import simplex3d.math.double.functions._
    """
  )
