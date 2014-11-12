import sbtprotobuf.{ProtobufPlugin=>PB}

name := "downearth"

organization := "fdad"

version := "0.2.0"

scalaVersion := "2.10.4"

mainClass in (Compile, run) := Some("downearth.Main")

resolvers ++= Seq(
  //"Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
  //"Slick" at "http://slick.cokeandcode.com/mavenrepo"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.2" % "test",
  "com.typesafe.akka" %% "akka-actor" % "2.2.4",
  //"org.scala-lang" % "scala-reflect" % "2.10.1",
  "org.simplex3d" %% "simplex3d-math-double" % "2.4.7",
  "org.simplex3d" %% "simplex3d-math-float" % "2.4.7",
  //"slick" % "slick" % "274",
  "java3d" % "vecmath" % "1.3.1"
  // "com.google.protobuf" % "protobuf-java" % "2.5.0"
)

//net.virtualvoid.sbt.graph.Plugin.graphSettings

//filterScalaLibrary := false

seq( LWJGLPlugin.lwjglSettings: _*)

seq(PB.protobufSettings: _*)

version in protobufConfig := "2.6.1"

lwjgl.version := "2.9.1"

scalacOptions ++= Seq(
  "-unchecked", 
  "-deprecation", 
  "-feature", 
  "-Yinline", "-Yinline-warnings",
  "-language:_"
  //,"-Xdisable-assertions", "-optimize"
)

javaOptions in run ++= Seq(
  "-XX:+ExplicitGCInvokesConcurrent",
  "-Xmx512m",
  "-Dorg.lwjgl.util.Debug=true",
//  "-Dorg.lwjgl.opengl.Window.undecorated=true",
  "-Dmyprocessname"
)

initialCommands := """
import simplex3d.math._
import simplex3d.math.double._
import simplex3d.math.double.functions._
"""
