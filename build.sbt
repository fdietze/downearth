name := "downearth"

organization := "fdad"

version := "0.2.0"

scalaVersion := "2.10.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "1.9.1" % "test",
	"com.typesafe.akka" %% "akka-actor" % "2.2-M2",
	"org.scala-lang" % "scala-reflect" % "2.10.1"
)

seq( LWJGLPlugin.lwjglSettings: _*)

lwjgl.version := "2.9.0"

scalacOptions ++= Seq(
    "-unchecked", 
    "-deprecation", 
    "-feature", 
    "-Yinline-warnings", 
    "-language:_"
)

javaOptions in run ++= Seq(
    "-XX:+ExplicitGCInvokesConcurrent",
    "-XX:+DoEscapeAnalysis",
    "-Xmx512m"
)

//initialCommands := """
//import simplex3d.math._
//import simplex3d.math.float._
//import simplex3d.math.float.functions._
//import ._
//"""
