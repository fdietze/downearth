name := "worldgen"

organization := "fdad"

version := "0.2.0"

scalaVersion := "2.9.1"

seq( LWJGLPlugin.lwjglSettings: _*)

javaOptions in run += "-XX:+ExplicitGCInvokesConcurrent"

javaOptions in run += "-Xmx512m"

//fork in run := true

scalacOptions in Compile += "-unchecked"

scalacOptions in Compile += "-deprecation"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

//initialCommands := """
//import simplex3d.math._
//import simplex3d.math.float._
//import simplex3d.math.float.functions._
//import openworld._
//"""

