name := "worldgen"

organization := "fdad"

version := "0.2.0"

scalaVersion := "2.9.1"

seq( LWJGLProject.engineSettings: _*)

javaOptions in run += "-XX:+ExplicitGCInvokesConcurrent"

javaOptions in run += "-Xmx512m"

scalacOptions in Compile += "-unchecked"

scalacOptions in Compile += "-deprecation"

