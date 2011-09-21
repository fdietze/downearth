name := "worldgen"

organization := "fdad"

version := "0.2.0"

scalaVersion := "2.9.1"

seq( LWJGLProject.engineSettings: _*)

libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "test"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases"

libraryDependencies += "se.scalablesolutions.akka" % "akka-actor" % "1.1.3" 

javaOptions in run += "-XX:+ExplicitGCInvokesConcurrent"

javaOptions in run += "-Xmx512m"

javaOptions in run += "-Xms512m"

//scalacOptions in Compile += "-g:none" // keine debug-infos

//scalacOptions in Compile += "-optimise"

//javaOptions in run += "-XX:+UseCompressedOops"
