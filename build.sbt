name := "worldgen"

organization := "fdad"

version := "0.2.0"

scalaVersion := "2.10.1"

seq( LWJGLPlugin.lwjglSettings: _*)

javaOptions in run += "-XX:+ExplicitGCInvokesConcurrent"

javaOptions in run += "-Xmx512m"

//fork in run := true

scalacOptions in Compile += "-unchecked"

scalacOptions in Compile += "-deprecation"

scalacOptions in Compile += "-feature"

scalacOptions in Compile += "-language:implicitConversions"

// scalacOptions in Compile += "-language:reflectiveCalls"

// scalacOptions in Compile += "-language:postfixOps"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "1.9.1" % "test"
//	"com.nativelibs4java" % "scalacl" % "0.2"
)

// ScalaCL:

// resolvers += "NativeLibs4Java Repository" at "http://nativelibs4java.sourceforge.net/maven/"

// autoCompilerPlugins := true

// addCompilerPlugin("com.nativelibs4java" % "scalacl-compiler-plugin" % "0.2")

//initialCommands := """
//import simplex3d.math._
//import simplex3d.math.float._
//import simplex3d.math.float.functions._
//import openworld._
//"""

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.2"
