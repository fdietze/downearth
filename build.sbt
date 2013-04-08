name := "worldgen"

organization := "fdad"

version := "0.2.0"

scalaVersion := "2.10.1"

seq( LWJGLPlugin.lwjglSettings: _*)

javaOptions in run += "-XX:+ExplicitGCInvokesConcurrent"

javaOptions in run += "-Xmx512m"

//fork in run := true

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Yinline-warnings")

scalacOptions ++= Seq("-language:_")

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "1.9.1" % "test"
	//"com.nativelibs4java" % "scalacl" % "0.2"
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

