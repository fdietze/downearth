import sbt._

class Plugin(info: ProjectInfo) extends PluginDefinition(info) {
	val lwjglVersion = "com.github.philcali" % "sbt-lwjgl-plugin" % "2.0.4"
	val sbtIdeaRepo = "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
	val sbtIdea = "com.github.mpeltonen" % "sbt-idea-plugin" % "0.4.0"
}

