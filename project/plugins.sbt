//libraryDependencies <+= (sbtVersion) { sv => 
//  "com.github.philcali" %% "sbt-lwjgl-plugin" % ("sbt" + sv + "_3.0.4")
//}

//resolvers += Classpaths.typesafeResolver

resolvers += Classpaths.typesafeResolver

addSbtPlugin("com.github.philcali" % "sbt-lwjgl-plugin" % "3.1.4")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.2")

