addSbtPlugin("io.get-coursier" % "sbt-coursier" % coursier.util.Properties.version)

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.18")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin-shaded" % "0.8.0-RC1"