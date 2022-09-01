addSbtPlugin("com.47deg" % "sbt-microsites" % "1.3.4")

val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.10.1")

addSbtPlugin("org.portable-scala" % "sbt-crossproject" % "1.2.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")

addSbtPlugin("io.github.cquiroz" % "sbt-tzdb" % "3.0.0")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.3.1")

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.10")

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.5.5")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.2.0")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.5")

libraryDependencies += "org.snakeyaml" % "snakeyaml-engine" % "2.4"
