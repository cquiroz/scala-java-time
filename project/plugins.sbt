resolvers += Resolver.sonatypeRepo("public")

addSbtPlugin("com.47deg" % "sbt-microsites" % "1.3.2")

val scalaJSVersion =
  Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.5.0")

addSbtPlugin("org.portable-scala" % "sbt-crossproject" % "1.0.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

addSbtPlugin("io.github.cquiroz" % "sbt-tzdb" % "1.0.1")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2")

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.17")

addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.6")

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.5.3")
