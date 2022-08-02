import org.scalajs.linker.interface.ModuleSplitStyle
import sbtcrossproject.CrossPlugin.autoImport.{ CrossType, crossProject }
import sbt._
import sbt.io.Using

val versions: Map[String, String] = {
  import org.snakeyaml.engine.v2.api.{ Load, LoadSettings }
  import java.util.{ List => JList, Map => JMap }
  import scala.jdk.CollectionConverters._
  val doc  = new Load(LoadSettings.builder().build())
    .loadFromReader(scala.io.Source.fromFile(".github/workflows/scala.yml").bufferedReader())
  val yaml = doc.asInstanceOf[
    JMap[String, JMap[String, JMap[String, JMap[String, JMap[String, JList[String]]]]]]
  ]
  val list = yaml.get("jobs").get("test").get("strategy").get("matrix").get("scala").asScala
  list.map { v =>
    val vs = v.split('.'); val init = vs.take(vs(0) match { case "2" => 2; case _ => 1 });
    (init.mkString("."), v)
  }.toMap
}

val scalaVer                = versions("2.13")
val scala3Ver               = versions("3")
val tzdbVersion             = "2019c"
val scalajavaLocalesVersion = "1.4.1"
Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val downloadFromZip: TaskKey[Unit] =
  taskKey[Unit]("Download the tzdb tarball and extract it")

addCommandAlias("validate", ";clean;scalajavatimeTestsJVM/test;scalajavatimeTestsJS/test")
addCommandAlias("demo", ";clean;demo/fastOptJS;demo/fullOptJS")

inThisBuild(
  List(
    organization := "io.github.cquiroz",
    homepage     := Some(url("https://github.com/cquiroz/scala-java-time")),
    licenses     := Seq("BSD 3-Clause License" -> url("https://opensource.org/licenses/BSD-3-Clause")),
    developers   := List(
      Developer("cquiroz",
                "Carlos Quiroz",
                "carlos.m.quiroz@gmail.com",
                url("https://github.com/cquiroz")
      )
    ),
    scmInfo      := Some(
      ScmInfo(
        url("https://github.com/cquiroz/scala-java-time"),
        "scm:git:git@github.com:cquiroz/scala-java-time.git"
      )
    )
  )
)

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .settings(
    publish / skip := true
  )
  .aggregate(
    core.js,
    core.jvm,
    core.native,
    tzdb.js,
    tzdb.jvm,
    tzdb.native,
    tests.js,
    tests.jvm,
    tests.native,
    demo.js,
    demo.jvm,
    demo.native
  )

lazy val commonSettings = Seq(
  description                     := "java.time API implementation in Scala and Scala.js",
  scalaVersion                    := scalaVer,
  crossScalaVersions              := versions.toList.map(_._2),
  // Don't include threeten on the binaries
  Compile / packageBin / mappings := (Compile / packageBin / mappings).value.filter { case (_, s) =>
    !s.contains("threeten")
  },
  Compile / scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor == 13 =>
        Seq("-deprecation:false")
      case _                                         =>
        Seq.empty
    }
  },
  Compile / doc / scalacOptions   := {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor >= 11 =>
        Seq("-deprecation:false")
      case _                                         =>
        Seq.empty
    }
  },
  scalacOptions ++= { if (isDotty.value) Seq.empty else Seq("-target:jvm-1.8") },
  scalacOptions --= {
    if (isDotty.value)
      List(
        "-Xfatal-warnings"
      )
    else
      List(
      )
  },
  javaOptions ++= Seq("-Dfile.encoding=UTF8"),
  autoAPIMappings                 := true,
  Compile / doc / sources         := { if (isDotty.value) Seq() else (Compile / doc / sources).value }
)

/**
 * Copy source files and translate them to the java.time package
 */
def copyAndReplace(srcDirs: Seq[File], destinationDir: File): Seq[File] = {
  // Copy a directory and return the list of files
  def copyDirectory(
    source:               File,
    target:               File,
    overwrite:            Boolean = false,
    preserveLastModified: Boolean = false
  ): Set[File] =
    IO.copy(PathFinder(source).allPaths.pair(Path.rebase(source, target)).toTraversable,
            overwrite,
            preserveLastModified,
            false
    )

  val onlyScalaDirs                      = srcDirs.filter(_.getName.matches(".*scala(-\\d)?"))
  // Copy the source files from the base project, exclude classes on java.util and dirs
  val generatedFiles: List[java.io.File] = onlyScalaDirs
    .foldLeft(Set.empty[File]) { (files, sourceDir) =>
      files ++ copyDirectory(sourceDir, destinationDir, overwrite = true)
    }
    .filterNot(_.isDirectory)
    .filter(_.getName.endsWith(".scala"))
    .filterNot(_.getParentFile.getName == "util")
    .toList

  // These replacements will in practice rename all the classes from
  // org.threeten to java.time
  def replacements(line: String): String =
    line
      .replaceAll("package org.threeten$", "package java")
      .replaceAll("package object bp", "package object time")
      .replaceAll("package org.threeten.bp", "package java.time")
      .replaceAll("""import org.threeten.bp(\..*)?(\.[A-Z_{][^\.]*)""", "import java.time$1$2")
      .replaceAll("import zonedb.threeten", "import zonedb.java")
      .replaceAll("private\\s*\\[bp\\]", "private[time]")

  // Visit each file and read the content replacing key strings
  generatedFiles.foreach { f =>
    val replacedLines = IO.readLines(f).map(replacements)
    IO.writeLines(f, replacedLines)
  }
  generatedFiles
}

lazy val core = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "scala-java-time",
    libraryDependencies += ("org.portable-scala" %%% "portable-scala-reflect" % "1.1.2")
      .cross(CrossVersion.for3Use2_13)
  )
  .jsSettings(
    scalacOptions ++= {
      if (isDotty.value) Seq("-scalajs-genStaticForwardersForNonTopLevelObjects")
      else Seq("-P:scalajs:genStaticForwardersForNonTopLevelObjects")
    },
    scalacOptions ++= {

      if (isDotty.value) Seq.empty
      else {
        val tagOrHash =
          if (isSnapshot.value) sys.process.Process("git rev-parse HEAD").lineStream_!.head
          else s"v${version.value}"
        (Compile / sourceDirectories).value.map { f =>
          val a = f.toURI.toString
          val g =
            "https://raw.githubusercontent.com/cquiroz/scala-java-time/" + tagOrHash + "/shared/src/main/scala/"
          s"-P:scalajs:mapSourceURI:$a->$g/"
        }
      }
    },
    Compile / sourceGenerators += Def.task {
      val srcDirs        = (Compile / sourceDirectories).value
      val destinationDir = (Compile / sourceManaged).value
      copyAndReplace(srcDirs, destinationDir)
    }.taskValue,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-locales" % scalajavaLocalesVersion
    )
  )
  .nativeSettings(
    Compile / sourceGenerators += Def.task {
      val srcDirs        = (Compile / sourceDirectories).value
      val destinationDir = (Compile / sourceManaged).value
      copyAndReplace(srcDirs, destinationDir)
    }.taskValue,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-locales" % scalajavaLocalesVersion
    )
  )

lazy val tzdb = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("tzdb"))
  .settings(commonSettings)
  .settings(
    name        := "scala-java-time-tzdb",
    includeTTBP := true
  )
  .jsSettings(
    dbVersion := TzdbPlugin.Version(tzdbVersion),
    Compile / sourceGenerators += Def.task {
      val srcDirs        = (Compile / sourceManaged).value
      val destinationDir = (Compile / sourceManaged).value
      copyAndReplace(Seq(srcDirs), destinationDir)
    }.taskValue
  )
  .nativeSettings(
    dbVersion    := TzdbPlugin.Version(tzdbVersion),
    tzdbPlatform := TzdbPlugin.Platform.Native,
    Compile / sourceGenerators += Def.task {
      val srcDirs        = (Compile / sourceManaged).value
      val destinationDir = (Compile / sourceManaged).value
      copyAndReplace(Seq(srcDirs), destinationDir)
    }.taskValue
  )
  .jvmSettings(
    tzdbPlatform := TzdbPlugin.Platform.Jvm
  )
  .dependsOn(core)
  .enablePlugins(TzdbPlugin)

lazy val tests = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("tests"))
  .settings(commonSettings)
  .settings(
    name               := "tests",
    publish / skip     := true,
    Keys.`package`     := file(""),
    libraryDependencies +=
      "org.scalatest" %%% "scalatest" % "3.2.13" % Test,
    scalacOptions ~= (_.filterNot(
      Set("-Wnumeric-widen", "-Ywarn-numeric-widen", "-Ywarn-value-discard", "-Wvalue-discard")
    ))
  )
  .jvmSettings(
    // Fork the JVM test to ensure that the custom flags are set
    Test / fork                        := true,
    Test / baseDirectory               := baseDirectory.value.getParentFile,
    // Use CLDR provider for locales
    // https://docs.oracle.com/javase/8/docs/technotes/guides/intl/enhancements.8.html#cldr
    Test / javaOptions ++= Seq("-Duser.language=en",
                               "-Duser.country=US",
                               "-Djava.locale.providers=CLDR"
    ),
    Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
  )
  .jsSettings(
    Test / parallelExecution := false,
    Test / sourceGenerators += Def.task {
      val srcDirs        = (Test / sourceDirectories).value
      val destinationDir = (Test / sourceManaged).value
      copyAndReplace(srcDirs, destinationDir)
    }.taskValue,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "locales-full-db" % scalajavaLocalesVersion
    )
  )
  .nativeSettings(
    Test / parallelExecution := false,
    Test / sourceGenerators += Def.task {
      val srcDirs        = (Test / sourceDirectories).value
      val destinationDir = (Test / sourceManaged).value
      copyAndReplace(srcDirs, destinationDir)
    }.taskValue,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "locales-full-db" % scalajavaLocalesVersion
    )
  )
  .dependsOn(core, tzdb)

val zonesFilterFn = (x: String) => x == "Europe/Helsinki" || x == "America/Santiago"

lazy val demo = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("demo"))
  .dependsOn(core)
  .enablePlugins(TzdbPlugin)
  .settings(
    scalaVersion   := scalaVer,
    name           := "demo",
    publish / skip := true,
    Keys.`package` := file(""),
    zonesFilter    := zonesFilterFn
  )
  .jsSettings(
    // scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    // scalaJSLinkerConfig ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallestModules)),
    scalaJSUseMainModuleInitializer := true
  )
  .jvmSettings(
    tzdbPlatform := TzdbPlugin.Platform.Jvm,
    Compile / scalacOptions -= "-Xfatal-warnings"
  )
  .nativeSettings(
    tzdbPlatform := TzdbPlugin.Platform.Native,
    // demo/native/target/scala-2.13/src_managed/main/tzdb/tzdb_java.scala:21:30: object JavaConverters in package collection is deprecated (since 2.13.0): Use `scala.jdk.CollectionConverters` instead
    //    ZoneRules.of(bso, bwo, standardTransitions asJava, transitionList asJava, lastRules asJava)
    //                           ^
    Compile / scalacOptions -= "-Xfatal-warnings"
  )

// lazy val docs = project
//   .in(file("docs"))
//   .dependsOn(scalajavatime.jvm, scalajavatime.js)
//   .settings(commonSettings)
//   .settings(name := "docs")
//   .enablePlugins(MicrositesPlugin)
//   .settings(
//     micrositeName := "scala-java-time",
//     micrositeAuthor := "Carlos Quiroz",
//     micrositeGithubOwner := "cquiroz",
//     micrositeGithubRepo := "scala-java-time",
//     micrositeBaseUrl := "/scala-java-time",
//     micrositePushSiteWith := GitHub4s,
//     //micrositeDocumentationUrl := "/scala-java-time/docs/",
//     micrositeHighlightTheme := "color-brewer",
//     micrositeGithubToken := Option(System.getProperty("GH_TOKEN"))
//   )
