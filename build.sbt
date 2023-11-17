val scala213 = "2.13.10"
val scala3   = "3.2.1"
ThisBuild / scalaVersion       := scala213
ThisBuild / crossScalaVersions := Seq("2.12.17", scala213, scala3)

ThisBuild / tlBaseVersion := "2.5"

val temurin11 = JavaSpec.temurin("11")
ThisBuild / githubWorkflowJavaVersions := Seq(temurin11)

ThisBuild / githubWorkflowBuildMatrixExclusions ++= Seq(
  MatrixExclude(Map("scala" -> scala3, "project" -> "rootJVM")), // TODO
  MatrixExclude(
    Map("scala" -> scala3, "project" -> "rootNative", "os" -> "ubuntu-latest")
  ) // run on macOS instead
)

ThisBuild / githubWorkflowBuildMatrixInclusions +=
  MatrixInclude(Map("scala" -> scala3, "java" -> temurin11.render, "project" -> "rootNative"),
                Map("os"    -> "macos-latest")
  )

val tzdbVersion             = "2019c"
val scalajavaLocalesVersion = "1.5.1"
Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val downloadFromZip: TaskKey[Unit] =
  taskKey[Unit]("Download the tzdb tarball and extract it")

inThisBuild(
  List(
    organization            := "io.github.cquiroz",
    licenses                := Seq("BSD 3-Clause License" -> url("https://opensource.org/licenses/BSD-3-Clause")),
    developers              := List(
      Developer("cquiroz",
                "Carlos Quiroz",
                "carlos.m.quiroz@gmail.com",
                url("https://github.com/cquiroz")
      )
    ),
    tlSonatypeUseLegacyHost := true,
    tlMimaPreviousVersions  := Set(),
    tlCiReleaseBranches     := Seq("master"),
    tlCiHeaderCheck         := false
  )
)

lazy val root = tlCrossRootProject.aggregate(core, tzdb, tests, demo)

lazy val commonSettings = Seq(
  description                     := "java.time API implementation in Scala and Scala.js",
  versionScheme                   := Some("always"),
  // Don't include threeten on the binaries
  Compile / packageBin / mappings := (Compile / packageBin / mappings).value.filter { case (_, s) =>
    !s.contains("threeten")
  },
  Compile / packageSrc / mappings := (Compile / packageSrc / mappings).value.filter { case (_, s) =>
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
  scalacOptions --= {
    if (tlIsScala3.value)
      List(
        "-Xfatal-warnings",
        "-source:3.0-migration"
      )
    else
      List(
      )
  },
  javaOptions ++= Seq("-Dfile.encoding=UTF8"),
  Compile / doc / sources         := Seq()
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
    name                                          := "scala-java-time",
    libraryDependencies += ("org.portable-scala" %%% "portable-scala-reflect" % "1.1.2")
      .cross(CrossVersion.for3Use2_13)
  )
  .jsSettings(
    scalacOptions ++= {
      if (tlIsScala3.value) Seq("-scalajs-genStaticForwardersForNonTopLevelObjects")
      else Seq("-P:scalajs:genStaticForwardersForNonTopLevelObjects")
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
    name            := "scala-java-time-tzdb",
    includeTTBP     := true,
    dbVersion       := TzdbPlugin.Version(tzdbVersion),
    tlFatalWarnings := false
  )
  .jsSettings(
    Compile / sourceGenerators += Def.task {
      val srcDirs        = (Compile / sourceManaged).value
      val destinationDir = (Compile / sourceManaged).value
      copyAndReplace(Seq(srcDirs), destinationDir)
    }.taskValue
  )
  .nativeSettings(
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
  .enablePlugins(NoPublishPlugin)
  .settings(commonSettings)
  .settings(
    name               := "tests",
    Keys.`package`     := file(""),
    libraryDependencies +=
      "org.scalatest" %%% "scalatest" % "3.2.14" % Test,
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
  .platformsSettings(JSPlatform, NativePlatform)(
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
  .enablePlugins(TzdbPlugin, NoPublishPlugin)
  .settings(
    name            := "demo",
    Keys.`package`  := file(""),
    zonesFilter     := zonesFilterFn,
    dbVersion       := TzdbPlugin.Version(tzdbVersion),
    tlFatalWarnings := false,
    // delegate test to run, so that it is invoked during test step in ci
    Test / test     := (Compile / run).toTask("").value
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true
  )
  .jvmSettings(
    tzdbPlatform := TzdbPlugin.Platform.Jvm
  )
  .nativeSettings(
    tzdbPlatform := TzdbPlugin.Platform.Native
  )
