val scala213 = "2.13.14"
val scala3   = "3.3.3"
ThisBuild / scalaVersion       := scala213
ThisBuild / crossScalaVersions := Seq("2.12.17", scala213, scala3)

ThisBuild / tlBaseVersion := "2.6"

ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)

val javaDistro = JavaSpec.corretto("11")
ThisBuild / githubWorkflowJavaVersions := Seq(javaDistro)

ThisBuild / githubWorkflowSbtCommand := "./sbt"

ThisBuild / githubWorkflowBuildMatrixExclusions ++= Seq(
  MatrixExclude(Map("scala" -> "3", "project" -> "rootJVM")), // TODO
  MatrixExclude(
    Map("scala" -> "3", "project" -> "rootNative", "os" -> "ubuntu-latest")
  ) // run on macOS instead
)

ThisBuild / githubWorkflowBuildMatrixInclusions +=
  MatrixInclude(Map("scala" -> "3", "java" -> javaDistro.render, "project" -> "rootNative"),
                Map("os"    -> "macos-latest")
  )

val tzdbVersion             = "2019c"
val scalajavaLocalesVersion = "1.5.4"
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
def copyAndReplace(
  srcDirs:          Seq[File],
  destinationDir:   File,
  createNestedDirs: Boolean = false
): Seq[File] = {
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
      val targetDestinationDir = if (createNestedDirs) {
        destinationDir / sourceDir.getName
      } else {
        destinationDir
      }
      files ++ copyDirectory(sourceDir, targetDestinationDir, overwrite = true)
    }
    .filterNot(_.isDirectory)
    .filter(_.getName.endsWith(".scala"))
    .filterNot(_.getParentFile.getName == "util")
    .toList

  // These replacements will in practice rename all the classes from
  // org.threeten to java.time
  //
  // !!! WARNING: AVOID MODIFYING FILE CONTENTS HERE MORE THAN ABSOLUTELY NECESSARY !!!
  //  - The more significant the change, in terms of changing source code position
  //    (line number + column), the more broken the Scala.js source maps will be,
  //    preventing debugging of Scala.js code that uses scala-java-code in the browser.
  //  - Line-for-line replacements of `package/import x` with `package/import y` are
  //    mostly ok because they don't affect the remaining code in the file.
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
  .disablePlugins(TypelevelScalaJSGitHubPlugin)
  .settings(commonSettings)
  .settings(
    name                                          := "scala-java-time",
    libraryDependencies += ("org.portable-scala" %%% "portable-scala-reflect" % "1.1.3")
      .cross(CrossVersion.for3Use2_13)
  )
  .jsSettings(
    scalacOptions ++= {
      if (tlIsScala3.value) Seq("-scalajs-genStaticForwardersForNonTopLevelObjects")
      else Seq("-P:scalajs:genStaticForwardersForNonTopLevelObjects")
    },
    scalaJsGithubSourceMaps("core/shared"),
    Compile / sourceGenerators += Def.task {
      val srcDirs        = (Compile / sourceDirectories).value
      val destinationDir = (Compile / sourceManaged).value
      copyAndReplace(srcDirs, destinationDir, createNestedDirs = true)
    }.taskValue,
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-locales" % scalajavaLocalesVersion
    )
  )
  .nativeSettings(
    scalacOptions += "-P:scalanative:genStaticForwardersForNonTopLevelObjects",
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
    includeTTBP := true,
    dbVersion   := TzdbPlugin.Version(tzdbVersion)
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
      "org.scalatest" %%% "scalatest" % "3.2.18" % Test,
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
    name           := "demo",
    Keys.`package` := file(""),
    zonesFilter    := zonesFilterFn,
    dbVersion      := TzdbPlugin.Version(tzdbVersion),
    // delegate test to run, so that it is invoked during test step in ci
    Test / test    := (Compile / run).toTask("").value
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

def scalaJsGithubSourceMaps(projectDir: String) =
  // - Unfortunately we can only specify one `projectDir`.
  //   So, if our JS project has sources in both `core/js` and `core/shared`,
  //   we can only pick one of those, and all sources in the other one will have
  //   broken URLs in source maps.
  // - The root of the problem is that in `copyAndReplace` we copy the contents of
  //   multiple source directories into a single src_managed directory, and so we
  //   lose information about where our sources originally came from, and even if we
  //   could capture or recover this information, the syntax of `mapSourceURI` option
  //   doesn't allow more than one mapping, so we probably wouldn't be able to use it
  //   to create multiple mappings.
  // - Also, for the same reason, we are unable to map sources that are not copied
  //   to src_managed (stuff under java.util). Those will have invalid file: URLs.
  // - In the future, maybe we can somehow adjust the folder structure inside `src_managed`
  //   to introduce new top level directories matching project names (`core`).
  // - The CI env var is set by Github actions automatically.
  //   We only want this transformation to run when creating & publishing an artifact to Maven,
  //   as this lets us use the original local file paths for local dev / publishLocal.
  // ---
  // - How to test changes to this code locally:
  //   - Make sure the sys.env.get("CI") filter passes, one way or another
  //   - publishLocal the core project (press enter to skip passphrase, don't need signing)
  //   - find the resulting jar – see file path in sbt output. Open it (it's a zip archive).
  //   - find .sjsir files inside the jar, open some of them with plain text editor
  //   - in the first bytes of the binary, observe the raw.githubusercontent.com URL in plaintext
  //   - copy-paste that URL into the browser.
  //     - It should show the contents of this file on github (subject to caveats above)
  //     - If it doesn't, make sure the version / commit hash you have locally exists on github (or fake it).
  scalacOptions ++= sys.env.get("CI").map { _ =>
    val localCopiedSourcesPath = (Compile / sourceManaged).value.toURI
    val remoteSourcesPath      =
      s"https://raw.githubusercontent.com/cquiroz/scala-java-time/${git.gitHeadCommit.value.get}/${projectDir}/src/main/"
    val sourcesOptionName      =
      if (tlIsScala3.value) "-scalajs-mapSourceURI" else "-P:scalajs:mapSourceURI"
    s"${sourcesOptionName}:$localCopiedSourcesPath->$remoteSourcesPath"
  }
