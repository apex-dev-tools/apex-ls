import org.scalajs.linker.interface.Report

import scala.sys.process._

ThisBuild / scalaVersion         := "2.13.16"
ThisBuild / description          := "Salesforce Apex static analysis toolkit"
ThisBuild / organization         := "io.github.apex-dev-tools"
ThisBuild / organizationHomepage := Some(url("https://github.com/apex-dev-tools/apex-ls"))
ThisBuild / homepage             := Some(url("https://github.com/apex-dev-tools/apex-ls"))
ThisBuild / licenses := List(
  "BSD-3-Clause" -> new URL("https://opensource.org/licenses/BSD-3-Clause")
)
ThisBuild / developers := List(
  Developer(
    "apexdevtools",
    "Apex Dev Tools Team",
    "apexdevtools@gmail.com",
    url("https://github.com/apex-dev-tools")
  )
)
ThisBuild / versionScheme          := Some("strict")
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots

lazy val build = taskKey[File]("Build artifacts")
lazy val pack  = inputKey[Unit]("Publish specific local version")
lazy val Dev   = config("dev") extend Compile

// Don't publish root
publish / skip := true

// Limit to sequential test for both cross projects
Global / concurrentRestrictions += Tags.limit(Tags.Test, 1)

lazy val apexls = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .configs(Dev)
  .settings(
    name := "apex-ls",
    scalacOptions += "-deprecation",
    libraryDependencies ++= Seq(
      "io.github.apex-dev-tools" %%% "outline-parser"                         % "1.3.0",
      "com.github.nawforce"      %%% "scala-json-rpc"                         % "1.1.0",
      "com.github.nawforce"      %%% "scala-json-rpc-upickle-json-serializer" % "1.1.0",
      "com.lihaoyi"              %%% "upickle"                                % "1.2.0",
      "com.lihaoyi"              %%% "mainargs"                               % "0.5.4",
      "org.scalatest"            %%% "scalatest"                              % "3.2.0" % Test
    )
  )
  .jvmSettings(
    build       := buildJVM.value,
    Test / fork := true,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules"  %% "scala-xml"                      % "1.3.0",
      "org.scala-lang.modules"  %% "scala-parallel-collections"     % "1.0.0",
      "org.scala-js"            %% "scalajs-stubs"                  % "1.0.0",
      "io.github.apex-dev-tools" % "apex-parser"                    % "4.3.1",
      "io.github.apex-dev-tools" % "vf-parser"                      % "1.1.0",
      "io.github.apex-dev-tools" % "sobject-types"                  % "63.0.0",
      "io.github.apex-dev-tools" % "standard-types"                 % "63.0.0",
      "io.methvin"              %% "directory-watcher-better-files" % "0.18.0",
      "com.github.nawforce"      % "uber-apex-jorje"                % "1.0.0" % Test,
      "com.google.jimfs"         % "jimfs"                          % "1.1"   % Test
    ),
    packageOptions += Package.ManifestAttributes(
      "Class-Path" -> (Compile / dependencyClasspath).value.files.map(_.getName.trim).mkString(" "),
      "Implementation-Build" -> java.time.Instant.now().toEpochMilli.toString
    )
  )
  .jsSettings(
    build                    := buildJs(Compile / fullLinkJS).value,
    Dev / build              := buildJs(Compile / fastLinkJS).value,
    Test / parallelExecution := false,
    libraryDependencies ++= Seq("net.exoego" %%% "scala-js-nodejs-v14" % "0.12.0"),
    scalaJSUseMainModuleInitializer := false,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )

lazy val buildJVM = Def.task {
  val targetDir = crossTarget.value
  val targetJar = (Compile / Keys.`package`).value

  // Delete extra jars from target dir
  IO.delete((targetDir * "*.jar").get().filterNot(_.equals(targetJar)))

  // Copy jar deps to target for easier testing
  val files = (Compile / dependencyClasspath).value.files map { f =>
    f -> targetDir / f.getName
  }
  IO.copy(files, CopyOptions().withOverwrite(true))

  targetJar
}

def buildJs(jsTask: TaskKey[Attributed[Report]]): Def.Initialize[Task[File]] = Def.task {
  def exec: (String, File) => Unit = run(streams.value.log)(_, _)

  // Depends on scalaJS fast/full linker output
  jsTask.value

  val targetDir  = crossTarget.value
  val targetFile = (jsTask / scalaJSLinkerOutputDirectory).value / "main.js"
  val npmDir     = baseDirectory.value / "npm"

  val files: Map[File, File] = Map(
    // Update target with NPM modules (for testing)
    npmDir / "package.json" -> targetDir / "package.json",
    // Add source to NPM
    targetFile -> npmDir / "lib/apex-ls.js"
  )

  IO.copy(files, CopyOptions().withOverwrite(true))

  // Install modules in NPM
  exec("npm ci", npmDir)

  // Update target with NPM modules (for testing)
  IO.delete(targetDir / "node_modules")
  IO.copyDirectory(
    npmDir / "node_modules",
    targetDir / "node_modules",
    CopyOptions().withOverwrite(true)
  )

  targetFile
}

// Command to do a local release under a specific version
// Defaults to last reachable tag (ignoring current commit) or 0.0.0
// e.g. sbt "pack 1.2.3-SNAPSHOT" / sbt pack
pack := {
  import sbt.complete.Parsers.spaceDelimited
  val args: Seq[String] = spaceDelimited("<arg>").parsed
  val v                 = args.headOption.getOrElse(previousStableVersion.value.getOrElse("0.0.0"))

  val newState =
    Project.extract(state.value).appendWithoutSession(Seq(ThisBuild / version := v), state.value)
  val proj = Project.extract(newState)

  proj.runTask(apexls.jvm / publishLocal, newState)
  proj.runTask(apexls.js / publishLocal, newState)
}

// Run a command and log to provided logger
def run(log: ProcessLogger)(cmd: String, cwd: File): Unit = {
  val shell: Seq[String] =
    if (sys.props("os.name").contains("Windows")) Seq("cmd", "/c") else Seq("bash", "-c")
  val exitCode = Process(shell :+ cmd, cwd) ! log
  if (exitCode > 0) {
    log.err(s"Process exited with non-zero exit code: $exitCode")
    sys.exit(exitCode)
  }
}
