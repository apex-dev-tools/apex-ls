import sbt.Keys.libraryDependencies
import sbt.url
import sbtcrossproject.CrossPlugin.autoImport.crossProject

ThisBuild / version              := "3.0.0"
ThisBuild / isSnapshot           := true
ThisBuild / scalaVersion         := "2.13.3"
ThisBuild / description          := "Salesforce Apex static analysis toolkit"
ThisBuild / organization         := "com.github.financialforcedev"
ThisBuild / organizationHomepage := Some(url("https://github.com/financialforcedev/ff-apex-ls"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/financialforcedev/ff-apex-ls"),
    "git@github.com:financialforcedev/ff-apex-ls.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "nawforce",
    name = "Kevin Jones",
    email = "nawforce@gmail.com",
    url = url("https://github.com/nawforce")
  )
)
ThisBuild / licenses := List(
  "BSD-3-Clause" -> new URL("https://opensource.org/licenses/BSD-3-Clause")
)
ThisBuild / homepage := Some(url("https://github.com/financialforcedev/ff-apex-ls"))
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials")
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

lazy val build = taskKey[Unit]("Build artifacts")

lazy val apexls = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("."))
  .settings(
    scalacOptions += "-deprecation",
    libraryDependencies += "io.github.apex-dev-tools" %%% "outline-parser" % "1.0.0",
    libraryDependencies += "com.github.nawforce"      %%% "scala-json-rpc" % "1.0.1",
    libraryDependencies += "com.github.nawforce" %%% "scala-json-rpc-upickle-json-serializer" % "1.0.1",
    libraryDependencies += "com.lihaoyi"   %%% "upickle"   % "1.2.0",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.0" % Test
  )
  .jvmSettings(
    build                                            := buildJVM.value,
    Test / fork                                      := true,
    libraryDependencies += "org.scala-lang.modules"  %% "scala-xml"                  % "1.3.0",
    libraryDependencies += "org.scala-lang.modules"  %% "scala-parallel-collections" % "1.0.0",
    libraryDependencies += "org.scala-js"            %% "scalajs-stubs"              % "1.0.0",
    libraryDependencies += "io.github.apex-dev-tools" % "apex-parser"                % "3.0.0",
    libraryDependencies += "io.github.apex-dev-tools" % "vf-parser"                  % "1.0.0",
    libraryDependencies += "org.antlr"                % "antlr4-runtime"             % "4.8-1",
    libraryDependencies += "io.github.apex-dev-tools" % "sobject-types"              % "55.0.0",
    libraryDependencies += "io.github.apex-dev-tools" % "standard-types"             % "55.0.0",
    libraryDependencies += "com.github.nawforce" % "uber-apex-jorje" % "1.0.0" % Test,
    libraryDependencies += "com.google.jimfs"    % "jimfs"           % "1.1"   % Test
  )
  .jsSettings(
    build                                := buildNPM.value,
    libraryDependencies += "net.exoego" %%% "scala-js-nodejs-v14" % "0.12.0",
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )

lazy val downloadJars = Def.task {
  val jars = baseDirectory.value / "target" / "scala-2.13"
  val files = (Compile / dependencyClasspath).value.files map { f =>
    f -> jars / f.getName
  }
  IO.copy(files, CopyOptions().withOverwrite(true))
  jars.get.last
}

lazy val buildJVM = Def.task {
  downloadJars.value
  (Compile / compile).value
}

lazy val buildNPM = Def.task {

  // Update NPM module with latest compile
  import java.nio.file.StandardCopyOption.REPLACE_EXISTING
  import java.nio.file.Files.copy

  (Compile / fastOptJS).value
  (Compile / fullOptJS).value

  val jsDir     = file("js")
  val targetDir = jsDir / "target" / "scala-2.13"
  val optSource = targetDir / "apexls-opt.js"
  val optTarget = jsDir / "npm" / "src" / "apexls.js"
  copy(optSource.toPath, optTarget.toPath, REPLACE_EXISTING)

  // Install modules in NPM
  import scala.language.postfixOps
  import scala.sys.process._

  val npmDir = jsDir / "npm"
  val shell: Seq[String] =
    if (sys.props("os.name").contains("Windows")) Seq("cmd", "/c") else Seq("bash", "-c")
  Process(shell :+ "npm i --production", npmDir) !

  // Update NPM README.md
  val readMeTarget = npmDir / "README.md"
  copy(file("README.md").toPath, readMeTarget.toPath, REPLACE_EXISTING)

  // Update target with NPM modules (for testing)
  val packageJSONSource = npmDir / "package.json"
  val packageJSONTarget = targetDir / "package.json"
  copy(packageJSONSource.toPath, packageJSONTarget.toPath, REPLACE_EXISTING)
  Process(shell :+ "npm i", targetDir) !
}
