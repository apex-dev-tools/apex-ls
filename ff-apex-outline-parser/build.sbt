import sbtcrossproject.CrossPlugin.autoImport.crossProject

ThisBuild / version := "1.0.0"
ThisBuild / organization := "com.github.financialforcedev"
ThisBuild / scalaVersion := "2.13.3"
ThisBuild / isSnapshot := true

lazy val build = taskKey[Unit]("Build artifacts")

lazy val buildJS = Def.task {
  (Compile / fastLinkJS).value
}

lazy val buildJVM = Def.task {
  assembly.value
}

lazy val root = project.in(file(".")).aggregate(parser.js, parser.jvm)

lazy val parser = crossProject(JVMPlatform, JSPlatform)
  .in(file("."))
  .settings(
    name := "ff-apex-outline-parser",
    libraryDependencies ++= Seq(
      "org.scalatest"     %%% "scalatest"   % "3.2.9" % "test",
      "com.github.nawforce" % "apex-parser" % "2.13.0" //% "test"
    ),
    scalacOptions += "-deprecation",
    publishM2 / checksums := Nil
  )
  .jvmSettings(
    build := buildJVM.value,
    assembly / mainClass := Some("com.financialforce.oparser.JVMParser"),
    libraryDependencies += "com.github.nawforce" % "uber-apex-jorje" % "1.0.0" % Test
  )
  .jsSettings(
    build := buildJS.value,
    libraryDependencies ++= Seq("net.exoego" %%% "scala-js-nodejs-v14" % "0.12.0"),
    scalaJSUseMainModuleInitializer := false,
    /* Compile / mainClass := Some("com.financialforce.oparser.JSParser"), */
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule)
    }
  )
