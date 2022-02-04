import sbtcrossproject.CrossPlugin.autoImport.crossProject

ThisBuild / version := "0.0.1"
ThisBuild / scalaVersion := "2.13.3"

lazy val build = taskKey[Unit]("Build artifacts")

lazy val buildJS = Def.task {
  (Compile / fastLinkJS).value
}

lazy val buildJVM = Def.task {
  assembly.value
}

lazy val root = project.in(file(".")).
  aggregate(parser.js, parser.jvm).
  settings(
    name := "outline_parser"
  )

lazy val parser = crossProject(JVMPlatform, JSPlatform).in(file(".")).
  settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.2.9" % "test",
      "com.github.nawforce" % "apex-parser" % "2.11.0" //% "test"
    ),
    scalacOptions += "-deprecation"
  ).
  jvmSettings(
    build := buildJVM.value,
    assembly / mainClass := Some("com.financialforce.oparser.JVMParser")
  ).
  jsSettings(
    build := buildJS.value,
    libraryDependencies ++= Seq("net.exoego" %%% "scala-js-nodejs-v14" % "0.12.0"),
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("com.financialforce.oparser.JSParser"),
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule)
    }
  )
