import scala.sys.process._

ThisBuild / scalaVersion         := "2.13.16"
ThisBuild / description          := "Apex Language Server MCP (Model Context Protocol) Support"
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
ThisBuild / versionScheme := Some("strict")

// Java 17 requirement for MCP SDK
ThisBuild / javacOptions ++= Seq("-source", "17", "-target", "17")
ThisBuild / javaOptions ++= Seq("--add-opens", "java.base/java.lang=ALL-UNNAMED")

lazy val build = taskKey[File]("Build MCP JAR")

name := "apex-ls-mcp"

// Resolve the main apex-ls JAR from the parent project
lazy val apexLsJar = taskKey[File]("Get apex-ls JAR from parent project")

apexLsJar := {
  val parentTarget = baseDirectory.value.getParentFile / "jvm" / "target" / "scala-2.13"
  val jarFiles = (parentTarget ** "apex-ls*.jar").get()
  if (jarFiles.isEmpty) {
    sys.error("apex-ls JAR not found. Please build the main project first with: sbt build")
  }
  jarFiles.head
}

libraryDependencies ++= Seq(
  // MCP SDK (requires Java 17)
  "io.modelcontextprotocol.sdk" % "mcp" % "0.10.0",
  
  // JSON processing
  "com.fasterxml.jackson.core" % "jackson-core" % "2.17.0",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.17.0",
  
  // Logging
  "org.slf4j" % "slf4j-api" % "2.0.12",
  "ch.qos.logback" % "logback-classic" % "1.5.3",
  
  // Testing
  "org.mockito" % "mockito-core" % "5.11.0" % Test,
  "net.aichler" % "jupiter-interface" % "0.11.1" % Test,
  
  // HTTP client for MCP system testing
  "org.apache.httpcomponents" % "httpclient" % "4.5.14" % Test
)

// Add the main apex-ls JAR to the runtime classpath
Compile / unmanagedJars += apexLsJar.value

// Custom build task to create the MCP JAR
build := {
  val originalJar = (Compile / Keys.`package`).value
  val targetDir = target.value
  
  // Create a final JAR in target root for easier deployment
  val finalJar = targetDir / s"apex-ls-mcp-${version.value}.jar"
  IO.copyFile(originalJar, finalJar)
  
  // Ensure lib directory exists
  val libDir = targetDir / "lib"
  IO.createDirectory(libDir)
  
  // Copy apex-ls JAR to target for easier deployment
  val apexLsSource = apexLsJar.value
  val apexLsTarget = targetDir / apexLsSource.getName
  IO.copyFile(apexLsSource, apexLsTarget)
  
  // Copy dependencies to lib directory (excluding the apex-ls JAR which goes in root)
  val deps = (Compile / dependencyClasspath).value.files.filter(_.getName.endsWith(".jar"))
  deps.foreach { dep =>
    if (dep != apexLsSource) {  // Don't copy apex-ls JAR to lib, it goes in root
      val target = libDir / dep.getName
      IO.copyFile(dep, target)
    }
  }
  
  println(s"MCP JAR built: ${finalJar.getAbsolutePath}")
  println(s"Depends on apex-ls JAR: ${apexLsTarget.getAbsolutePath}")
  println(s"Dependencies copied to: ${libDir.getAbsolutePath}")
  
  finalJar
}

// Package with manifest that includes apex-ls JAR in classpath
packageOptions += Package.ManifestAttributes(
  "Main-Class" -> "io.github.apexdevtools.apexls.mcp.MCPServer",
  "Class-Path" -> {
    val apexLsJarName = apexLsJar.value.getName
    val deps = (Compile / dependencyClasspath).value.files.filter(_.getName.endsWith(".jar"))
    // Only include managed dependencies in lib/, apex-ls JAR is in root
    val depNames = deps.filter(_ != apexLsJar.value).map(f => s"lib/${f.getName}")
    (apexLsJarName +: depNames).mkString(" ")
  }
)

// Ensure tests can access apex-ls classes
Test / unmanagedJars += apexLsJar.value
Test / fork := true


