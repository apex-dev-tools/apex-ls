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

// javadoc runs by default instead of scaladoc (no scala code)
// Override base options (javadoc doesn't support -target flag)
Compile / doc / javacOptions := Seq("-source", "17")

// Java formatting configuration
javafmtOnCompile := false

lazy val build = taskKey[File]("Build MCP JAR")
lazy val buildRegular = taskKey[File]("Build regular JAR for Maven Central")
lazy val buildStandalone = taskKey[File]("Build standalone JAR for GitHub Releases")

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

// Include all apex-ls dependencies in the assembly by adding them to unmanagedJars
Compile / unmanagedJars ++= {
  val apexLsJarFile = apexLsJar.value
  val apexLsTargetDir = apexLsJarFile.getParentFile
  val apexLsDeps = (apexLsTargetDir ** "*.jar").get().filter(_ != apexLsJarFile)
  apexLsDeps
}

// Add unmanaged jars to javadoc classpath so it can resolve apex-ls types
Compile / doc / dependencyClasspath ++= (Compile / unmanagedJars).value

// Custom build task to create the MCP JAR
build := {
  val originalJar = (Compile / Keys.`package`).value
  val targetDir = target.value

  // Create a final JAR in target root for easier deployment
  val finalJar = targetDir / s"apex-ls-mcp-v${version.value}.jar"
  IO.copyFile(originalJar, finalJar)

  // Ensure lib directory exists
  val libDir = targetDir / "lib"
  IO.createDirectory(libDir)

  // Copy apex-ls JAR to target for easier deployment
  val apexLsSource = apexLsJar.value
  val apexLsTarget = targetDir / apexLsSource.getName
  IO.copyFile(apexLsSource, apexLsTarget)

  // Copy apex-ls dependencies from parent project target directory
  val apexLsTargetDir = apexLsSource.getParentFile
  val apexLsDeps = (apexLsTargetDir ** "*.jar").get().filter(_ != apexLsSource)
  apexLsDeps.foreach { dep =>
    val target = libDir / dep.getName
    IO.copyFile(dep, target)
  }

  // Copy MCP project dependencies to lib directory (excluding the apex-ls JAR which goes in root)
  val mcpDeps = (Compile / dependencyClasspath).value.files.filter(_.getName.endsWith(".jar"))
  mcpDeps.foreach { dep =>
    if (dep != apexLsSource) {  // Don't copy apex-ls JAR to lib, it goes in root
      val target = libDir / dep.getName
      IO.copyFile(dep, target)
    }
  }

  println(s"MCP JAR built: ${finalJar.getAbsolutePath}")
  println(s"Depends on apex-ls JAR: ${apexLsTarget.getAbsolutePath}")
  println(s"Apex-ls dependencies: ${apexLsDeps.size} JARs")
  println(s"MCP dependencies: ${mcpDeps.size} JARs")
  println(s"Dependencies copied to: ${libDir.getAbsolutePath}")

  finalJar
}

// Package with manifest that includes apex-ls JAR in classpath
packageOptions += Package.ManifestAttributes(
  "Main-Class" -> "io.github.apexdevtools.apexls.mcp.MCPServer",
  "Class-Path" -> {
    val apexLsJarName = apexLsJar.value.getName

    // Get apex-ls dependencies from parent project target directory
    val apexLsTargetDir = apexLsJar.value.getParentFile
    val apexLsDeps = (apexLsTargetDir ** "*.jar").get().filter(_ != apexLsJar.value)
    val apexLsDepNames = apexLsDeps.map(f => s"lib/${f.getName}")

    // Get MCP project dependencies
    val mcpDeps = (Compile / dependencyClasspath).value.files.filter(_.getName.endsWith(".jar"))
    val mcpDepNames = mcpDeps.filter(_ != apexLsJar.value).map(f => s"lib/${f.getName}")

    // Combine all: apex-ls jar in root, then all dependencies in lib/
    (apexLsJarName +: (apexLsDepNames ++ mcpDepNames)).mkString(" ")
  }
)

// Assembly configuration for fat JAR (GitHub Releases only)
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", "services", xs @ _*) => MergeStrategy.concat
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case PathList("META-INF", xs @ _*) if xs.exists(_.endsWith(".SF")) => MergeStrategy.discard
  case PathList("META-INF", xs @ _*) if xs.exists(_.endsWith(".DSA")) => MergeStrategy.discard
  case PathList("META-INF", xs @ _*) if xs.exists(_.endsWith(".RSA")) => MergeStrategy.discard
  case "application.conf" => MergeStrategy.concat
  case "logback.xml" => MergeStrategy.first
  case "logback-test.xml" => MergeStrategy.first
  case "module-info.class" => MergeStrategy.discard
  case x => MergeStrategy.first
}

assembly / assemblyJarName := s"apex-ls-mcp-v${version.value}-standalone.jar"

// Regular JAR build task (Maven Central)
buildRegular := (Compile / Keys.`package`).value

// Standalone JAR build task (GitHub Releases)
buildStandalone := assembly.value

// Ensure tests can access apex-ls classes
Test / unmanagedJars += apexLsJar.value
Test / fork := true


