package com.nawforce.runtime

import java.nio.file.Files
import com.google.common.jimfs.{Configuration, Jimfs}
import com.nawforce.pkgforce.documents.ParsedCache
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.platform.{Environment, Path}

import java.nio.charset.StandardCharsets

object FileSystemHelper {

  // Abstract virtual filesystem for testing
  def run[T](files: Map[String, String])(verify: Path => T): T = {
    val os = System.getProperty("os.name")
    val config =
      if (os.contains("Windows")) {
        val b = Configuration.windows().toBuilder
        b.setWorkingDirectory("C:\\")
        b.build()
      } else if (os.contains("OS X")) {
        val b = Configuration.osX().toBuilder
        b.setWorkingDirectory("/")
        b.build()
      } else {
        val b = Configuration.unix().toBuilder
        b.setWorkingDirectory("/")
        b.build()
      }

    val fs      = Jimfs.newFileSystem(config)
    val rootDir = fs.getRootDirectories.iterator().next()
    populateMetaFiles(files).foreach(kv => {
      // Allow UNIX style for test files on Windows
      var newPath = kv._1
      if (Environment.isWindows) {
        newPath = newPath.split('/').mkString(Path.separator)
        if (newPath.head.toString == Path.separator)
          newPath = newPath.tail
      }
      val path = rootDir.resolve(newPath)

      Files.createDirectories(path.getParent)
      Files.write(path, kv._2.getBytes(StandardCharsets.UTF_8))
    })

    ParsedCache.clear()
    verify(new Path(rootDir))
  }

  /* Many test were written without providing class/trigger metafiles so we add them in */
  private def populateMetaFiles(files: Map[String, String]): Map[String, String] = {
    val classesAndTriggers = files.keys.filter(path =>
      path.toLowerCase.endsWith(".cls") || path.toLowerCase.endsWith(".trigger")
    )
    val missingMetaFiles =
      classesAndTriggers.map(path => s"$path-meta.xml").filterNot(files.contains)
    missingMetaFiles.map(path => (path, "")).toMap ++ files
  }

  // Copy files to allow mutation of test data
  def runWithCopy[T](dir: Path)(verify: Path => T): T = {
    val config = Configuration
      .unix()
      .toBuilder
      .setWorkingDirectory("/")
      .build()
    val fs      = Jimfs.newFileSystem(config)
    val rootDir = fs.getRootDirectories.iterator().next()
    val nio     = dir.native
    val stream  = Files.walk(nio)
    stream.forEach(streamPath => {
      val relativePath = nio.toUri.relativize(streamPath.toUri).getPath
      val copy         = rootDir.resolve(relativePath)
      if (copy.getParent != null)
        Files.createDirectories(copy.getParent)
      if (!Files.isDirectory(streamPath))
        Files.copy(streamPath, copy)
    })
    stream.close()
    ParsedCache.clear()
    verify(new Path(rootDir))
  }

  // Temp directory based model
  def runTempDir[T](files: Map[String, String], setupCache: Boolean = false)(
    verify: PathLike => T
  ): T = {
    val tempDir = Files.createTempDirectory("apexlinktest")
    files.foreach(kv => {
      val path = tempDir.resolve(kv._1)
      Files.createDirectories(path.getParent)
      Files.write(path, kv._2.getBytes(StandardCharsets.UTF_8))
    })

    // Make sure cache is empty if we are going to use it
    if (setupCache)
      ParsedCache.clear()

    try {
      verify(new Path(tempDir))
    } finally {
      files.foreach(kv => {
        val path = tempDir.resolve(kv._1)
        path.toFile.delete()
      })
      tempDir.toFile.delete()
    }
  }

}
