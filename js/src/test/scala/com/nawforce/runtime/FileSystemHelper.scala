/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.
 */
package com.nawforce.runtime

import com.nawforce.pkgforce.documents.ParsedCache
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.imports.{FSMonkey, Memfs}
import com.nawforce.runtime.platform.{Environment, Path}
import io.scalajs.nodejs.os.OS

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object FileSystemHelper {

  private var directoryId = 0

  // Abstract virtual filesystem for testing
  def run[T](files: Map[String, String], setupCache: Boolean = false)(verify: PathLike => T): T = {

    // Load files into memfs
    Memfs.vol.fromJSON(
      populateMetaFiles(files)
        .map(kv => ("/" + kv._1, kv._2))
        .toJSDictionary
        .asInstanceOf[js.Dynamic]
    )

    // Make a cache directory so don't need home access
    if (setupCache) {
      Memfs.vol.mkdirSync("/tmpcache")
      Environment.setCacheDirOverride(Some(Some(Path("/tmpcache"))))
    }

    val unpatch = FSMonkey.patchFs(Memfs.vol)
    try {
      verify(Path("/"))
    } finally {
      unpatch()
      Memfs.vol.reset()
      if (setupCache) {
        Environment.setCacheDirOverride(None)
      }
    }
  }

  private def makeDir(path: PathLike): Unit = {
    if (!path.isDirectory) {
      makeDir(path.parent)
      path.parent.createDirectory(path.basename)
    }
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

  def createTmpDir(): Path = {
    val dirName = s"apexlinktest${directoryId}"
    directoryId += 1
    val tempDir = Path(OS.tmpdir()).join(dirName)
    if (tempDir.exists)
      createTmpDir()
    else {
      tempDir.parent.createDirectory(dirName)
      tempDir
    }
  }

  // Temp directory based model
  def runTempDir[T](files: Map[String, String], setupCache: Boolean = false)(
    verify: PathLike => T
  ): T = {
    val tempDir = createTmpDir()
    files.foreach(kv => {
      val path = tempDir.join(kv._1)
      makeDir(path.parent)
      path.write(kv._2)
    })

    // Make sure cache is empty if we are going to use it
    if (setupCache)
      ParsedCache.clear()

    try {
      verify(tempDir)
    } finally {
      files.foreach(kv => {
        val path = tempDir.join(kv._1)
        path.delete()
      })
      tempDir.delete()
    }
  }
}
