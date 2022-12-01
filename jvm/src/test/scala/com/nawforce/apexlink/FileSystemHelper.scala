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
package com.nawforce.apexlink

import java.nio.file.Files
import com.google.common.jimfs.{Configuration, Jimfs}
import com.nawforce.pkgforce.documents.ParsedCache
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.platform.Path

object FileSystemHelper {

  // Abstract virtual filesystem for testing
  def run[T](files: Map[String, String])(verify: PathLike => T): T = {
    val config = Configuration
      .unix()
      .toBuilder
      .setWorkingDirectory("/")
      .build()
    val fs      = Jimfs.newFileSystem(config)
    val rootDir = fs.getRootDirectories.iterator().next()
    files.foreach(kv => {
      val path = rootDir.resolve(kv._1)
      Files.createDirectories(path.getParent)
      Files.write(path, kv._2.getBytes())
    })

    ParsedCache.clear()
    verify(new Path(rootDir))
  }

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
      Files.write(path, kv._2.getBytes())
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
