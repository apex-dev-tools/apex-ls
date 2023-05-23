/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.pkgforce.documents

import com.nawforce.pkgforce.path.PathLike

import scala.collection.mutable

/* Directory tree for maintaining information on state of a file system and refreshing to locate changes.
 * Changes are detected using file modification times. If the underlying filesystem only reports in seconds
 * then the accuracy of what has been changed may be compromised. */
class DirectoryTree private (val path: PathLike) {
  private var fileModTimes   = Array[(String, Long)]()
  private var subDirectories = Array[DirectoryTree]()

  /* Create a refreshed version of this node, return unchanged if the directory does not exist */
  def refresh(changed: mutable.ArrayBuffer[String]): DirectoryTree = {
    if (path.isDirectory) {
      val (files, directories) = path.splitDirectoryEntries()
      refreshSubdirectories(directories, changed)
      refreshFiles(files, changed)
    }
    this
  }

  def directories: Array[DirectoryTree] = subDirectories

  def fileModificationTimes: Map[String, Long] = fileModTimes.toMap

  private def refreshSubdirectories(
    directoryPaths: Array[PathLike],
    changed: mutable.ArrayBuffer[String]
  ): Unit = {
    // Find deleted & report all files recursively as changed
    val currentDirectories = directoryPaths.toSet
    subDirectories
      .filterNot(sd => currentDirectories.contains(sd.path))
      .foreach(_.collectFiles(changed))

    // Regenerate sub directory list
    val existing = subDirectories.map(d => (d.path, d)).toMap
    subDirectories = directoryPaths
      .map(dir => {
        existing.get(dir) match {
          case None            => DirectoryTree(dir, changed)
          case Some(directory) => directory.refresh(changed)
        }
      })
  }

  private def refreshFiles(
    filePaths: Array[PathLike],
    changed: mutable.ArrayBuffer[String]
  ): Unit = {
    // Find deleted & report as changed
    val currentFiles = filePaths.map(_.toString).toSet
    fileModTimes
      .map(_._1)
      .filterNot(currentFiles.contains)
      .foreach(path => changed.append(path))

    // Regenerate, reporting on any changed mod times
    val existingModTimes = fileModTimes.toMap
    fileModTimes = filePaths.flatMap(path => {
      val modified = path.lastModified()
      modified match {
        case None =>
          changed.append(path.toString)
        case Some(newModTime) if newModTime != existingModTimes.getOrElse(path.toString, 0) =>
          changed.append(path.toString)
        case _ => ()
      }
      modified.map(modTime => (path.toString, modTime))
    })
  }

  /* Collect all file names, recursively */
  private def collectFiles(changed: mutable.ArrayBuffer[String]): Unit = {
    fileModTimes.map(_._1).foreach(path => changed.append(path))
    subDirectories.foreach(_.collectFiles(changed))
  }
}

object DirectoryTree {
  def apply(path: PathLike, changed: mutable.ArrayBuffer[String]): DirectoryTree = {
    val dir = new DirectoryTree(path)
    if (path.isDirectory)
      dir.refresh(changed)
    dir
  }
}
