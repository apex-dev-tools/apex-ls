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
package com.nawforce.pkgforce.documents

import com.nawforce.pkgforce.diagnostics.{IssueLogger, LoggerOps}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.pkgforce.sfdx.ForceIgnore

trait DocumentCollector {
  def onAdd(logger: IssueLogger, doc: MetadataDocument): Unit
}

/** Directory scanner for discovering (non-ignored) metadata documents */
object DocumentScanner {

  def index(
    path: PathLike,
    logger: IssueLogger,
    forceIgnore: Option[ForceIgnore],
    collector: DocumentCollector
  ): Unit = {

    if (isExcluded(path))
      return
    if (path.isDirectory) {
      if (forceIgnore.forall(_.includeDirectory(path))) {
        val entries = path.splitDirectoryEntries()
        // Enforce top-down handling
        entries._1.foreach(file => addPath(file, logger, forceIgnore, collector))
        entries._2.foreach(dir => index(dir, logger, forceIgnore, collector))
      } else {
        LoggerOps.debug(s"Ignoring directory $path")
      }
    } else {
      addPath(path, logger, forceIgnore, collector)
    }
  }

  private def addPath(
    path: PathLike,
    logger: IssueLogger,
    forceIgnore: Option[ForceIgnore],
    collector: DocumentCollector
  ): Unit = {
    // Not testing if this is a regular file to improve scan performance, will fail later on read
    if (forceIgnore.forall(_.includeFile(path))) {
      val dt = MetadataDocument(path)
      dt.foreach(dt => collector.onAdd(logger, dt))
    } else {
      LoggerOps.debug(s"Ignoring file $path")
    }
  }

  /** Exclude some paths that we would waste time searching. */
  private def isExcluded(path: PathLike): Boolean = {
    val basename = path.basename
    basename.startsWith(".") || basename == "node_modules"
  }
}
