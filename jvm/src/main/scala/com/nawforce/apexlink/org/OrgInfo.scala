/*
 Copyright (c) 2022 Kevin Jones, All rights reserved.
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
package com.nawforce.apexlink.org

import com.nawforce.apexlink.org.OPM.OrgImpl
import com.nawforce.pkgforce.diagnostics.{
  Diagnostic,
  ERROR_CATEGORY,
  Issue,
  LoggerOps,
  MISSING_CATEGORY
}
import com.nawforce.pkgforce.path.{Location, PathLike, PathLocation}

import java.io.{File, PrintWriter, StringWriter}
import java.nio.file.Files
import scala.collection.compat.immutable.ArraySeq
import scala.util.DynamicVariable

/** Access to the 'current' org, this should be deprecated now we have the OPM hierarchy.
  */
object OrgInfo {

  /** Access the in-scope Org */
  private[nawforce] val current: DynamicVariable[OrgImpl] = new DynamicVariable[OrgImpl](null)

  /** Log an issue against the in-scope org */
  private[nawforce] def log(issue: Issue): Unit = {
    if (issue.path != null && current.value != null)
      current.value.issueManager.add(issue)
  }

  /** Log a general error against the in-scope org */
  private[nawforce] def logError(pathLocation: PathLocation, message: String): Unit = {
    log(new Issue(pathLocation.path, Diagnostic(ERROR_CATEGORY, pathLocation.location, message)))
  }

  /** Log a missing error against the in-scope org */
  private[nawforce] def logMissing(pathLocation: PathLocation, message: String): Unit = {
    log(new Issue(pathLocation.path, Diagnostic(MISSING_CATEGORY, pathLocation.location, message)))
  }

  /** Log an exception during processing. If at least one path is provided this logs the
    * exception against the first. The files are copied to a temporary directory to aid debugging.
    */
  private[nawforce] def logException(ex: Throwable, paths: ArraySeq[PathLike]): Unit = {
    if (paths.isEmpty) {
      LoggerOps.info("Exception reported against no paths", ex)
      return
    }

    try {
      val writer = new StringWriter
      writer.append("Validation failed: ")
      val tempDir = Files.createTempDirectory("apex-ls-log")
      paths.foreach(path => {
        val from = new File(path.toString).toPath
        if (Files.exists(from)) {
          Files.copy(
            from,
            tempDir.resolve(from.getFileName),
            java.nio.file.StandardCopyOption.REPLACE_EXISTING
          )
        }
      })
      writer.append("log directory ")
      writer.append(tempDir.toString)
      writer.append('\n')
      ex.printStackTrace(new PrintWriter(writer))
      log(Issue(ERROR_CATEGORY, PathLocation(paths.head, Location.empty), writer.toString))
    } catch {
      case ex: Throwable =>
        LoggerOps.info("Failed to log an exception", ex)
    }
  }
}
