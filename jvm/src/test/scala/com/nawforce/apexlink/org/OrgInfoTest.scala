/*
 * Copyright (c) 2024 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.TestHelper
import com.nawforce.pkgforce.diagnostics.LoggerOps.{INFO_LOGGING, NO_LOGGING}
import com.nawforce.pkgforce.diagnostics.{Logger, LoggerOps}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, StringWriter}
import scala.collection.immutable.ArraySeq

class OrgInfoTest extends AnyFunSuite with TestHelper {

  final class CaptureLogger extends Logger {
    val writer: StringWriter = new StringWriter()

    override def info(message: String): Unit = {
      writer.append(s"INFO: $message\n")
    }

    override def debug(message: String): Unit = {
      writer.append(s"DEBUG: $message\n")
    }

    override def trace(message: String): Unit = {
      writer.append(s"TRACE: $message\n")
    }
  }

  test("Log exception without files creates info message") {
    FileSystemHelper.run(Map()) { root: PathLike =>
      createOrg(root)

      val captureLogger = new CaptureLogger
      val oldLogger     = LoggerOps.setLogger(captureLogger)
      LoggerOps.setLoggingLevel(INFO_LOGGING)

      OrgInfo.logException(new Exception("Hello"), ArraySeq())

      assert(captureLogger.writer.toString.startsWith("INFO: Exception reported against no paths"))
      assert(captureLogger.writer.toString.contains("INFO: java.lang.Exception: Hello"))

      LoggerOps.setLoggingLevel(NO_LOGGING)
      LoggerOps.setLogger(oldLogger)

    }
  }

  test("Log exception captures files") {
    FileSystemHelper.runTempDir(
      Map("a.txt" -> "a.txt", "dir1/b.txt" -> "b.txt", "dir1/dir2/c.txt" -> "c.txt")
    ) { root: PathLike =>
      createOrg(root)

      val captureLogger = new CaptureLogger
      val oldLogger     = LoggerOps.setLogger(captureLogger)
      LoggerOps.setLoggingLevel(INFO_LOGGING)

      try {

        withOrg { _ =>
          OrgInfo.logException(
            new Exception("Hello"),
            ArraySeq(root.join("a.txt"), root.join("dir1/b.txt"), root.join("dir1/dir2/c.txt"))
          )
        }

        assert(captureLogger.writer.toString.isEmpty)
        assert(getMessages().contains("/a.txt: Error: line 1: Validation failed: log directory"))
        assert(getMessages().contains("java.lang.Exception: Hello"))

        val tmpDir     = getMessages().split("\n").head.split("directory").last.trim
        val tmpDirPath = new File(tmpDir)
        assert(tmpDirPath.isDirectory)
        assert(tmpDirPath.listFiles().map(_.getName).toSet == Set("a.txt", "b.txt", "c.txt"))

      } finally {
        LoggerOps.setLoggingLevel(NO_LOGGING)
        LoggerOps.setLogger(oldLogger)
      }
    }
  }

}
