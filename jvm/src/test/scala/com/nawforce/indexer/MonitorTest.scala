/*
 * Copyright (c) 2023 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.indexer

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.api.{IndexerConfiguration, ServerOps}
import com.nawforce.apexlink.indexer.Monitor
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

/*
 * NOTE: The Monitor takes some time to start reporting, I have used Thread.sleep() to control this but it may need
 * some adjustment to maintain passing tests.
 */
class MonitorTest extends AnyFunSuite with TestHelper {

  private def nap(): Unit = {
    Thread.sleep(300)
  }

  private def run[T](files: Map[String, String])(verify: (Monitor, PathLike) => T): T = {
    FileSystemHelper.runTempDir(files) { root =>
      val oldConfig = ServerOps.setIndexerConfiguration(IndexerConfiguration(50, 300))
      val monitor   = new Monitor(root)
      try {
        nap() // Give monitor time to setup before making changes
        verify(monitor, root)
      } finally {
        monitor.stop()
        ServerOps.setIndexerConfiguration(oldConfig)
      }
    }
  }

  test("Starts & stops") {
    run(Map[String, String]()) { (monitor: Monitor, root: PathLike) =>
      nap()
    }
  }

  test("Allows monitoring of workspace") {
    run(Map[String, String]()) { (monitor: Monitor, root: PathLike) =>
      val changed = mutable.ArrayBuffer[String]()
      monitor.monitor(root, path => changed.append(path))
      root.join("test.txt").write("")

      nap()
      assert(changed.toSet == Set(root.join("test.txt").toString))
    }
  }

  test("Allows monitoring of workspace sub directory") {
    run(Map[String, String]()) { (monitor: Monitor, root: PathLike) =>
      val subdir = root.createDirectory("a").getOrElse(throw new IllegalArgumentException())

      val changed = mutable.ArrayBuffer[String]()
      monitor.monitor(
        subdir,
        path => {
          changed.append(path)
        }
      )
      subdir.join("test.txt").write("")
      root.join("bad.txt").write("")

      nap()
      assert(changed.toSet == Set(subdir.join("test.txt").toString))
    }
  }

  test("Allows monitoring of none-existing workspace sub directory") {
    run(Map[String, String]()) { (monitor: Monitor, root: PathLike) =>
      val subdir = root.join("a")

      val changed = mutable.ArrayBuffer[String]()
      monitor.monitor(
        subdir,
        path => {
          changed.append(path)
        }
      )
      root.createDirectory("a")
      subdir.join("test.txt").write("")
      root.join("bad.txt").write("")

      nap()
      assert(changed.toSet == Set(subdir.join("test.txt").toString))
    }
  }

  test("Allows monitoring recursively") {
    run(Map[String, String]()) { (monitor: Monitor, root: PathLike) =>
      val subdir   = root.createDirectory("a").getOrElse(throw new IllegalArgumentException())
      val childdir = subdir.createDirectory("b").getOrElse(throw new IllegalArgumentException())

      val changed = mutable.ArrayBuffer[String]()
      monitor.monitor(
        subdir,
        path => {
          changed.append(path)
        }
      )
      childdir.join("test.txt").write("")
      root.join("bad.txt").write("")

      nap()
      assert(changed.toSet == Set(childdir.join("test.txt").toString))
    }
  }

  test("Does not allows monitoring outside of workspace") {
    run(Map[String, String]()) { (monitor: Monitor, root: PathLike) =>
      val thrown = intercept[Exception] {
        monitor.monitor(root.parent, path => {})
      }
      assert(
        thrown.getMessage ==
          s"Path ${root.parent.toString} must be the whole workspace or a subdirectory of ${root.toString}"
      )
    }
  }

  test("Does not allows monitoring of subdirectory of root monitor") {
    run(Map[String, String]()) { (monitor: Monitor, root: PathLike) =>
      monitor.monitor(root, path => {})
      val thrown = intercept[Exception] {
        monitor.monitor(root.join("a"), path => {})
      }
      assert(
        thrown.getMessage ==
          s"Path ${root.join("a").toString} can not be a subdirectory of another monitored path ${root.toString}"
      )
    }
  }

  test("Does not allows monitoring of subdirectory of monitored sub-directory") {
    run(Map[String, String]()) { (monitor: Monitor, root: PathLike) =>
      monitor.monitor(root.join("a"), path => {})
      val thrown = intercept[Exception] {
        monitor.monitor(root.join("a").join("b"), path => {})
      }
      assert(
        thrown.getMessage ==
          s"Path ${root.join("a").join("b").toString} can not be a subdirectory of another monitored path ${root.join("a").toString}"
      )
    }
  }

}
