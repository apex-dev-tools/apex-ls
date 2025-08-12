/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.indexer

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.api.{IndexerConfiguration, ServerOps}
import com.nawforce.apexlink.indexer.{Indexer, Monitor}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

/*
 * NOTE: The handling here depends on timing, I have used Thread.sleep() to control this but it may need some
 * adjustment to maintain passing tests.
 */
class IndexerTest extends AnyFunSuite with TestHelper {

  private def nap(): Unit = {
    Thread.sleep(300)
  }

  private def run[T](files: Map[String, String])(verify: (Monitor, PathLike) => T): T = {
    FileSystemHelper.runTempDir(files) { root =>
      val oldConfig = ServerOps.setIndexerConfiguration(IndexerConfiguration(50, 200))
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

  test("Root directory deletion is safe") {
    run(Map[String, String]()) { (monitor: Monitor, root: PathLike) =>
      val indexer = new Indexer(root, monitor) {
        override def onFilesChanged(path: Array[String], rescan: Boolean): Unit = {}
      }

      nap()
      root.delete()
      nap()
      indexer.stop();
    }
  }

  test("Injected changes are reported individually") {
    run(Map[String, String]("a.txt" -> "", "b.txt" -> "")) { (monitor: Monitor, root: PathLike) =>
      val changed = mutable.ArrayBuffer[String]()
      var rescans = 0

      val indexer = new Indexer(root, monitor) {
        override def onFilesChanged(paths: Array[String], rescan: Boolean): Unit = {
          if (rescan)
            rescans += 1
          paths.foreach(changed.append)
        }
      }

      val aFile = root.join("a.txt").toString
      val bFile = root.join("b.txt").toString

      indexer.injectFileChange(aFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile))
      nap()

      indexer.injectFileChange(bFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile, bFile))
      nap()

      indexer.injectFileChange(aFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile, bFile))

      indexer.stop();
    }
  }

  test("Injected changes cause rescan if close together") {
    run(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { (monitor: Monitor, root: PathLike) =>
      val changed = mutable.ArrayBuffer[String]()
      var rescans = 0

      val indexer = new Indexer(root, monitor) {
        override def onFilesChanged(paths: Array[String], rescan: Boolean): Unit = {
          if (rescan)
            rescans += 1
          paths.foreach(changed.append)
        }
      }

      val aFile = root.join("a.txt").toString
      val bFile = root.join("b.txt").toString
      val cFile = root.join("dir1/c.txt").toString

      indexer.injectFileChange(aFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile))

      indexer.injectFileChange(bFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile))

      indexer.injectFileChange(cFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile))

      nap()
      assert(rescans == 1)
      assert(changed.toSet == Set(aFile)) // Just a as rescan won't find any changes for b & c

      indexer.stop()
    }
  }

  test("Indexer can revert to file by file reporting after scanning") {
    run(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { (monitor: Monitor, root: PathLike) =>
      val changed = mutable.ArrayBuffer[String]()
      var rescans = 0

      val indexer = new Indexer(root, monitor) {
        override def onFilesChanged(paths: Array[String], rescan: Boolean): Unit = {
          if (rescan)
            rescans += 1
          paths.foreach(changed.append)
        }
      }

      val aFile = root.join("a.txt").toString
      val bFile = root.join("b.txt").toString
      val cFile = root.join("dir1/c.txt").toString

      indexer.injectFileChange(aFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile))

      indexer.injectFileChange(bFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile))

      // Inject after scan timeout
      nap()
      indexer.injectFileChange(cFile)
      assert(rescans == 1)
      assert(changed.toSet == Set(aFile, cFile))

      indexer.stop()
    }
  }

  test("Indexer can do back to back scanning") {
    run(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { (monitor: Monitor, root: PathLike) =>
      val changed = mutable.ArrayBuffer[String]()
      var rescans = 0

      val indexer = new Indexer(root, monitor) {
        override def onFilesChanged(paths: Array[String], rescan: Boolean): Unit = {
          if (rescan)
            rescans += 1
          paths.foreach(changed.append)
        }
      }

      val aFile = root.join("a.txt").toString
      val bFile = root.join("b.txt").toString

      // 1st Scan
      indexer.injectFileChange(aFile)
      indexer.injectFileChange(bFile)
      nap()
      assert(rescans == 1)
      assert(changed.toSet == Set(aFile))

      // 2nd Scan
      indexer.injectFileChange(bFile)
      indexer.injectFileChange(aFile)
      nap()
      assert(rescans == 2)
      assert(changed.toSet == Set(aFile, bFile))

      indexer.stop()
    }
  }
}
