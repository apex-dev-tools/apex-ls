/*
 * Copyright (c) 2023 Certinia, inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.api.{IndexerConfiguration, ServerOps}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

/*
 * NOTE: The handling here depends on timing, I have used Thread.sleep() to control this but it may need some
 * adjustment to maintain passing tests.
 */
class IndexerTest extends AnyFunSuite with TestHelper {

  test("Root directory deletion is safe") {
    FileSystemHelper.runTempDir(Map[String, String]()) { root: PathLike =>
      ServerOps.setIndexerConfiguration(IndexerConfiguration(50, 200))
      new Indexer(root) {
        override def onFilesChanged(path: Array[String], rescan: Boolean): Unit = {}
      }

      Thread.sleep(300)
      root.delete()
      Thread.sleep(300)
    }
  }

  test("Injected changes are reported individually") {
    FileSystemHelper.runTempDir(Map[String, String]("a.txt" -> "", "b.txt" -> "")) {
      root: PathLike =>
        val changed = mutable.ArrayBuffer[String]()
        var rescans = 0

        ServerOps.setIndexerConfiguration(IndexerConfiguration(50, 200))
        val monitor = new Indexer(root) {
          override def onFilesChanged(path: Array[String], rescan: Boolean): Unit = {
            if (rescan)
              rescans += 1
            path.foreach(changed.append)
          }
        }

        val aFile = root.join("a.txt").toString
        val bFile = root.join("b.txt").toString

        monitor.injectFileChange(aFile)
        assert(rescans == 0)
        assert(changed.toSet == Set(aFile))
        Thread.sleep(100)

        monitor.injectFileChange(bFile)
        assert(rescans == 0)
        assert(changed.toSet == Set(aFile, bFile))
        Thread.sleep(100)

        monitor.injectFileChange(aFile)
        assert(rescans == 0)
        assert(changed.toSet == Set(aFile, bFile))
        Thread.sleep(100)
    }
  }

  test("Injected changes cause rescan if close together") {
    FileSystemHelper.runTempDir(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val changed           = mutable.ArrayBuffer[String]()
      @volatile var rescans = 0

      ServerOps.setIndexerConfiguration(IndexerConfiguration(50, 100))
      val monitor = new Indexer(root) {
        override def onFilesChanged(path: Array[String], rescan: Boolean): Unit = {
          if (rescan)
            rescans += 1
          path.foreach(changed.append)
        }
      }

      val aFile = root.join("a.txt").toString
      val bFile = root.join("b.txt").toString
      val cFile = root.join("dir1/c.txt").toString

      monitor.injectFileChange(aFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile))

      monitor.injectFileChange(bFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile))

      monitor.injectFileChange(cFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile))

      Thread.sleep(200)
      assert(rescans == 1)
      assert(changed.toSet == Set(aFile)) // Just a as rescan won't find any changes for b & c
    }
  }

  test("Indexer can revert to file by file reporting after scanning") {
    FileSystemHelper.runTempDir(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val changed           = mutable.ArrayBuffer[String]()
      @volatile var rescans = 0

      ServerOps.setIndexerConfiguration(IndexerConfiguration(50, 100))
      val monitor = new Indexer(root) {
        override def onFilesChanged(paths: Array[String], rescan: Boolean): Unit = {
          if (rescan)
            rescans += 1
          paths.foreach(changed.append)
        }
      }

      val aFile = root.join("a.txt").toString
      val bFile = root.join("b.txt").toString
      val cFile = root.join("dir1/c.txt").toString

      monitor.injectFileChange(aFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile))

      monitor.injectFileChange(bFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile))

      // Inject after scan timeout
      Thread.sleep(200)
      monitor.injectFileChange(cFile)
      assert(rescans == 1)
      assert(changed.toSet == Set(aFile, cFile))
    }
  }

  test("Indexer can do back to back scanning") {
    FileSystemHelper.runTempDir(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val changed           = mutable.ArrayBuffer[String]()
      @volatile var rescans = 0

      ServerOps.setIndexerConfiguration(IndexerConfiguration(50, 100))
      val monitor = new Indexer(root) {
        override def onFilesChanged(paths: Array[String], rescan: Boolean): Unit = {
          if (rescan)
            rescans += 1
          paths.foreach(changed.append)
        }
      }

      val aFile = root.join("a.txt").toString
      val bFile = root.join("b.txt").toString

      // 1st Scan
      monitor.injectFileChange(aFile)
      monitor.injectFileChange(bFile)
      Thread.sleep(200)
      assert(rescans == 1)
      assert(changed.toSet == Set(aFile))

      // 2nd Scan
      monitor.injectFileChange(bFile)
      monitor.injectFileChange(aFile)
      Thread.sleep(200)
      assert(rescans == 2)
      assert(changed.toSet == Set(aFile, bFile))
    }
  }

}
