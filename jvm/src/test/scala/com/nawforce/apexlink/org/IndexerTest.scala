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

  def run[T](files: Map[String, String])(verify: PathLike => T): T = {
    FileSystemHelper.runTempDir(files) { root =>
      val oldConfig = ServerOps.setIndexerConfiguration(IndexerConfiguration(50, 200))
      try {
        verify(root)
      } finally {
        ServerOps.setIndexerConfiguration(oldConfig)
      }
    }
  }

  test("Root directory deletion is safe") {
    run(Map[String, String]()) { root: PathLike =>
      val indexer = new Indexer(root) {
        override def onFilesChanged(path: Array[String], rescan: Boolean): Unit = {}
      }

      Thread.sleep(300)
      root.delete()
      Thread.sleep(300)
      indexer.stop();
    }
  }

  test("Injected changes are reported individually") {
    run(Map[String, String]("a.txt" -> "", "b.txt" -> "")) { root: PathLike =>
      val changed = mutable.ArrayBuffer[String]()
      var rescans = 0

      val indexer = new Indexer(root) {
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
      Thread.sleep(100)

      indexer.injectFileChange(bFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile, bFile))
      Thread.sleep(100)

      indexer.injectFileChange(aFile)
      assert(rescans == 0)
      assert(changed.toSet == Set(aFile, bFile))

      indexer.stop();
    }
  }

  test("Injected changes cause rescan if close together") {
    run(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val changed = mutable.ArrayBuffer[String]()
      var rescans = 0

      val indexer = new Indexer(root) {
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

      Thread.sleep(250)
      assert(rescans == 1)
      assert(changed.toSet == Set(aFile)) // Just a as rescan won't find any changes for b & c

      indexer.stop()
    }
  }

  test("Indexer can revert to file by file reporting after scanning") {
    run(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val changed = mutable.ArrayBuffer[String]()
      var rescans = 0

      val indexer = new Indexer(root) {
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
      Thread.sleep(250)
      indexer.injectFileChange(cFile)
      assert(rescans == 1)
      assert(changed.toSet == Set(aFile, cFile))

      indexer.stop()
    }
  }

  test("Indexer can do back to back scanning") {
    run(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val changed = mutable.ArrayBuffer[String]()
      var rescans = 0

      val indexer = new Indexer(root) {
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
      Thread.sleep(250)
      assert(rescans == 1)
      assert(changed.toSet == Set(aFile))

      // 2nd Scan
      indexer.injectFileChange(bFile)
      indexer.injectFileChange(aFile)
      Thread.sleep(250)
      assert(rescans == 2)
      assert(changed.toSet == Set(aFile, bFile))

      indexer.stop()
    }
  }
}
