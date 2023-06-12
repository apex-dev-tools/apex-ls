/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.pkgforce.documents

import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import com.nawforce.runtime.platform.Path
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class DirectoryTreeTest extends AnyFunSuite with BeforeAndAfter {

  test("Missing dir creates empty tree") {
    val changed = mutable.ArrayBuffer[String]()
    val tree    = DirectoryTree(Path("something"), changed)
    assert(tree.path == Path("something"))
    assert(tree.directories.isEmpty)
    assert(tree.fileModificationTimes.isEmpty)
    assert(changed.isEmpty)
  }

  test("Empty dir creates single node tree") {
    FileSystemHelper.runTempDir(Map[String, String]()) { root: PathLike =>
      val changed = mutable.ArrayBuffer[String]()
      val tree    = DirectoryTree(Path(root), changed)
      assert(tree.path == root)
      assert(tree.directories.isEmpty)
      assert(tree.fileModificationTimes.isEmpty)
      assert(changed.isEmpty)
    }
  }

  test("Non-empty dir creates tree") {
    FileSystemHelper.runTempDir(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val changed = mutable.ArrayBuffer[String]()
      val tree    = DirectoryTree(Path(root), changed)
      assert(tree.path == root)

      val aFilePath = root.join("a.txt").toString
      val bFilePath = root.join("b.txt").toString
      val cFilePath = root.join("dir1/c.txt").toString
      val dFilePath = root.join("dir1/dir2/d.txt").toString
      assert(tree.fileModificationTimes.keys.toSet == Set(aFilePath, bFilePath))
      assert(!tree.fileModificationTimes.exists(_._2 == 0))

      assert(tree.directories.length == 1)
      val dir1 = tree.directories(0)
      assert(dir1.fileModificationTimes.keys.toSet == Set(cFilePath))
      assert(!dir1.fileModificationTimes.exists(_._2 == 0))

      assert(dir1.directories.length == 1)
      val dir2 = dir1.directories(0)
      assert(dir2.directories.isEmpty)
      assert(dir2.fileModificationTimes.keys.toSet == Set(dFilePath))
      assert(!dir2.fileModificationTimes.exists(_._2 == 0))

      assert(changed.toSet == Set(aFilePath, bFilePath, cFilePath, dFilePath))
    }
  }

  test("Refresh creates change list for added file") {
    FileSystemHelper.runTempDir(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val tree = DirectoryTree(Path(root), mutable.ArrayBuffer[String]())

      val eFilePath = root.join("e.txt")
      eFilePath.write("")

      val changed = mutable.ArrayBuffer[String]()
      val newTree = tree.refresh(changed)

      assert(changed.toSet == Set(eFilePath.toString))

      val aFilePath = root.join("a.txt").toString
      val bFilePath = root.join("b.txt").toString
      assert(
        newTree.fileModificationTimes.keys.toSet == Set(aFilePath, bFilePath, eFilePath.toString)
      )
      assert(!newTree.fileModificationTimes.exists(_._2 == 0))
    }
  }

  test("Refresh creates change list for modified file") {
    FileSystemHelper.runTempDir(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val tree = DirectoryTree(Path(root), mutable.ArrayBuffer[String]())

      // We need a new mod time here but can't sleep in ScalaJS so do busy wait
      val bFilePath   = root.join("b.txt")
      val origModTime = bFilePath.lastModified().get
      while (bFilePath.lastModified().get == origModTime)
        bFilePath.write("")

      val changed = mutable.ArrayBuffer[String]()
      val newTree = tree.refresh(changed)

      assert(changed.toSet == Set(bFilePath.toString))

      val aFilePath = root.join("a.txt").toString
      assert(newTree.fileModificationTimes.keys.toSet == Set(aFilePath, bFilePath.toString))
      assert(!newTree.fileModificationTimes.exists(_._2 == 0))
    }
  }

  test("Refresh creates change list for deleted file") {
    FileSystemHelper.runTempDir(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val tree = DirectoryTree(Path(root), mutable.ArrayBuffer[String]())

      val bFilePath = root.join("b.txt")
      bFilePath.delete()

      val changed = mutable.ArrayBuffer[String]()
      val newTree = tree.refresh(changed)

      assert(changed.toSet == Set(bFilePath.toString))

      val aFilePath = root.join("a.txt").toString
      assert(newTree.fileModificationTimes.keys.toSet == Set(aFilePath))
      assert(!newTree.fileModificationTimes.exists(_._2 == 0))

    }
  }

  test("Refresh creates change list for subdir added file") {
    FileSystemHelper.runTempDir(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val tree = DirectoryTree(Path(root), mutable.ArrayBuffer[String]())

      val eFilePath = root.join("dir1/e.txt")
      eFilePath.write("")

      val changed = mutable.ArrayBuffer[String]()
      val newTree = tree.refresh(changed)

      assert(changed.toSet == Set(eFilePath.toString))

      val cFilePath = root.join("dir1/c.txt").toString
      assert(
        newTree.directories(0).fileModificationTimes.keys.toSet == Set(
          cFilePath,
          eFilePath.toString
        )
      )
      assert(!newTree.directories(0).fileModificationTimes.exists(_._2 == 0))
    }
  }

  test("Refresh creates change list for subdir modified file") {
    FileSystemHelper.runTempDir(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val tree = DirectoryTree(Path(root), mutable.ArrayBuffer[String]())

      // We need a new mod time here but can't sleep in ScalaJS so do busy wait
      val cFilePath   = root.join("dir1/c.txt")
      val origModTime = cFilePath.lastModified().get
      while (cFilePath.lastModified().get == origModTime)
        cFilePath.write("")

      val changed = mutable.ArrayBuffer[String]()
      val newTree = tree.refresh(changed)

      assert(changed.toSet == Set(cFilePath.toString))
      assert(newTree.directories(0).fileModificationTimes.keys.toSet == Set(cFilePath.toString))
      assert(!newTree.directories(0).fileModificationTimes.exists(_._2 == 0))
    }
  }

  test("Refresh creates change list for subdir deleted file") {
    FileSystemHelper.runTempDir(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val tree = DirectoryTree(Path(root), mutable.ArrayBuffer[String]())

      val cFilePath = root.join("dir1/c.txt")
      cFilePath.delete()

      val changed = mutable.ArrayBuffer[String]()
      val newTree = tree.refresh(changed)

      assert(changed.toSet == Set(cFilePath.toString))
      assert(newTree.directories(0).fileModificationTimes.keys.isEmpty)
    }
  }

  test("Refresh creates change list for subdir deleted") {
    FileSystemHelper.runTempDir(
      Map[String, String]("a.txt" -> "", "b.txt" -> "", "dir1/c.txt" -> "", "dir1/dir2/d.txt" -> "")
    ) { root: PathLike =>
      val tree = DirectoryTree(Path(root), mutable.ArrayBuffer[String]())

      val dir1Path = root.join("dir1")
      val cPath    = dir1Path.join("c.txt")
      val dir2Path = dir1Path.join("dir2")
      val dPath    = dir2Path.join("d.txt")

      assert(dPath.delete().isEmpty)
      assert(dir2Path.delete().isEmpty)
      assert(cPath.delete().isEmpty)
      assert(dir1Path.delete().isEmpty)

      val changed = mutable.ArrayBuffer[String]()
      val newTree = tree.refresh(changed)

      assert(changed.toSet == Set(cPath.toString, dPath.toString))
      assert(newTree.fileModificationTimes.size == 2)
      assert(newTree.directories.isEmpty)
    }
  }

}
