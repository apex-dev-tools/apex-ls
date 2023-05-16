/*
 * Copyright (c) 2023 Certinia, inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import better.files
import com.nawforce.apexlink.api.ServerOps
import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.pkgforce.path.PathLike
import io.methvin.better.files.RecursiveFileMonitor

import java.nio.file.{Paths, WatchEvent}
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{
  Callable,
  Executors,
  ScheduledExecutorService,
  ScheduledFuture,
  ScheduledThreadPoolExecutor,
  TimeUnit
}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global

/* File system watcher that detects rapid mass file change events and performs a delayed recursive re-scan to ensure
 * that all changes are being detected. The detection is very simple, we assume a mass change is happening if we see
 * two changes occur within a short period. The assumption here is that change events won't be lost if the rate
 * of change is always less than the fixed trigger interval. A re-scan is only started after a quite period with no
 * file change events to ensure the mass change event has completed. Change detection is done via modification time
 * rather than hashing for performance, on file systems that don't support sub second times this could cause some
 * changes to be missed.*/
abstract class Indexer(rootPath: PathLike) extends Callable[Unit] {

  // Re-scan will fire if two events are less then this apart
  private final val config = ServerOps.getIndexerConfiguration

  // Last file change event time tick
  private var lastEventTick: Long = 0

  // Next re-scan callback, cancel future to remove callback
  private var rescanFuture: Option[ScheduledFuture[Unit]] = None

  // Thread pool for scheduled callbacks, we only ever need 1
  private lazy val scheduler: ScheduledExecutorService = {
    val s = Executors.newScheduledThreadPool(1).asInstanceOf[ScheduledThreadPoolExecutor]
    s.setRemoveOnCancelPolicy(true)
    s
  }

  // Acquire within callbacks to protect state
  private val callbackLock = new ReentrantLock(true)

  private var root: Option[DirectoryIndex] = {
    if (config.rescanTriggerTimeMs > 0 && config.quietPeriodForRescanMs > 0) {
      LoggerOps.debugTime(s"Indexer scanned $rootPath") {
        val index = DirectoryIndex(rootPath, new ArrayBuffer[String]())
        index.foreach(_ => startMonitor(this))
        index
      }
    } else {
      LoggerOps.debug(s"Indexer for $rootPath not started due to config")
      None
    }
  }

  /* Override to detect file changes */
  def onFilesChanged(path: Array[String]): Unit

  private def startMonitor(callable: Callable[Unit]): Unit = {
    new RecursiveFileMonitor(Paths.get(rootPath.toString)) {
      override def onEvent(
        eventType: WatchEvent.Kind[java.nio.file.Path],
        file: files.File,
        count: Int
      ): Unit = {
        callbackLock.synchronized {
          val now = System.currentTimeMillis()
          if (rescanFuture.nonEmpty || now - lastEventTick < config.rescanTriggerTimeMs) {
            rescanFuture.foreach(_.cancel(false))
            rescanFuture = Some(
              scheduler
                .schedule[Unit](callable, config.quietPeriodForRescanMs, TimeUnit.MILLISECONDS)
            )
          } else {
            onFilesChanged(Array(file.path.toString))
          }
          lastEventTick = now
        }
      }
    }.start()
    LoggerOps.debug(s"Indexer monitor for $rootPath started")
  }

  override def call(): Unit = {
    callbackLock.synchronized {
      rescanFuture = None
      LoggerOps.debugTime(s"Indexer re-scanned $rootPath") {
        val changed = new mutable.ArrayBuffer[String]()
        root = root.flatMap(_.refresh(changed))
        onFilesChanged(changed.toArray)
      }
    }
  }

  /* Directory index node for maintaining known state. */
  class DirectoryIndex private (val path: PathLike) {
    private var fileModTimes   = Array[(String, Long)]()
    private var subDirectories = Array[DirectoryIndex]()

    /* Create a refreshed version of this node, may return None is node directory has been deleted */
    def refresh(changed: mutable.ArrayBuffer[String]): Option[DirectoryIndex] = {
      if (path.isDirectory) {
        val (files, directories) = path.splitDirectoryEntries()
        refreshSubdirectories(directories, changed)
        refreshFiles(files, changed)
        Some(this)
      } else {
        None
      }
    }

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
        .flatMap(dir => {
          existing.get(dir) match {
            case None            => DirectoryIndex(dir, changed)
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

  private object DirectoryIndex {
    def apply(path: PathLike, changed: mutable.ArrayBuffer[String]): Option[DirectoryIndex] = {
      val dir = new DirectoryIndex(path)
      dir.refresh(changed)
    }
  }
}
