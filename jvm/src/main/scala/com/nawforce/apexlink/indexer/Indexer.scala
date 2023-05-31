/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.indexer

import com.nawforce.apexlink.api.ServerOps
import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.pkgforce.documents.DirectoryTree
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.platform.Path

import java.util.concurrent._
import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/* File system watcher that detects rapid mass file change events and performs a delayed recursive re-scan to ensure
 * that all changes are being detected. The detection is very simple, we assume a mass change is happening if we see
 * two changes occur within a short period. The assumption here is that change events won't be lost if the rate
 * of change is always less than the fixed trigger interval. A re-scan is only started after a quite period with no
 * file change events to ensure the mass change event has completed. */
abstract class Indexer(path: PathLike, launcher: Monitor) extends Callable[Unit] {

  // Root in Path form, to allow access to 'native' on JVM
  private val rootPath: Path = path.asInstanceOf[Path]

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

  // Current in-memory state of the tree, used to calculate changes
  private var root: Option[DirectoryTree] = {
    if (config.enabled) {
      LoggerOps.debugTime(s"Indexer scanned $rootPath") {
        val index = DirectoryTree(rootPath, new ArrayBuffer[String]())
        launcher.monitor(
          rootPath,
          (file: String) => {
            onFileChange(file)
          }
        )
        Some(index)
      }
    } else {
      LoggerOps.debug(s"Indexer for $rootPath not started due to config")
      None
    }
  }

  /* Override to detect file changes */
  def onFilesChanged(path: Array[String], rescan: Boolean): Unit

  // Inject a change, mostly useful for testing
  def injectFileChange(file: String): Unit = {
    onFileChange(file)
  }

  // Stop the indexer, mostly useful for testing, see also Monitor.stop()
  def stop(): Unit = {
    rescanFuture.foreach(f => f.cancel(false))
    scheduler.shutdown()
  }

  // Detect action to be taken on each reported change
  private def onFileChange(file: String): Unit = {
    callbackLock.synchronized {
      val now = System.currentTimeMillis()
      if (rescanFuture.nonEmpty || now - lastEventTick < config.rescanTriggerTimeMs) {
        rescanFuture.foreach(_.cancel(false))
        rescanFuture = Some(
          scheduler
            .schedule[Unit](this, config.quietPeriodForRescanMs, TimeUnit.MILLISECONDS)
        )
      } else {
        onFilesChanged(Array(file), rescan = false)
      }
      lastEventTick = now
    }
  }

  // Timer callback for detect end of quiet period after mass change
  def call(): Unit = {
    rescanFuture = None
    refresh()
  }

  // Perform a full scan of the directory to capture all changes
  private def refresh(): Unit = {
    LoggerOps.debugTime(s"Indexer re-scanned $rootPath") {
      val changed = new mutable.ArrayBuffer[String]()
      root = root.map(_.refresh(changed))
      onFilesChanged(changed.toArray, rescan = true)
    }
  }
}
