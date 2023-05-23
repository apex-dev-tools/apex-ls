/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */

package com.nawforce.apexlink.indexer

import better.files
import com.nawforce.apexlink.api.ServerOps
import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.platform.Path
import io.methvin.better.files.RecursiveFileMonitor
import io.methvin.watcher.hashing.FileHasher

import java.nio.file.WatchEvent
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

/* Workspace FS change monitor. OS X fsevents only supports recursive monitoring which makes selective monitoring of
 parts of a workspace difficult since you can not use a non-recursive monitor of parent directories to track changes
 to the root of the sub-directories you are specifically interested in, such as a interesting sub-directory being
 created after you have started monitoring. To work around this we use a single recursive monitor for the whole
 workspace but support quick filtering by subdirectories. */
class Monitor(workspace: PathLike) {

  private val workspacePath = workspace.asInstanceOf[Path]
  private val callbacks     = mutable.Map[String, String => Unit]()

  // Workspace wide recursive monitor, dispatches via callbacks on case-sensitive match of absolute path
  private val workspaceMonitor = {
    if (ServerOps.getIndexerConfiguration.enabled) {
      Some(new RecursiveFileMonitor(workspacePath.native, Some(FileHasher.LAST_MODIFIED_TIME)) {
        override def onEvent(
          eventType: WatchEvent.Kind[java.nio.file.Path],
          file: files.File,
          count: Int
        ): Unit = {
          println(s"onEvent(file=${file.path.toString}, isDirectory: ${file.isDirectory})")
          if (!file.isDirectory) {
            val path = file.path.toAbsolutePath.toString
            println(s"onEvent(absPath=$path, callback paths=${callbacks.keys.mkString(",")})")
            callbacks
              .find(entry => path.startsWith(entry._1))
              .foreach(entry => {
                println("Invoking a callback")
                entry._2(path)
                println("Callback done")
              })
          }
        }
      })
    } else {
      None
    }
  }
  workspaceMonitor.foreach(_.start())

  /* Add monitoring for a sub-directory of the workspace. The monitored directory can not be a child of an existing
   * monitored directory. */
  def monitor(path: PathLike, onFileChanged: String => Unit): Unit = {
    if (workspace != path && !workspace.isParentOf(path))
      throw new IllegalArgumentException(
        s"Path ${path.toString} must be the whole workspace or a subdirectory of ${workspace.toString}"
      )

    callbacks.keys
      .find(p => Path(p).isParentOf(path))
      .foreach(monitoredParent => {
        throw new IllegalArgumentException(
          s"Path ${path.toString} can not be a subdirectory of another monitored path $monitoredParent"
        )
      })

    callbacks.put(path.asInstanceOf[Path].native.toAbsolutePath.toString, onFileChanged)
  }

  /* Terminate the monitoring, mostly useful for testing */
  def stop(): Unit = {
    workspaceMonitor.foreach(_.stop())
  }
}
