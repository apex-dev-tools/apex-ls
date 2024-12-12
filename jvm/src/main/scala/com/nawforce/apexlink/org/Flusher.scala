/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
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

import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.pkgforce.documents.ParsedCache
import com.nawforce.pkgforce.memory.Cleanable
import com.nawforce.pkgforce.path.PathLike

import scala.collection.mutable

trait RefreshListener {
  def onRefresh(orgPath: PathLike, updatedPath: PathLike): Unit
  def onRefreshAll(orgPath: PathLike, updatedPaths: Seq[PathLike]): Unit
}

case class RefreshRequest(pkg: OPM.PackageImpl, path: PathLike, highPriority: Boolean)

class Flusher(org: OPM.OrgImpl, parsedCache: Option[ParsedCache]) {
  protected val refreshQueue                    = new mutable.Queue[RefreshRequest]()
  protected var skippedQueue                    = false
  private var expired                           = false
  private var listener: Option[RefreshListener] = None

  def setListener(rl: Option[RefreshListener]): Unit = listener = rl

  def isDirty: Boolean = {
    org.refreshLock.synchronized { refreshQueue.nonEmpty }
  }

  def queue(request: RefreshRequest): Unit = {
    if (!request.highPriority || refreshQueue.nonEmpty) {
      refreshQueue.enqueue(request)
    } else {
      org.refreshLock.synchronized {
        val updated = request.pkg.refreshBatched(Seq(request))
        // Notify of updated path
        if (updated) listener.foreach(_.onRefresh(org.path, request.path))

        // Tell auto flush we skipped the queue
        skippedQueue |= updated
      }
    }
  }

  def queueAll(request: Iterable[RefreshRequest]): Unit = {
    refreshQueue.enqueueAll(request)
  }

  def refreshAndFlush(): Boolean = {
    OrgInfo.current.withValue(org) {
      org.refreshLock.synchronized {
        var updated      = false
        val updatedPaths = mutable.Set[PathLike]()
        val packages     = org.packages

        // Process in chunks, new requests may be queued during processing
        while (refreshQueue.nonEmpty) {
          val toProcess = refreshQueue.dequeueAll(_ => true)
          LoggerOps.debug(s"Batched refresh starting for ${toProcess.length} items")
          packages
            .foreach(pkg => {
              val reqs = toProcess.filter(_.pkg == pkg)
              updated |= pkg.refreshBatched(reqs)

              if (updated) updatedPaths.addAll(reqs.map(_.path))
            })
          LoggerOps.debug(s"Batched refresh completed")
        }

        // Flush to cache
        flush()

        // Notify of updated paths
        if (updated) listener.foreach(_.onRefreshAll(org.path, updatedPaths.toSeq))

        updated
      }
    }
  }

  protected def flush(): Unit = {
    OrgInfo.current.withValue(org) {
      org.refreshLock.synchronized {
        val packages = org.packages

        // Reset skip status to prevent more flushes
        skippedQueue = false

        parsedCache.foreach(pc => {
          packages.foreach(pkg => {
            pkg.flush(pc)
          })
          if (!expired) {
            pc.expire()
            expired = true
          }
        })

        // Clean registered caches to reduce memory
        Cleanable.clean()
      }
    }
  }

}

class CacheFlusher(org: OPM.OrgImpl, parsedCache: Option[ParsedCache])
    extends Flusher(org, parsedCache)
    with Runnable {

  private val t = new Thread(this)
  t.setDaemon(true)
  t.setName("apex-link cache flusher")
  t.start()

  override def run(): Unit = {
    def queueSize: Int   = org.refreshLock.synchronized { refreshQueue.size }
    def skipped: Boolean = org.refreshLock.synchronized { skippedQueue }

    while (true) {
      // Wait for non-zero queue to be stable
      // Or with an empty queue and a priority/single update
      var stable = false
      var skip   = false
      while (!stable && !skip) {
        val start = queueSize
        Thread.sleep(1000)
        val end = queueSize
        stable = start > 0 && start == end
        skip = skipped
      }

      if (stable) {
        // Process refresh requests & flush
        refreshAndFlush()
      } else if (skip) {
        // Already refreshed, just flush
        flush()
      }
    }
  }
}
