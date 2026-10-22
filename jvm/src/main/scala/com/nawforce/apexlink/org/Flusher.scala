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

import java.lang.ref.WeakReference
import scala.collection.mutable
import scala.util.control.NonFatal

trait RefreshListener {
  def onRefreshOne(orgPath: PathLike, updatedPath: PathLike): Unit
  def onRefreshMany(orgPath: PathLike, updatedPaths: Seq[PathLike]): Unit
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
    org.refreshLock.synchronized {
      if (!request.highPriority || refreshQueue.nonEmpty) {
        refreshQueue.enqueue(request)
      } else {
        val updated = request.pkg.refreshBatched(Seq(request))
        // Notify of updated path
        if (updated) listener.foreach(_.onRefreshOne(org.path, request.path))

        // Tell auto flush we skipped the queue
        skippedQueue |= updated
      }
    }
  }

  def queueAll(request: Iterable[RefreshRequest]): Unit = {
    org.refreshLock.synchronized { refreshQueue.enqueueAll(request) }
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
        if (updated) listener.foreach(_.onRefreshMany(org.path, updatedPaths.toSeq))

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
    extends Flusher(org, parsedCache) {

  CacheFlusher.start(this)

  private def queueSize: Int   = org.refreshLock.synchronized { refreshQueue.size }
  private def skipped: Boolean = org.refreshLock.synchronized { skippedQueue }

  /* Process the queue if it has been stable since startSize was taken, or flush if a priority
   * update skipped the queue. */
  private def process(startSize: Int): Unit = {
    val endSize = queueSize
    if (startSize > 0 && startSize == endSize) {
      refreshAndFlush()
    } else if (skipped) {
      flush()
    }
  }
}

object CacheFlusher {

  /* The thread holds the flusher weakly so it stops once the org is no longer in use, rather than
   * keeping the org alive and polling for the lifetime of the process. It is created with no current
   * org, as the thread would otherwise inherit a strong reference to it. */
  private def start(flusher: CacheFlusher): Unit = {
    val ref = new WeakReference(flusher)
    OrgInfo.current.withValue(null) {
      val t = new Thread(() => run(ref))
      t.setDaemon(true)
      t.setName("apex-link cache flusher")
      t.start()
    }
  }

  private def run(ref: WeakReference[CacheFlusher]): Unit = {
    var startSize = Option(ref.get()).map(_.queueSize)
    while (startSize.nonEmpty) {
      Thread.sleep(1000)
      startSize = Option(ref.get()).map(flusher => {
        try {
          flusher.process(startSize.get)
        } catch {
          case NonFatal(ex) => LoggerOps.info("Cache flush failed", ex)
        }
        flusher.queueSize
      })
    }
  }
}
