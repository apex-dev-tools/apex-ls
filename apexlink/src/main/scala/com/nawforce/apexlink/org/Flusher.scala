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

import com.nawforce.pkgforce.documents.ParsedCache
import com.nawforce.pkgforce.memory.Cleanable
import com.nawforce.pkgforce.path.PathLike

import scala.collection.mutable

case class RefreshRequest(pkg: OPM.PackageImpl, path: PathLike, highPriority: Boolean)

class Flusher(org: OPM.OrgImpl, parsedCache: Option[ParsedCache]) {
  protected val refreshQueue = new mutable.Queue[RefreshRequest]()
  private var expired        = false

  def isDirty: Boolean = {
    org.refreshLock.synchronized { refreshQueue.nonEmpty }
  }

  def queue(request: RefreshRequest): Unit = {
    if (!request.highPriority || refreshQueue.nonEmpty) {
      refreshQueue.enqueue(request)
    } else {
      org.refreshLock.synchronized {
        request.pkg.refreshBatched(Seq(request))
      }
    }
  }

  def refreshAndFlush(): Boolean = {
    OrgInfo.current.withValue(org) {
      org.refreshLock.synchronized {
        var updated  = false
        val packages = org.packages

        // Process in chunks, new requests may be queued during processing
        while (refreshQueue.nonEmpty) {
          val toProcess = refreshQueue.dequeueAll(_ => true)
          packages
            .foreach(pkg => {
              updated |= pkg.refreshBatched(toProcess.filter(_.pkg == pkg))
            })
        }

        // Flush to cache
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
        updated
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
    def queueSize: Int = org.refreshLock.synchronized { refreshQueue.size }

    while (true) {
      // Wait for non-zero queue to be stable
      var stable = false
      while (!stable) {
        val start = queueSize
        Thread.sleep(1000)
        val end = queueSize
        stable = start > 0 && start == end
      }

      // Process refresh requests & flush
      refreshAndFlush()
    }
  }
}
