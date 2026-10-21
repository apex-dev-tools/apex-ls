/*
 Copyright (c) 2026 Kevin Jones, All rights reserved.
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

import com.nawforce.apexlink.api.{Org, ServerOps}
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

import java.lang.ref.WeakReference
import scala.jdk.CollectionConverters._

class CacheFlusherTest extends AnyFunSuite {

  private def flusherThreads: Set[Thread] =
    Thread.getAllStackTraces.keySet.asScala.filter(_.getName == "apex-link cache flusher").toSet

  private def newOrgRef(): (WeakReference[Org], Set[Thread]) = {
    val before = flusherThreads
    FileSystemHelper.run(Map("Dummy.cls" -> "public class Dummy {}")) { root: PathLike =>
      val org = Org.newOrg(root)
      (new WeakReference(org), flusherThreads -- before)
    }
  }

  test("Flusher thread stops once its org is unused") {
    assume(ServerOps.isAutoFlushEnabled)
    // Shared worker threads started by the first org load keep a reference to it, so measure a later one
    newOrgRef()
    val (orgRef, started) = newOrgRef()
    assert(started.size == 1)

    val deadline = System.currentTimeMillis() + 30000
    while (
      (orgRef.get() != null || started.exists(_.isAlive)) && System.currentTimeMillis() < deadline
    ) {
      System.gc()
      Thread.sleep(100)
    }

    assert(orgRef.get() == null)
    assert(!started.exists(_.isAlive))
  }
}
