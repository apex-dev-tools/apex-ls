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
package com.nawforce.pkgforce.memory

import com.nawforce.pkgforce.names.Names
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.{CyclicBarrier, Executors, TimeUnit}

class InternCacheTest extends AnyFunSuite {

  test("intern cache returns one instance to concurrent callers") {
    val cache = new InternCache[String]
    assertConcurrentInterning(index => cache.intern(new String(s"value-$index")))
  }

  test("names returns one instance to concurrent callers") {
    val prefix = s"concurrent-name-${System.nanoTime()}"
    assertConcurrentInterning(index => Names(new String(s"$prefix-$index")))
  }

  private def assertConcurrentInterning(intern: Int => AnyRef): Unit = {
    val workers  = 8
    val rounds   = 256
    val barrier  = new CyclicBarrier(workers)
    val results  = Array.ofDim[AnyRef](rounds, workers)
    val executor = Executors.newFixedThreadPool(workers)

    try {
      val futures = (0 until workers).map(worker =>
        executor.submit(new Runnable {
          override def run(): Unit = {
            (0 until rounds).foreach(round => {
              barrier.await()
              results(round)(worker) = intern(round)
              barrier.await()
            })
          }
        })
      )
      futures.foreach(_.get(30, TimeUnit.SECONDS))
    } finally {
      executor.shutdownNow()
    }

    results.foreach(instances => assert(instances.tail.forall(_ eq instances.head)))
  }
}
