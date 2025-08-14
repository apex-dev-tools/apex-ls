/*
 Copyright (c) 2020 Kevin Jones, All rights reserved.
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

package com.nawforce.pkgforce.documents

import com.nawforce.runtime.platform.Environment
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class ParsedCacheTest extends AnyFunSuite with BeforeAndAfter {

  private val emptyPackageContext = PackageContext(None, Array(), Array(), Array(), Array(), false)

  before {
    ParsedCache.clear()
  }

  after {
    ParsedCache.clear()
  }

  test("homedir is available") {
    assert(Environment.homedir.nonEmpty)
  }

  test("default uses homedir") {
    val cache = ParsedCache.create(1)
    assert(cache.isRight)
    assert(
      cache.getOrElse(throw new NoSuchElementException()).path.parent == Environment.homedir.get
    )
  }

  test("custom path used") {
    try {
      val testPath = Environment.homedir.get.join(".apexlink_cache_test")
      Environment.setCacheDirOverride(Some(Some(testPath)))
      ParsedCache.clear()

      val cache = ParsedCache.create(1)
      assert(cache.isRight)
      assert(cache.getOrElse(throw new NoSuchElementException()).path == testPath)
      assert(testPath.delete().isEmpty)
    } finally {
      Environment.setCacheDirOverride(None)
    }
  }

  test("empty key insert/recover") {
    val cache = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    cache.upsert(emptyPackageContext, "", 0, "Hello".getBytes())
    assert(cache.get(emptyPackageContext, "", 0).get.sameElements("Hello".getBytes()))
    assert(cache.get(emptyPackageContext, "Foo", 0).isEmpty)
    assert(cache.get(emptyPackageContext, "", 1).isEmpty)
  }

  test("key insert/recover on name") {
    val cache = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    cache.upsert(emptyPackageContext, "Foo", 0, "Hello".getBytes())
    assert(cache.get(emptyPackageContext, "", 0).isEmpty)
    assert(
      cache
        .get(emptyPackageContext, "Foo", 0)
        .get
        .sameElements("Hello".getBytes())
    )
  }

  test("key insert/recover on content") {
    val cache = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    cache.upsert(emptyPackageContext, "", 1, "Hello".getBytes())
    assert(cache.get(emptyPackageContext, "", 0).isEmpty)
    assert(
      cache
        .get(emptyPackageContext, "", 1)
        .get
        .sameElements("Hello".getBytes())
    )
  }

  test("overwrite entry") {
    val cache = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    cache.upsert(emptyPackageContext, "Foo", 0, "Hello".getBytes())
    assert(
      cache
        .get(emptyPackageContext, "Foo", 0)
        .get
        .sameElements("Hello".getBytes())
    )
    cache.upsert(emptyPackageContext, "Foo", 0, "Goodbye".getBytes())
    assert(
      cache
        .get(emptyPackageContext, "Foo", 0)
        .get
        .sameElements("Goodbye".getBytes())
    )
  }

  test("key insert/recover wrong packageContext") {
    val cache = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    cache.upsert(emptyPackageContext, "Foo", 0, "Hello".getBytes())
    assert(
      cache
        .get(PackageContext(Some(""), Array(), Array(), Array(), Array(), false), "Foo", 0)
        .isEmpty
    )
    assert(
      cache
        .get(PackageContext(Some("Foo"), Array(), Array(), Array(), Array(), false), "Foo", 0)
        .isEmpty
    )
  }

  test("key insert/recover with namespaced packageContext") {
    val packageContext = PackageContext(Some("test"), Array(), Array(), Array(), Array(), false)
    val cache          = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    cache.upsert(packageContext, "Foo", 0, "Hello".getBytes())
    assert(cache.get(packageContext, "", 0).isEmpty)
    assert(cache.get(packageContext, "Foo", 0).get.sameElements("Hello".getBytes()))
  }

  test("key insert/recover with bad packageContext") {
    val packageContext =
      PackageContext(
        Some("test"),
        Array("ghosted1", "ghosted2"),
        Array("analysed1", "analysed2"),
        Array(),
        Array(),
        false
      )
    val cache = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    cache.upsert(packageContext, "Foo", 0, "Hello".getBytes())
    assert(cache.get(packageContext, "Foo", 0).get.sameElements("Hello".getBytes()))
    assert(
      cache
        .get(
          PackageContext(
            Some("test"),
            Array("ghosted1"),
            Array("analysed1", "analysed2"),
            Array(),
            Array(),
            false
          ),
          "Foo",
          0
        )
        .isEmpty
    )
    assert(
      cache
        .get(
          PackageContext(
            Some("test"),
            Array("ghosted2", "ghosted1"),
            Array("analysed1", "analysed2"),
            Array(),
            Array(),
            false
          ),
          "Foo",
          0
        )
        .isEmpty
    )
    assert(
      cache
        .get(
          PackageContext(
            Some("test"),
            Array("ghosted2", "ghosted1"),
            Array("analysed2"),
            Array(),
            Array(),
            false
          ),
          "Foo",
          0
        )
        .isEmpty
    )
    assert(
      cache
        .get(
          PackageContext(
            Some("test"),
            Array("ghosted1", "analysed1"),
            Array("ghosted1", "analysed2"),
            Array(),
            Array(),
            false
          ),
          "Foo",
          0
        )
        .isEmpty
    )
    assert(
      cache
        .get(PackageContext(Some("test"), Array(), Array(), Array(), Array(), false), "Foo", 0)
        .isEmpty
    )
  }

  test("cache invalidates when isLibrary changes") {
    val packageContext1 = PackageContext(Some("test"), Array(), Array(), Array(), Array(), false)
    val packageContext2 = PackageContext(Some("test"), Array(), Array(), Array(), Array(), true)
    val cache           = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    
    cache.upsert(packageContext1, "Foo", 0, "Hello".getBytes())
    assert(cache.get(packageContext1, "Foo", 0).get.sameElements("Hello".getBytes()))
    assert(cache.get(packageContext2, "Foo", 0).isEmpty)
  }

  test("cache invalidates when externalMetadataPaths changes") {
    val packageContext1 = PackageContext(Some("test"), Array(), Array(), Array(), Array(), false)
    val packageContext2 = PackageContext(Some("test"), Array(), Array(), Array(), Array("/path/to/external"), false)
    val cache           = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    
    cache.upsert(packageContext1, "Foo", 0, "Hello".getBytes())
    assert(cache.get(packageContext1, "Foo", 0).get.sameElements("Hello".getBytes()))
    assert(cache.get(packageContext2, "Foo", 0).isEmpty)
  }

  test("cache invalidates when additionalNamespaces changes") {
    val packageContext1 = PackageContext(Some("test"), Array(), Array(), Array(), Array(), false)
    val packageContext2 = PackageContext(Some("test"), Array(), Array(), Array("ns1"), Array(), false)
    val cache           = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    
    cache.upsert(packageContext1, "Foo", 0, "Hello".getBytes())
    assert(cache.get(packageContext1, "Foo", 0).get.sameElements("Hello".getBytes()))
    assert(cache.get(packageContext2, "Foo", 0).isEmpty)
  }

  test("cache handles multiple externalMetadataPaths") {
    val packageContext1 = PackageContext(Some("test"), Array(), Array(), Array(), Array("/path1", "/path2"), false)
    val packageContext2 = PackageContext(Some("test"), Array(), Array(), Array(), Array("/path1", "/path3"), false)
    val cache           = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    
    cache.upsert(packageContext1, "Foo", 0, "Hello".getBytes())
    assert(cache.get(packageContext1, "Foo", 0).get.sameElements("Hello".getBytes()))
    assert(cache.get(packageContext2, "Foo", 0).isEmpty)
  }

  test("cache with all new fields combined") {
    val packageContext1 = PackageContext(Some("test"), Array("ghost1"), Array("analysed1"), Array("ns1"), Array("/ext1"), false)
    val packageContext2 = PackageContext(Some("test"), Array("ghost1"), Array("analysed1"), Array("ns1"), Array("/ext1"), true)
    val packageContext3 = PackageContext(Some("test"), Array("ghost1"), Array("analysed1"), Array("ns2"), Array("/ext1"), false)
    val packageContext4 = PackageContext(Some("test"), Array("ghost1"), Array("analysed1"), Array("ns1"), Array("/ext2"), false)
    val cache           = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    
    cache.upsert(packageContext1, "Foo", 0, "Hello".getBytes())
    assert(cache.get(packageContext1, "Foo", 0).get.sameElements("Hello".getBytes()))
    
    // Each field change should invalidate cache
    assert(cache.get(packageContext2, "Foo", 0).isEmpty) // isLibrary changed
    assert(cache.get(packageContext3, "Foo", 0).isEmpty) // additionalNamespaces changed  
    assert(cache.get(packageContext4, "Foo", 0).isEmpty) // externalMetadataPaths changed
  }

  test("array ordering consistency for externalMetadataPaths") {
    // Since externalMetadataPaths are sorted in OPM.scala, different orderings should be equivalent
    val packageContext1 = PackageContext(Some("test"), Array(), Array(), Array(), Array("/path1", "/path2"), false)
    val packageContext2 = PackageContext(Some("test"), Array(), Array(), Array(), Array("/path2", "/path1"), false)
    val cache           = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    
    cache.upsert(packageContext1, "Foo", 0, "Hello".getBytes())
    // This test verifies current behavior - since arrays use sameElements, order matters for cache equality
    assert(cache.get(packageContext1, "Foo", 0).get.sameElements("Hello".getBytes()))
    assert(cache.get(packageContext2, "Foo", 0).isEmpty)
  }

  test("array ordering consistency for ghostedPackages") {
    // Since ghostedPackages are sorted in OPM.scala, different orderings should be equivalent  
    val packageContext1 = PackageContext(Some("test"), Array("ghost1", "ghost2"), Array(), Array(), Array(), false)
    val packageContext2 = PackageContext(Some("test"), Array("ghost2", "ghost1"), Array(), Array(), Array(), false)
    val cache           = ParsedCache.create(1).getOrElse(throw new NoSuchElementException())
    
    cache.upsert(packageContext1, "Foo", 0, "Hello".getBytes())
    // This test verifies current behavior - since arrays use sameElements, order matters for cache equality
    assert(cache.get(packageContext1, "Foo", 0).get.sameElements("Hello".getBytes()))
    assert(cache.get(packageContext2, "Foo", 0).isEmpty)
  }
}
