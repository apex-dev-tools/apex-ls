/*
 * Copyright (c) 2024 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.org

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.pkgforce.names.TypeNameFuncs.TypeNameFuncs
import com.nawforce.pkgforce.names.{Name, TypeName}
import org.scalatest.Inspectors.forAll
import org.scalatest.funsuite.AnyFunSuite

class TypeDeclarationCacheTest extends AnyFunSuite with TestHelper {

  private val simpleTypeName = TypeName(Name("Simple"))
  private val scopedTypeName = TypeName(Name("Scoped"), Nil, Some(TypeName(Name("Outer"))))
  private val systemTypeName = TypeName(Name("String"), Nil, Some(TypeName(Name("System"))))
  private val genericTypeName =
    TypeName(Name("List"), systemTypeName :: Nil, Some(TypeName(Name("System"))))
  private val schemaTypeName =
    TypeName(Name("Account"), Nil, Some(TypeNames.Schema))
  private val nestedSchemaTypeName =
    TypeName(Name("Fake"), Nil, Some(TypeName(Name("Account"), Nil, Some(TypeNames.Schema))))

  test("Empty cache has no size") {
    assert(new TypeDeclarationCache().size == 0)
  }

  forAll(
    List(
      /*
      simpleTypeName,
      scopedTypeName,
      systemTypeName,
      genericTypeName,
      schemaTypeName,
       */
      nestedSchemaTypeName
    )
  ) { testTypeName =>
    test(s"$testTypeName name can be added and removed") {
      val cache = new TypeDeclarationCache()
      cache.put(testTypeName, null)

      assert(cache.size == 1)
      assert(cache.contains(testTypeName))
      assert(cache.get(testTypeName).contains(null))
      assert(cache.getUnsafe(testTypeName) == null)
      assert(cache.getWithSchema(testTypeName).isEmpty)
      val stripped = testTypeName.replaceTail(TypeName.Schema, None)
      if (stripped != testTypeName)
        assert(cache.getWithSchema(stripped).nonEmpty)
      assert(cache.values().toList == List(null))
      assert(cache.filter(_._1 == testTypeName).nonEmpty)
      assert(cache.collect { case (t: TypeName, null) if t == testTypeName => }.nonEmpty)

      assert(cache.remove(testTypeName).contains(null))
      assert(cache.size == 0)
    }
  }

}
