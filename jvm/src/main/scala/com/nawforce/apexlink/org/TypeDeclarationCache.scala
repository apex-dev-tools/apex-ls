/*
 * Copyright (c) 2024 Certinia Inc. All rights reserved.
 */

package com.nawforce.apexlink.org

import com.nawforce.apexlink.types.core.TypeDeclaration
import com.nawforce.pkgforce.names.TypeName

import scala.collection.mutable

/** Cache of TypeDeclarations against their TypeName. Provided to speed up Schema namespace
  * searched by avoiding the need to construct a new TypeName for each search.
  */
class TypeDeclarationCache {
  private val allTypes    = mutable.Map[TypeName, TypeDeclaration]()
  private val schemaTypes = mutable.Map[TypeName, TypeDeclaration]()

  def size: Int = {
    allTypes.size
  }

  def put(td: TypeDeclaration, altTypeName: Option[TypeName] = None): Unit = {
    put(altTypeName.getOrElse(td.typeName), td)
  }

  /** Upsert an entry. Beware, assumes the TypeName is fully qualified. */
  def put(typeName: TypeName, td: TypeDeclaration): Unit = {
    allTypes.put(typeName, td)
    if (typeName.outer.contains(TypeName.Schema)) {
      schemaTypes.put(typeName.withOuter(None), td)
    }
  }

  def get(typeName: TypeName): Option[TypeDeclaration] = {
    allTypes.get(typeName)
  }

  def getUnsafe(typeName: TypeName): TypeDeclaration = {
    allTypes(typeName)
  }

  def getWithSchema(typeName: TypeName): Option[TypeDeclaration] = {
    schemaTypes.get(typeName)
  }

  def contains(typeName: TypeName): Boolean = {
    allTypes.contains(typeName)
  }

  def values(): Iterable[TypeDeclaration] = {
    allTypes.values
  }

  def filter(
    pred: ((TypeName, TypeDeclaration)) => Boolean
  ): mutable.Map[TypeName, TypeDeclaration] = {
    allTypes.filter(pred)
  }

  def collect[T](pf: PartialFunction[(TypeName, TypeDeclaration), T]): mutable.Iterable[T] = {
    allTypes.collect(pf)
  }

  def remove(typeName: TypeName): Option[TypeDeclaration] = {
    val result = allTypes.remove(typeName)
    if (typeName.outer.contains(TypeName.Schema)) {
      schemaTypes.remove(typeName.withOuter(None))
    }
    result
  }
}
