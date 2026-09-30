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

package io.github.apexdevtools.apexls

import com.nawforce.pkgforce.names.TypeIdentifier

/** Encoding of type identifiers shared by the batch dependency commands. */
private[apexls] object BatchTypeIdentifiers {

  def name(identifier: TypeIdentifier): String = identifier.typeName.toString

  def namespace(identifier: TypeIdentifier): ujson.Value = {
    identifier.namespace.map(ns => ujson.Str(ns.value)).getOrElse(ujson.Null)
  }

  def toJson(identifier: TypeIdentifier): ujson.Obj = {
    ujson.Obj("name" -> name(identifier), "namespace" -> namespace(identifier))
  }
}
