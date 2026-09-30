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

import com.nawforce.apexlink.rpc.DependencyNode
import com.nawforce.pkgforce.names.TypeIdentifier

private[apexls] object DependencyReportCommand extends BatchCommand {
  override type Result = Array[DependencyNode]

  override val name: String               = "dependency-report"
  override val requiresWorkspace: Boolean = true

  override def execute(
    context: BatchContext,
    args: Seq[String]
  ): Either[BatchError, Array[DependencyNode]] = {
    val org = context.org.get
    val ids = org.getTypeIdentifiers(apexOnly = true)
    Right(org.getDependencyGraph(ids, depth = 1, apexOnly = true, ignoring = Array.empty).nodeData)
  }

  override def writeResult(result: Array[DependencyNode]): ujson.Value = {
    val nodes = result.sortBy(node => BatchTypeIdentifiers.name(node.identifier)).map { node =>
      ujson.Obj(
        "name"                    -> BatchTypeIdentifiers.name(node.identifier),
        "namespace"               -> BatchTypeIdentifiers.namespace(node.identifier),
        "nature"                  -> node.nature,
        "size"                    -> ujson.Num(node.size.toDouble),
        "transitiveCount"         -> node.transitiveCount,
        "maxDependencyCount"      -> optionalNumber(node.maxDependencyCount),
        "isEntryPoint"            -> node.isEntryPoint,
        "extending"               -> identifiers(node.extending),
        "implementing"            -> identifiers(node.implementing),
        "using"                   -> identifiers(node.using),
        "extendingIdentifiers"    -> identifierObjects(node.extending),
        "implementingIdentifiers" -> identifierObjects(node.implementing),
        "usingIdentifiers"        -> identifierObjects(node.using)
      )
    }
    ujson.Obj("nodes" -> ujson.Arr(nodes.toIndexedSeq: _*))
  }

  private def optionalNumber(value: Option[Int]): ujson.Value = {
    value.map(ujson.Num(_)).getOrElse(ujson.Null)
  }

  private def sorted(values: Array[TypeIdentifier]): Array[TypeIdentifier] = {
    values.sortBy(BatchTypeIdentifiers.name)
  }

  private def identifiers(values: Array[TypeIdentifier]): ujson.Arr = {
    ujson.Arr(sorted(values).map(id => ujson.Str(BatchTypeIdentifiers.name(id))).toIndexedSeq: _*)
  }

  /** In the same order as [[identifiers]], so the two arrays correspond by index */
  private def identifierObjects(values: Array[TypeIdentifier]): ujson.Arr = {
    ujson.Arr(sorted(values).map(BatchTypeIdentifiers.toJson).toIndexedSeq: _*)
  }
}
