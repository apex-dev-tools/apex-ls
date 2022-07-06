/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved
 */
package com.nawforce.apexlink.deps

import com.nawforce.apexlink.types.apex.ApexDeclaration
import com.nawforce.apexlink.types.core.TypeId
import com.nawforce.pkgforce.modifiers.ISTEST_ANNOTATION
import com.nawforce.pkgforce.parsers.INTERFACE_NATURE

import scala.collection.mutable

object ReferencingCollector {

  /** Given some Types, find all referencing Apex tests. For classes we include references of references when
    * looking for test classes, but for interfaces only tests directly referencing the interface are returned to
    * avoid the scope expanding into unrelated implementors of those interfaces. This works in practice because
    * any significant change to an interface would require changes to all implementors for the code to be deployable,
    * so tests for those implementation classes will be found independently. Tests included in the input will be
    * contained in the results.
    */
  def testReferences(sourceTds: Set[ApexDeclaration]): Set[ApexDeclaration] = {
    val results = mutable.ArrayBuffer[ApexDeclaration]()
    visitApexReferences(sourceTds, path => testClassVisit(results, path))
    results.toSet
  }

  /** Visitor for locating @isTest classes */
  private def testClassVisit(
    accum: mutable.ArrayBuffer[ApexDeclaration],
    path: List[ApexDeclaration]
  ): Boolean = {
    // Always save test classes
    if (path.head.modifiers.contains(ISTEST_ANNOTATION))
      accum.append(path.head)

    // Don't spider beyond one level past an interface
    !path.tail.headOption.exists(_.nature == INTERFACE_NATURE)
  }

  /** Visit references to the passed type declarations. The visit function is called on all unique paths discovered,
    * the current path being passed as a list where the head is the current node and last is the origin. Spidering
    * continues while the function returns true. If you return false the current path is not expanded further but
    * other unexplored branches will continue to be processed.
    */
  def visitApexReferences(
    sourceTds: Set[ApexDeclaration],
    visitPath: List[ApexDeclaration] => Boolean
  ): Unit = {
    val searchQueue = mutable.Queue[List[ApexDeclaration]]()
    searchQueue.enqueueAll(sourceTds.map(id => List(id)))

    while (searchQueue.nonEmpty) {
      val path = searchQueue.dequeue()
      if (visitPath(path)) {
        getDependencyHolders(path.head)
          .foreach(holder => searchQueue.enqueue(holder :: path))
      }
    }
  }

  /** Get Apex holders for an ApexDeclaration. This avoids using the 'Type' dependencies so that it can be used
    * on inner classes.
    */
  private def getDependencyHolders(td: ApexDeclaration): Set[ApexDeclaration] = {
    toApexDeclarations(
      td.getDependencyHolders
        .flatMap(_.thisTypeIdOpt)
    )
  }

  /** Convert TypeIds to ApexDeclarations, ignoring any which can't be found. */
  private def toApexDeclarations(typeIds: Set[TypeId]): Set[ApexDeclaration] = {
    typeIds.flatMap(_.toApexDeclaration)
  }

  /** Helper to lookup a TypeId */
  implicit class TypeIdOps(typeId: TypeId) {
    def toApexDeclaration: Option[ApexDeclaration] = {
      typeId.module
        .findPackageType(typeId.typeName, None)
        .collect { case td: ApexDeclaration => td }
    }
  }
}
