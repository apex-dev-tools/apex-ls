/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved
 */
package com.nawforce.apexlink.deps

import com.nawforce.apexlink.types.apex.ApexDeclaration
import com.nawforce.apexlink.types.core.{TypeDeclaration, TypeId}
import com.nawforce.pkgforce.modifiers.ISTEST_ANNOTATION
import com.nawforce.pkgforce.parsers.INTERFACE_NATURE

import scala.collection.mutable
import scala.reflect.ClassTag

object ReferencingCollector {

  case class NodeInfo(td: ApexDeclaration, primary: Boolean)

  /** Given some Types, find all referencing Apex tests. To find all relevant tests the input should include related
    *  types such as superclasses, interfaces and nexted types with the primary flag set to false. Test classes included
    *  in the input will be contained in the results.
    */
  def testReferences(sourceTds: Set[NodeInfo]): Set[ApexDeclaration] = {
    val results = mutable.Set[ApexDeclaration]()
    visitApexReferences(sourceTds, (primary, path) => testClassVisit(results, primary, path))
    results.toSet
  }

  /** Visitor for locating @isTest classes */
  private def testClassVisit(
    accum: mutable.Set[ApexDeclaration],
    primary: Boolean,
    path: List[ApexDeclaration]
  ): Boolean = {
    // Terminate search if we already have this test class
    if (accum.contains(path.head))
      return false

    // Always save test classes, we don't bail out here as we want to capture test->test dependencies
    if (path.head.modifiers.contains(ISTEST_ANNOTATION))
      accum.add(path.head)

    if (!primary) {
      // For non-primary searches we don't want to spider from interfaces beyond the first layer as we can end up
      // in code unrelated to the primary changes. We are just looking for tests of the interface.
      !path.tail.exists(_.nature == INTERFACE_NATURE)
    } else {
      true
    }
  }

  /** Visit references to the passed type declarations. The visit function is called on all unique declarations
    * discovered, the current path being passed as a list where the head is the current node and last is the origin.
    * Spidering continues while the function returns true. If you return false the current path is not expanded
    * further, but it may be revisited on other unexplored branches.
    */
  def visitApexReferences(
    sourceTds: Set[NodeInfo],
    visitPath: (Boolean, List[ApexDeclaration]) => Boolean
  ): Unit = {
    val visited     = mutable.Set[ApexDeclaration]()
    val searchQueue = mutable.Queue[(Boolean, List[ApexDeclaration])]()
    searchQueue.enqueueAll(sourceTds.map(id => (id.primary, List(id.td))))

    while (searchQueue.nonEmpty) {
      val path = searchQueue.dequeue()
      if (visitPath(path._1, path._2)) {
        visited.add(path._2.head)
        getDependencyHolders(path._2.head)
          .diff(visited)
          .foreach(holder => searchQueue.enqueue((path._1, holder :: path._2)))
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
    typeIds.flatMap(_.toTypeDeclaration[ApexDeclaration])
  }

  /** Helper to lookup a TypeId */
  implicit class TypeIdOps(typeId: TypeId) {
    def toTypeDeclaration[T <: TypeDeclaration: ClassTag]: Option[T] = {
      typeId.module
        .findPackageType(typeId.typeName, None)
        .collect { case td: T => td }
    }
  }
}
