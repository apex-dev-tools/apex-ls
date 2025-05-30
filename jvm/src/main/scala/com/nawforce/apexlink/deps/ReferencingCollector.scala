/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved
 */
package com.nawforce.apexlink.deps

import com.nawforce.apexlink.types.apex.ApexDeclaration
import com.nawforce.pkgforce.modifiers.ISTEST_ANNOTATION
import com.nawforce.pkgforce.parsers.INTERFACE_NATURE

import scala.collection.mutable

object ReferencingCollector {

  case class NodeInfo(td: ApexDeclaration, primary: Boolean)
  case class TestInfo(testClass: ApexDeclaration, explain: List[ApexDeclaration]) {
    def asTypeNameStrings(): (String, Array[String]) =
      (testClass.typeName.toString, explain.map(_.typeName.toString).toArray)
  }

  /** Given some Types, find all referencing Apex tests. To find all relevant tests the input should
    * include related types such as superclasses, interfaces and nested types with the primary flag
    * set to false. Test classes included in the input will be contained in the results.
    */
  def testReferences(sourceTds: Set[NodeInfo]): Set[TestInfo] = {
    val results = mutable.Map[String, TestInfo]()
    visitApexReferences(sourceTds, (primary, path) => testClassVisit(results, primary, path))
    results.values.toSet
  }

  /** Visitor for locating @isTest classes */
  private def testClassVisit(
    accum: mutable.Map[String, TestInfo],
    primary: Boolean,
    path: List[ApexDeclaration]
  ): Boolean = {
    // Terminate search if we already have this test class
    val typeName = path.head.typeName.toString
    if (accum.contains(typeName))
      return false

    // Always save test classes, we don't bail out here as we want to capture test->test dependencies
    if (path.head.modifiers.contains(ISTEST_ANNOTATION))
      accum.put(typeName, TestInfo(path.head, path))

    if (!primary) {
      // For non-primary searches we need to be sure we have a use relationship between holder and dependent.
      // This is to stop the search extending into unrelated subtrees.
      path.tail.isEmpty || doesUse(path.head, path.tail.head)
    } else {
      true
    }
  }

  /** Determine if the holder is using the dependent as apposed to extending/implementing it. */
  private def doesUse(holder: ApexDeclaration, dependent: ApexDeclaration): Boolean = {
    // Quick exclude of interfaces
    if (dependent.nature == INTERFACE_NATURE)
      return false

    // Check if extends in some way, ideally we would use a positive test here but our dependency info can include
    // transitives making that unsound, instead we reverse and detect extends but in a very specific way
    if (doesExtend(holder, dependent))
      return false

    true
  }

  /** Determine if the holder has some form of extends relationship with the dependent. We include
    * in this checking outer classes and super classes for extension because the holder information
    * can pick up these kinds of relationships as well. NOTE: If we make improvements that more
    * cleanly differentiate between direct and transitives dependencies then the scope of this can
    * probably be reduced.
    */
  private def doesExtend(holder: ApexDeclaration, dependent: ApexDeclaration): Boolean = {
    // Direct superclass
    holder.superClassDeclaration.contains(dependent) ||
    // Indirect superclass
    holder.superClassDeclaration
      .collect { case ad: ApexDeclaration => ad }
      .exists(parent => doesExtend(parent, dependent)) ||
    // Outer class extends
    holder.outerTypeDeclaration
      .collect { case ad: ApexDeclaration => ad }
      .exists(outer => doesExtend(outer, dependent))
  }

  /** Visit references to the passed type declarations. The visit function is called on all unique
    * declarations discovered, the current path being passed as a list where the head is the current
    * node and last is the origin. Spidering continues while the function returns true. If you
    * return false the current path is not expanded further, but it may be revisited on other
    * unexplored branches.
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

  /** Get Apex holders for an ApexDeclaration. This avoids using the 'Type' dependencies so that it
    * can be used on inner classes.
    */
  private def getDependencyHolders(td: ApexDeclaration): Set[ApexDeclaration] = {
    td.getDependencyHolders
      .flatMap(_.thisTypeIdOpt)
      .flatMap(typeId =>
        typeId.module
          .findType(typeId.typeName)
          .toOption
          .collect { case td: ApexDeclaration => td }
      )
  }
}
