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

  /** Given some TypeIds, find all referencing Apex tests. If you pass an inner type, it's outer type is used to
    * locate references. For classes we include references of references when looking for test classes, but for
    * interfaces only tests directly referencing the interface are returned to avoid the scope expanding into
    * unrelated implementors of those interfaces. This works in practice because any significant change to an
    * interface would require changes to all implementors for the code to be deployable, so tests for those
    * implementation classes will be found independently. Tests included in the input will be contained in the
    * results.
    *
    * This can produce over general results where inner interfaces are used. This is caused by not examining the type
    * of each dependency, which means a class dependency is used where a more limited interface dependency could have
    * been inferred. See test cases in TestClassesTest for an example of this.
    */
  def testReferences(sourceIds: Set[TypeId]): Set[ApexDeclaration] = {
    val results = mutable.Set[ApexDeclaration]()

    val refsSeen = mutable.Set[TypeId]()
    refsSeen.addAll(sourceIds)

    val searchQueue = mutable.Queue[SearchInfo]()
    searchQueue.enqueueAll(sourceIds.map(id => SearchInfo(id, limitDepth = false)))

    while (searchQueue.nonEmpty) {
      val searchInfo = searchQueue.dequeue()
      val td = searchInfo.typeId.module
        .findPackageType(searchInfo.typeId.typeName, None)
        .map(_.outermostTypeDeclaration)
      val nextLimitDepth = td.exists(_.nature == INTERFACE_NATURE)

      td match {
        case Some(td: ApexDeclaration) =>
          // Report on any test, may include starting typeIds
          if (td.modifiers.contains(ISTEST_ANNOTATION))
            results.add(td)

          // Don't expand the search once in depth limiting mode
          if (!searchInfo.limitDepth) {
            // Queue holders for processing
            val newRefs = td.getTypeDependencyHolders.toIterable.filterNot(refsSeen.contains)
            searchQueue.addAll(newRefs.map(SearchInfo(_, nextLimitDepth)))
            refsSeen.addAll(newRefs)
          }
        case _ => ()
      }
    }
    results.toSet
  }

  private case class SearchInfo(typeId: TypeId, limitDepth: Boolean)

}
