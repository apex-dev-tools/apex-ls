package com.nawforce.runtime.gc

import com.nawforce.common.api.ServerOps
import com.nawforce.common.types.core.TypeDeclaration

object Monitor {
  val map = new SkinnyWeakSet[AnyRef]()
  var duplicateTypes: SkinnyWeakSet[TypeDeclaration] = _

  def push[T <: AnyRef](t: T): Unit = {
    map.add(t)
  }

  def size: Int = map.size

  def reportDuplicateTypes: Unit = {
    val tdsByName = map.toSet.toArray.collect {case td: TypeDeclaration => (td.typeName, td)}
    val typeNames = tdsByName.map(_._1)
    val duplicates = typeNames.toSeq.groupBy(identity).collect { case (t, Seq(_, _, _*)) => t }
    if (duplicates.nonEmpty) {
      duplicates.foreach(typeName => {
        ServerOps.debug(ServerOps.Trace, s"Duplicate types found for $typeName")
      })
      duplicateTypes = new SkinnyWeakSet[TypeDeclaration]();
      duplicates.foreach(dup => tdsByName.filter(_._1 == dup).foreach(x => duplicateTypes.add(x._2)))
    }
  }
}
