/*
 Copyright (c) 2021 Kevin Jones, All rights reserved.
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

package com.nawforce.pkgforce.stream

import com.nawforce.pkgforce.documents._
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.platform.Path

final case class ApexEvent(path: PathLike) extends PackageEvent

/** Convert Apex documents into PackageEvents */
object ApexGenerator {

  def iterator(index: DocumentIndex): Iterator[PackageEvent] = {
    val docs = index.get(ApexNature)
    docs.iterator.flatMap(md => {
      val docs     = md._2.flatMap(path => MetadataDocument(path))
      val metaDocs = docs.collect { case d: ApexClassMetaDocument => d }
      val isActive = metaDocs.isEmpty || metaDocs.exists(_.isActive)
      if (isActive) {
        docs.find(_.nature == ApexNature).map(doc => ApexEvent(doc.path))
      } else {
        Iterator.empty
      }
    })
  }
}
