/*
 [The "BSD licence"]
 Copyright (c) 2020 Kevin Jones
 All rights reserved.

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

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.nawforce.common.stream

import com.nawforce.common.diagnostics.Issue
import com.nawforce.common.documents._
import com.nawforce.common.names.Name

import scala.collection.compat.immutable.ArraySeq

trait PackageEvent

case class IssuesEvent(issues: ArraySeq[Issue]) extends PackageEvent

object IssuesEvent {
  def apply(issues: Issue*): IssuesEvent = {
    new IssuesEvent(issues.to(ArraySeq))
  }

  def iterator(issues: Seq[Issue]): Iterator[IssuesEvent] = {
    if (issues.nonEmpty)
      Iterator(new IssuesEvent(issues.to(ArraySeq)))
    else
      Iterator.empty
  }
}

class PackageStream(val namespace: Option[Name], val events: ArraySeq[PackageEvent]) {
  def issues: Seq[IssuesEvent] = events.collect { case e: IssuesEvent            => e }
  def labelsFiles: Seq[LabelFileEvent] = events.collect { case e: LabelFileEvent => e }
  def labels: Seq[LabelEvent] = events.collect { case e: LabelEvent              => e }
  def pages: Seq[PageEvent] = events.collect { case e: PageEvent                 => e }
  def flows: Seq[FlowEvent] = events.collect { case e: FlowEvent                 => e }
  def components: Seq[ComponentEvent] = events.collect { case e: ComponentEvent  => e }
}

object PackageStream {
  def apply(namespace: Option[Name], index: DocumentIndex): PackageStream = {
    new PackageStream(namespace, eventStream(index).to(ArraySeq))
  }

  def eventStream(index: DocumentIndex): Iterator[PackageEvent] = {
    LabelGenerator.iterator(LabelNature, index) ++
      PageGenerator.iterator(PageNature, index) ++
      FlowGenerator.iterator(FlowNature, index) ++
      ComponentGenerator.iterator(ComponentNature, index) ++
      SObjectGenerator.iterator(SObjectNature, index) ++
      ApexGenerator.iterator(ClassNature, index) ++
      TriggerGenerator.iterator(TriggerNature, index)
  }
}
