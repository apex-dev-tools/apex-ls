/*
 [The "BSD licence"]
 Copyright (c) 2019 Kevin Jones
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

package com.nawforce.common.pkg

import java.nio.charset.StandardCharsets

import com.nawforce.common.api.{ApexSummary, ServerOps}
import com.nawforce.common.diagnostics.{Issue, MISSING_CATEGORY}
import com.nawforce.common.documents._
import com.nawforce.common.names.{Name, TypeName}
import com.nawforce.common.types.apex.{FullDeclaration, SummaryApex, TriggerDeclaration}
import com.nawforce.common.types.schema.SObjectDeclaration
import upickle.default.writeBinary

trait PackageDeploy {
  this: PackageImpl =>

  private val epoch = System.currentTimeMillis()

  def deployWorkspace(): Unit = {
    val startingTypes = types.size

    loadCustomObjects()
    loadComponents()
    loadClasses()
    loadTriggers()

    if (types.size > startingTypes) {
      val total = System.currentTimeMillis() - epoch
      val avg = total / types.size
      ServerOps.debug(ServerOps.Trace, s"Package(${namespace.map(_.value).getOrElse("")}) loaded ${types.size}" +
        s" types in ${total / 1000} seconds, average $avg ms/type")
    }
  }

  private def loadCustomObjects(): Unit = {
    val docs = documentsByExtension(Name("object"))
    ServerOps.debugTime(s"Parsed ${docs.size} objects", docs.nonEmpty) {
      val tds = docs.flatMap {
        case docType: SObjectDocument =>
          SObjectDeclaration.create(this, docType.path)
        case docType: PlatformEventDocument =>
          SObjectDeclaration.create(this, docType.path)
        case docType: CustomMetadataDocument =>
          SObjectDeclaration.create(this, docType.path)
        case _ => assert(false); Seq()
      }
      tds.foreach(upsertMetadata(_))
      tds.foreach(_.validate())
      schema().relatedLists.validate()
    }
  }

  private def loadComponents(): Unit = {
    val docs = documentsByExtension(Name("component"))
    ServerOps.debugTime(s"Parsed ${docs.size} components", docs.nonEmpty) {
      docs.foreach {
        case docType: ComponentDocument => upsertComponent(namespace, docType)
        case _ => assert(false); Seq()
      }
    }
  }

  private def loadClasses(): Unit = {
    val pcOpt = getParsedCache
    val docs = documentsByExtension(Name("cls"))
    ServerOps.debugTime(s"Parsed ${docs.size} classes", docs.nonEmpty) {

      // Load summary docs that have valid dependents
      if (pcOpt.nonEmpty) {
        val summaryDocs = docs.flatMap(doc => {
          val data = doc.path.read()
          val value = pcOpt.flatMap(_.get(data.right.get.getBytes(), packageContext))
          value.map(v => new SummaryApex(doc.path, this, v))
        })
        val summaryDocsByType = summaryDocs.map(d => (d.declaration.typeName, d.declaration)).toMap

        // Upsert any summary docs that are valid and report known issues
        val validSummaryDocs = summaryDocs
            .filterNot(_.diagnostics.exists(_.category == MISSING_CATEGORY.value))
            .filter(_.declaration.areTypeDependenciesValid(summaryDocsByType))
        validSummaryDocs.foreach(doc => {
          upsertMetadata(doc.declaration)
          val path = doc.declaration.path.toString
          doc.diagnostics.map(diagnostic => org.issues.add(Issue.fromDiagnostic(path, diagnostic)))
        })

        // Validate these (must be after all have been inserted to allow for dependency propagation)
        validSummaryDocs.foreach(_.declaration.validate())
      }

      // Load full docs for rest of set
      val fullTypes = docs
        .filterNot(doc => types.contains(TypeName(doc.name).withNamespace(namespace)))
        .flatMap(doc => {
          val data = doc.path.read()
          val tdOpt = ServerOps.debugTime(s"Parsed ${doc.path}") {
            FullDeclaration.create(this, doc.path, data.right.get)
          }
          tdOpt.map(td => {
            upsertMetadata(td)
            (td, data.right.get)
          })
        })

      // Validate the full types & write back to cache
      fullTypes.foreach(loadedWithSource => {
        val td = loadedWithSource._1
        td.validate()
        pcOpt.map(pc => {
          val diagnostics = org.issues.getDiagnostics(td.getPath.toString)
          val summary = ApexSummary(td.summary, diagnostics)
          pc.upsert(loadedWithSource._2.getBytes(StandardCharsets.UTF_8),
            writeBinary(summary), packageContext)
        })
      })
    }
  }

  private def getParsedCache: Option[ParsedCache] = {
    if (ServerOps.isParsedDataCaching)
      ParsedCache.create() match {
        case Left(err) => ServerOps.error(err); None
        case Right(cache) => Some(cache)
      }
    else
      None
  }

  private def loadTriggers(): Unit = {
    val docs = documentsByExtension(Name("trigger"))
    ServerOps.debugTime(s"Parsed ${docs.size} triggers", docs.nonEmpty) {
      val tds = docs.flatMap {
        case docType: ApexTriggerDocument =>
          val data = docType.path.read()
          TriggerDeclaration.create(this, docType.path, data.right.get)
        case _ => assert(false); Seq()
      }
      tds.foreach(upsertMetadata(_))
      tds.foreach(_.validate())
    }
  }
}