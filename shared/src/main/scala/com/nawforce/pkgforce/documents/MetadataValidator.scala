/*
 Copyright (c) 2022 Kevin Jones, All rights reserved.
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
package com.nawforce.pkgforce.documents

import com.nawforce.pkgforce.diagnostics.{Diagnostic, ERROR_CATEGORY, Issue, IssuesManager}
import com.nawforce.pkgforce.path.{Location, PathLike}

/** Basic validation of metadata files from just examining the file name. */
class MetadataValidator(logger: IssuesManager) {

  def validate(nature: MetadataNature, typeName: String, documents: List[PathLike]): Unit = {
    nature match {
      case ApexNature    => validateApex(typeName, documents)
      case TriggerNature => validateTrigger(typeName, documents)
      case _             => ()
    }
  }

  private def validateApex(typeName: String, documents: List[PathLike]): Unit = {
    val allDocs = documents.flatMap(MetadataDocument(_))
    getSingleControllingDocument(ApexNature, typeName, allDocs).foreach(controllingDoc => {
      assertSingleMetaDocument(
        controllingDoc,
        ApexMetaNature,
        typeName,
        allDocs.filterNot(_ == controllingDoc)
      )
    })
  }

  private def validateTrigger(typeName: String, documents: List[PathLike]): Unit = {
    val allDocs = documents.flatMap(MetadataDocument(_))
    getSingleControllingDocument(TriggerNature, typeName, allDocs).foreach(controllingDoc => {
      assertSingleMetaDocument(
        controllingDoc,
        TriggerMetaNature,
        typeName,
        allDocs.filterNot(_ == controllingDoc)
      )
    })
  }

  private def getSingleControllingDocument(
    nature: MetadataNature,
    typeName: String,
    documents: List[MetadataDocument]
  ): Option[MetadataDocument] = {
    val controllingDocs = documents.filter(_.nature == nature)
    if (controllingDocs.isEmpty) {
      documents.foreach(document => {
        logger.log(
          Issue(
            document.path,
            Diagnostic(
              ERROR_CATEGORY,
              Location.empty,
              s"Type '$typeName' is not defined, but expected due to '${document.path}', ignoring this file"
            )
          )
        )
      })
      None
    } else if (controllingDocs.size > 1) {
      controllingDocs.tail.foreach(document => {
        val otherPaths =
          controllingDocs.filterNot(_.path == document.path).map(_.path).mkString(", ")
        logger.log(
          Issue(
            document.path,
            Diagnostic(
              ERROR_CATEGORY,
              Location.empty,
              s"Duplicate for type '$typeName' found in '${document.path}', ignoring this file, see also $otherPaths"
            )
          )
        )
      })
      None
    } else {
      controllingDocs.headOption
    }
  }

  private def assertSingleMetaDocument(
    controllingDoc: MetadataDocument,
    nature: MetadataNature,
    typeName: String,
    documents: List[MetadataDocument]
  ): Unit = {
    val metaDocs = documents.filter(_.nature == nature)
    if (metaDocs.isEmpty) {
      logger.log(
        Issue(
          controllingDoc.path,
          Diagnostic(
            ERROR_CATEGORY,
            Location.empty,
            s"Type '$typeName' is defined, but meta file is missing for '${controllingDoc.path}'"
          )
        )
      )
    } else if (metaDocs.size > 1) {
      val otherPaths = documents.map(_.path).mkString(", ")
      logger.log(
        Issue(
          controllingDoc.path,
          Diagnostic(
            ERROR_CATEGORY,
            Location.empty,
            s"Type '$typeName' is defined, but multiple meta files found at $otherPaths"
          )
        )
      )
    } else if (controllingDoc.path.parent != metaDocs.head.path.parent) {
      logger.log(
        Issue(
          controllingDoc.path,
          Diagnostic(
            ERROR_CATEGORY,
            Location.empty,
            s"Type '$typeName' is defined, but its meta file is in a different directory see ${metaDocs.head.path}"
          )
        )
      )
    }
  }
}