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
import com.nawforce.pkgforce.names.{EncodedName, Name}
import com.nawforce.pkgforce.path.{Location, PathLike}

/** Basic validation of metadata files from just examining the file name. */
class MetadataValidator(logger: IssuesManager, namespace: Option[Name]) {

  def validate(nature: MetadataNature, documents: List[PathLike]): Unit = {
    documents.foreach(logger.pop)
    nature match {
      case ApexNature    => validateApex(documents)
      case TriggerNature => validateTrigger(documents)
      case SObjectNature => validateSObjectLike(documents)
      case _             => ()
    }
  }

  private def validateApex(documents: List[PathLike]): Unit = {
    val allDocs = documents.flatMap(MetadataDocument(_))
    getSingleControllingDocument(ApexNature, allDocs).foreach(controllingDoc => {
      assertSingleMetaDocument(
        controllingDoc,
        ApexMetaNature,
        allDocs.filterNot(_ == controllingDoc)
      )
    })
  }

  private def validateTrigger(documents: List[PathLike]): Unit = {
    val allDocs = documents.flatMap(MetadataDocument(_))
    getSingleControllingDocument(TriggerNature, allDocs).foreach(controllingDoc => {
      assertSingleMetaDocument(
        controllingDoc,
        TriggerMetaNature,
        allDocs.filterNot(_ == controllingDoc)
      )
    })
  }

  private def getSingleControllingDocument(
    nature: MetadataNature,
    documents: List[MetadataDocument]
  ): Option[MetadataDocument] = {
    val controllingDocs = documents.filter(_.nature == nature)
    if (controllingDocs.isEmpty) {
      documents.foreach(document => {
        val typeName = document.typeName(namespace)
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
        val typeName = document.typeName(namespace)
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
    documents: List[MetadataDocument]
  ): Unit = {
    val typeName = controllingDoc.typeName(namespace)
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

  private def validateSObjectLike(documents: List[PathLike]): Unit = {
    val allDocs         = documents.flatMap(MetadataDocument(_))
    val controllingDocs = allDocs.filter(_.nature == SObjectNature)
    val typeName        = allDocs.head.controllingTypeName(namespace)

    val isSObject: Boolean = controllingDocs.headOption
      .map(_.isInstanceOf[SObjectDocument])
      .getOrElse({
        // If we don't have a controlling doc use controlling typename ext to test if an actual SObject
        val encName = EncodedName(typeName.name)
        encName.ext.isEmpty || encName.ext.exists(_.value == "c")
      })

    if (!isSObject && controllingDocs.isEmpty) {
      logger.log(
        Issue(
          documents.head,
          Diagnostic(
            ERROR_CATEGORY,
            Location.empty,
            s"Components of type '$typeName' are defined, but the required object-meta.xml file is missing"
          )
        )
      )
    } else if (controllingDocs.length > 1) {
      controllingDocs.foreach(controllingDoc => {
        val typeName   = controllingDoc.typeName(namespace)
        val otherPaths = controllingDocs.filterNot(_ == controllingDoc).map(_.path).mkString(", ")
        logger.log(
          Issue(
            controllingDoc.path,
            Diagnostic(
              ERROR_CATEGORY,
              Location.empty,
              s"Type '$typeName' is defined, but duplicate object-meta.xml files found at $otherPaths"
            )
          )
        )
      })
    }

    assertUniqueName(FieldNature, allDocs)
    assertUniqueName(FieldSetNature, allDocs)
    assertUniqueName(SharingReasonNature, allDocs)
  }

  private def assertUniqueName(nature: MetadataNature, documents: List[MetadataDocument]): Unit = {
    documents
      .filter(_.nature == nature)
      .groupBy(_.name)
      .filter(_._2.size > 1)
      .foreach(duplicate => {
        duplicate._2.foreach(duplicated => {
          val typeName = duplicated.typeName(namespace)
          val otherPaths =
            duplicate._2.filterNot(_ == duplicated).map(_.path).mkString(", ")
          logger.log(
            Issue(
              duplicated.path,
              Diagnostic(
                ERROR_CATEGORY,
                Location.empty,
                s"Type '${typeName.toString}' is defined, but duplicate metadata found at $otherPaths"
              )
            )
          )
        })
      })
  }
}
