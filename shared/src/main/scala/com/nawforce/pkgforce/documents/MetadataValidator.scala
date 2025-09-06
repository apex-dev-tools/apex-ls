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

import com.nawforce.pkgforce.diagnostics.{Diagnostic, ERROR_CATEGORY, Issue, IssueLogger, Logger}
import com.nawforce.pkgforce.names.{EncodedName, Name}
import com.nawforce.pkgforce.path.{Location, PathLike}

/** Basic validation of metadata files from just examining the file name. */
class MetadataValidator(logger: IssueLogger, namespace: Option[Name], isGulped: Boolean) {

  def validate(nature: MetadataNature, documents: List[PathLike]): Unit = {
    // Clear any previous issues, this is start of a re-validation
    // WARNING: This is pretty bad for the refresh logic as it forces a revalidation
    // of types that have not changed, we need a better design for this.
    documents.foreach(logger.pop)

    // Not all of these will be validated, using a full list to get missing case warning
    nature match {
      case LabelNature         => ()
      case ApexNature          => validateApex(documents)
      case ApexMetaNature      => ()
      case TriggerNature       => validateTrigger(documents)
      case TriggerMetaNature   => ()
      case ComponentNature     => validateComponent(documents)
      case PageNature          => validatePage(documents)
      case FlowNature          => validateFlow(documents)
      case SObjectNature       => validateSObjectLike(documents)
      case FieldNature         => validateCaseSensitivity(documents, "fields")
      case FieldSetNature      => validateCaseSensitivity(documents, "fieldSets")
      case SharingReasonNature => validateCaseSensitivity(documents, "sharingReasons")
    }
  }

  private def validateApex(documents: List[PathLike]): Unit = {
    val allDocs = documents.flatMap(MetadataDocument(_))
    getSingleControllingDocument(ApexNature, ApexMetaNature, allDocs).foreach(controllingDoc => {
      assertSingleMetaDocument(
        controllingDoc,
        ApexMetaNature,
        allDocs.filterNot(_ == controllingDoc)
      )
    })
  }

  private def validateTrigger(documents: List[PathLike]): Unit = {
    val allDocs = documents.flatMap(MetadataDocument(_))
    getSingleControllingDocument(TriggerNature, TriggerMetaNature, allDocs).foreach(
      controllingDoc => {
        assertSingleMetaDocument(
          controllingDoc,
          TriggerMetaNature,
          allDocs.filterNot(_ == controllingDoc)
        )
      }
    )
  }

  private def getSingleControllingDocument(
    controllingNature: MetadataNature,
    metaNature: MetadataNature,
    documents: List[MetadataDocument]
  ): Option[MetadataDocument] = {
    val controllingDocs = documents.filter(_.nature == controllingNature)
    if (controllingDocs.isEmpty) {
      // Bypass error if controlling might exist but is ignored
      val metaDocs = documents.filter(_.nature == metaNature)
      val isControllingIgnored = metaDocs.size == 1 && metaDocs.headOption.exists(md => {
        val controllingPath = md.path.parent.join(md.path.basename.replaceFirst("-meta.xml$", ""))
        controllingPath != md.path && controllingPath.isFile
      })
      if (!isControllingIgnored) {
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
      }
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

    // Gulped metadata does not currently generate meta files for cls/trigger
    if (!isGulped && metaDocs.isEmpty) {
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
    } else if (metaDocs.nonEmpty && controllingDoc.path.parent != metaDocs.head.path.parent) {
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

  private def validateComponent(documents: List[PathLike]): Unit = {
    assertSingleDocument(documents)
  }

  private def validatePage(documents: List[PathLike]): Unit = {
    assertSingleDocument(documents)
  }

  private def validateFlow(documents: List[PathLike]): Unit = {
    assertSingleDocument(documents)
  }

  private def assertSingleDocument(documents: List[PathLike]): Unit = {
    if (documents.length > 1) {
      val allDocs = documents.flatMap(MetadataDocument(_))
      allDocs.foreach(document => {
        val otherPaths = documents.filterNot(_ == document.path).mkString(", ")
        logger.log(
          Issue(
            document.path,
            Diagnostic(
              ERROR_CATEGORY,
              Location.empty,
              s"Duplicate for type '${document.typeName(namespace)}', see also $otherPaths"
            )
          )
        )
      })
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
          documents.minBy(_.toString), // order not guaranteed, first alphabetically
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

  /** Validate case sensitivity for metadata directory names */
  private def validateCaseSensitivity(
    documents: List[PathLike],
    expectedDirectoryName: String
  ): Unit = {
    documents.foreach { document =>
      // Find the parent directory that should match expectedDirectoryName
      val parentDir = document.parent.basename

      if (parentDir.equalsIgnoreCase(expectedDirectoryName) && parentDir != expectedDirectoryName) {
        logger.log(
          Issue(
            document,
            Diagnostic(
              ERROR_CATEGORY,
              Location.empty,
              s"Directory name case mismatch: found '$parentDir' but should be '$expectedDirectoryName'. " +
                s"SFDX metadata directories should follow the correct case convention for consistency and cross-platform compatibility."
            )
          )
        )
      }
    }
  }
}
