/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
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

import com.nawforce.pkgforce.diagnostics._
import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.pkgforce.path.{Location, PathLike}
import com.nawforce.pkgforce.sfdx.ForceIgnore
import com.nawforce.runtime.platform.Path

import scala.collection.mutable

/** Metadata document index, maintains information on available metadata within a project/package.
  *
  * Duplicate detection is based on the relevant MetadataDocumentType(s) being able to generate an
  * accurate TypeName for the metadata. Where a document defined a unique type duplicates are
  * reported as errors and then ignored.
  *
  * During an upsert/deletion of new types the index will also need to be updated so that it
  * maintains an accurate view of the metadata files being used.
  */

/** Basic mutable store of documents partitioned by nature & type. Documents are grouped under a
  * controlling typeName to make it easier to validate them later, so cls-meta.xml files are grouped
  * with .cls and fields, fieldSets & sharing reasons are grouped with object-meta.xml files.
  */
class DocumentIndex(
  logger: IssueLogger,
  val path: PathLike,
  namespace: Option[Name],
  ignore: Option[ForceIgnore]
) extends DocumentCollector {

  /** Store Nature->Type name (lowercase)->Path string */
  private val documents =
    new mutable.HashMap[MetadataNature, mutable.HashMap[String, List[PathLike]]]()

  // Run scanner and then post-validate
  DocumentScanner.index(path, logger, ignore, this)

  def size: Int = documents.values.map(_.values.size).sum

  /** Get all documents for specific type of metadata */
  def get(nature: MetadataNature): mutable.Map[String, List[PathLike]] = {
    documents.getOrElse(nature, mutable.Map())
  }

  /** Get the controlling docs for a specific nature TODO: Handle duplicates?
    */
  def getControllingDocuments(nature: MetadataNature): List[MetadataDocument] = {
    documents
      .getOrElse(nature, mutable.Map())
      .flatMap(docs =>
        docs._2
          .flatMap(p => MetadataDocument(Path(p)))
          .filter(md => md.nature == nature)
      )
      .toList
  }

  /** Get all documents for specific type of metadata that contribute to a specific typename */
  def get(nature: MetadataNature, typeName: TypeName): List[MetadataDocument] = {
    documents.get(nature) match {
      case Some(byTypeName) =>
        byTypeName
          .getOrElse(typeName.rawStringLower, Nil)
          .flatMap(path => MetadataDocument(Path(path)))
      case None => Nil
    }
  }

  def get(typeName: TypeName): List[MetadataDocument] = {
    val rawTypeName = typeName.rawStringLower
    documents.values
      .flatMap(_.get(rawTypeName))
      .flatten
      .flatMap(path => MetadataDocument(Path(path)))
      .toList
  }

  /** Upsert a document. Document defining new or existing types return true, if the document would
    * create a duplicate type it is not added to the store and false is returned.
    */
  def upsert(logger: IssueLogger, document: MetadataDocument): Boolean = {

    if (!isVisibleFile(document.path)) return false

    // Partial can always be upserted
    if (document.nature.partialType) {
      onAdd(logger, document)
      return true
    }

    // As can metadata defining a new type
    val typeName = document.typeName(namespace)
    val existing = get(document.nature, typeName)
    if (existing.isEmpty) {
      onAdd(logger, document)
      return true
    }

    // Or existing documents
    if (existing.contains(document))
      return true

    // Otherwise we should ignore it, sad but true
    logger.log(
      Issue(
        document.path,
        Diagnostic(
          ERROR_CATEGORY,
          Location.empty,
          s"Duplicate type '$typeName' found in '${document.path}', ignoring this file"
        )
      )
    )
    false
  }

  /** Remove a document from the store. Returns true if the document was in the store and removed.
    */
  def remove(document: MetadataDocument): Boolean = {
    val typeName = document.controllingTypeName(namespace).rawStringLower
    val docMap   = safeDocumentMap(document.controllingNature)
    val existing = docMap.get(typeName).exists(d => d.contains(document.path))
    if (existing) {
      val residual = docMap(typeName).filterNot(_ == document.path)
      if (residual.nonEmpty)
        docMap.put(typeName, docMap(typeName).filterNot(_ == document.path))
      else
        docMap.remove(typeName)
    }
    existing
  }

  override def onAdd(logger: IssueLogger, document: MetadataDocument): Unit = {

    // Reject if document may be ignored
    if (document.ignorable())
      return

    /* TODO: Remove */
    // Duplicate detect for documents that define a complete type
    val typeName = document.typeName(namespace)
    if (!document.nature.partialType) {
      val existing = get(document.nature, typeName)
      if (existing.nonEmpty) {
        logger.log(
          Issue(
            document.path,
            Diagnostic(
              ERROR_CATEGORY,
              Location.empty,
              s"File creates duplicate type '$typeName' as '${existing.head.path}', ignoring"
            )
          )
        )
        return
      }
    }

    // If we find a field or fieldSet without a SObject metadata, fake it exists to make later processing easier
    if (document.nature == FieldNature || document.nature == FieldSetNature) {
      val objectDir = document.path.parent.parent
      val metaFile  = objectDir.join(objectDir.basename + ".object-meta.xml")
      val docType   = SObjectDocument(metaFile, Name(objectDir.basename))
      val typeName  = docType.typeName(namespace)
      if (get(SObjectNature, typeName).isEmpty) {
        safeDocumentMap(SObjectNature).put(typeName.rawStringLower, List(metaFile))
      }
    }

    {
      val docMap   = safeDocumentMap(document.controllingNature)
      val typeName = document.controllingTypeName(namespace).rawStringLower
      docMap.put(typeName, (document.path :: docMap.getOrElse(typeName, Nil)).distinct)
    }
  }

  private def safeDocumentMap(nature: MetadataNature): mutable.HashMap[String, List[PathLike]] = {
    documents.getOrElseUpdate(
      nature, {
        mutable.HashMap[String, List[PathLike]]()
      }
    )
  }

  def isVisibleFile(path: PathLike): Boolean = {
    ignore.forall(_.includeFile(path)) && isVisiblePath(path.parent)
  }

  /** Check a directory path would be included in index. */
  @scala.annotation.tailrec
  private def isVisiblePath(path: PathLike): Boolean = {
    if (this.path == path) return true
    if (!ignore.forall(_.includeDirectory(path))) return false

    val parent = path.parent
    if (parent != path)
      isVisiblePath(parent)
    else
      false
  }

}

object DocumentIndex {

  /** Construct a new DocumentIndex from a recursive descent scan of the passed path. */
  def apply(
    logger: IssueLogger,
    namespace: Option[Name],
    projectPath: PathLike,
    path: PathLike
  ): DocumentIndex = {
    val ignore = logger.logAndGet(ForceIgnore(projectPath.join(".forceignore")))
    new DocumentIndex(logger, path, namespace, ignore)
  }

  /** Simplified construction for MDAPI only projects where projectDir = module path. */
  def apply(logger: IssueLogger, namespace: Option[Name], path: PathLike): DocumentIndex = {
    DocumentIndex(logger, namespace, path, path)
  }
}
