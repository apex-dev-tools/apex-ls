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

/** Metadata document index, maintains information on available metadata files and reports errors
  * against them for things like duplicates or missing meta files. See DocumentScanner for initial
  * generation of the index. Documents are grouped under a controlling typeName to make it easier to
  * validate them later, so cls-meta.xml files are grouped with .cls and fields, fieldSets & sharing
  * reasons are grouped with object-meta.xml files.
  */
class DocumentIndex(
  val path: PathLike,
  logger: IssuesManager,
  namespace: Option[Name],
  ignore: Option[ForceIgnore]
) {

  /** Store Nature->Type name (lowercase)->Path string */
  private val documents =
    new mutable.HashMap[MetadataNature, mutable.HashMap[String, List[PathLike]]]()

  /** Basic validator for metadata documents */
  private val validator = new MetadataValidator(logger, namespace)

  // Run scanner to prime the index & then validate everything
  DocumentIndex.indexPath(path, ignore, this)
  documents.foreach(byNature => {
    byNature._2.foreach(byTypename => validator.validate(byNature._1, byTypename._2))
  })

  def size: Int = documents.values.map(_.values.size).sum

  /** Get all documents for specific type of metadata, beware mutable Map to avoid conversion */
  def get(nature: MetadataNature): mutable.Map[String, List[PathLike]] = {
    documents.getOrElse(nature, mutable.Map())
  }

  /** Get the controlling docs for a specific nature. */
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

  /** Get all documents for specific typename, this is a little more expensive than searching for a
    * specific nature (see above) but also more general.
    */
  def get(typeName: TypeName): List[MetadataDocument] = {
    val rawTypeName = typeName.rawStringLower
    documents.values
      .find(_.contains(rawTypeName))
      .getOrElse(mutable.Map[String, List[PathLike]]())
      .getOrElse(rawTypeName, Nil)
      .flatMap(path => MetadataDocument(Path(path)))
  }

  /** Upsert a document. Document defining new or existing types return true, if the document would
    * create a duplicate type it is not added to the store and false is returned.
    */
  def upsert(logger: IssueLogger, document: MetadataDocument): Boolean = {

    if (!isVisibleFile(document.path)) return false

    // Partial can always be upserted
    if (document.nature.partialType) {
      indexDocument(document, deferValidation = false)
      return true
    }

    // As can metadata defining a new type
    val typeName = document.typeName(namespace)
    val existing = get(document.nature, typeName)
    if (existing.isEmpty) {
      indexDocument(document, deferValidation = false)
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
      if (residual.nonEmpty) {
        docMap.put(typeName, residual)
        validator.validate(document.controllingNature, residual)
      } else {
        docMap.remove(typeName)
      }
    }
    existing
  }

  private def indexDocument(document: MetadataDocument, deferValidation: Boolean): Unit = {
    if (document.ignorable())
      return

    val docMap   = safeDocumentMap(document.controllingNature)
    val typeName = document.controllingTypeName(namespace).rawStringLower
    val docs     = (document.path :: docMap.getOrElse(typeName, Nil)).distinct
    docMap.put(typeName, docs)
    if (!deferValidation)
      validator.validate(document.controllingNature, docs)
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

  /** Construct a new DocumentIndex over the passed path. */
  def apply(
    logger: IssuesManager,
    namespace: Option[Name],
    projectPath: PathLike,
    path: PathLike
  ): DocumentIndex = {
    new DocumentIndex(
      path,
      logger,
      namespace,
      logger.logAndGet(ForceIgnore(projectPath.join(".forceignore")))
    )
  }

  /** Simplified construction for MDAPI only projects where projectDir = module path. */
  def apply(logger: IssuesManager, namespace: Option[Name], path: PathLike): DocumentIndex = {
    DocumentIndex(logger, namespace, path, path)
  }

  private def indexPath(
    path: PathLike,
    forceIgnore: Option[ForceIgnore],
    index: DocumentIndex
  ): Unit = {

    if (isExcluded(path))
      return
    if (path.isDirectory) {
      if (forceIgnore.forall(_.includeDirectory(path))) {
        val entries = path.splitDirectoryEntries()
        // Enforce top-down handling
        entries._1.foreach(file => addPath(file, forceIgnore, index))
        entries._2.foreach(dir => indexPath(dir, forceIgnore, index))
      } else {
        LoggerOps.debug(s"Ignoring directory $path")
      }
    } else {
      addPath(path, forceIgnore, index)
    }
  }

  private def addPath(
    path: PathLike,
    forceIgnore: Option[ForceIgnore],
    index: DocumentIndex
  ): Unit = {
    // Not testing if this is a regular file to improve scan performance, will fail later on read
    if (forceIgnore.forall(_.includeFile(path))) {
      val dt = MetadataDocument(path)
      dt.foreach(dt => index.indexDocument(dt, deferValidation = true))
    } else {
      LoggerOps.debug(s"Ignoring file $path")
    }
  }

  /** Exclude some paths that we would waste time searching. */
  private def isExcluded(path: PathLike): Boolean = {
    val basename = path.basename
    basename.startsWith(".") || basename == "node_modules"
  }

}
