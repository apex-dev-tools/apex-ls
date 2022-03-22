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

package com.nawforce.runtime.types.platform

import com.financialforce.oparser._
import com.nawforce.pkgforce.names.{DotName, Name, Names}
import com.nawforce.runtime.types.platform.PlatformTypeDeclaration.{emptyPaths, getTypeName}
import com.nawforce.runtime.workspace.{IModuleTypeDeclaration, IPM}

import java.lang.reflect.TypeVariable
import java.nio.file.{FileSystemNotFoundException, FileSystems, Files, Paths}
import java.util
import scala.collection.immutable.{ArraySeq, HashMap}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/* Platform type declaration, a wrapper around a com.nawforce.platform Java classes */
class PlatformTypeDeclaration(
  val native: Any,
  override val enclosing: Option[PlatformTypeDeclaration]
) extends IModuleTypeDeclaration {

  final private val cls: java.lang.Class[_] = native.asInstanceOf[java.lang.Class[_]]

  final private val typeInfo = getTypeName(cls)

  override def module: Option[IPM.Module] = None

  override val paths: Array[String] = emptyPaths

  override val location: Location = Location.default

  override val id: Id = typeInfo._2.id

  override val typeName: TypeName = typeInfo._2

  override def extendsTypeRef: TypeRef = null // TODO

  override def implementsTypeList: TypeList = null // TODO

  override def modifiers: ArraySeq[Modifier] = ArraySeq.empty // TODO

  override def annotations: ArraySeq[Annotation] = ArraySeq.empty // TODO

  override def initializers: ArraySeq[Initializer] = ArraySeq.empty // TODO

  override def innerTypes: ArraySeq[ITypeDeclaration] = ArraySeq.empty // TODO

  override def constructors: ArraySeq[ConstructorDeclaration] = ArraySeq.empty // TODO

  override def methods: ArraySeq[MethodDeclaration] = ArraySeq.empty // TODO

  override def properties: ArraySeq[PropertyDeclaration] = ArraySeq.empty // TODO

  override def fields: ArraySeq[FieldDeclaration] = ArraySeq.empty // TODO

}

object PlatformTypeDeclaration {
  final val emptyPaths: Array[String] = Array.empty

  /* Java package prefix for platform types */
  private val platformPackage = "com.nawforce.runforce"

  /* Cache of loaded platform declarations */
  private val declarationCache = mutable.Map[Name, Option[PlatformTypeDeclaration]]()

  /* Get a Path that leads to platform classes */
  lazy val platformPackagePath: java.nio.file.Path = {
    val path = "/" + platformPackage.replaceAll("\\.", "/")
    val uri  = classOf[com.nawforce.runforce.Internal.Object$].getResource(path).toURI
    if (uri.getScheme.equalsIgnoreCase("file")) {
      Paths.get(uri)
    } else {
      try {
        FileSystems.getFileSystem(uri).getPath(path)
      } catch {
        case _: FileSystemNotFoundException =>
          FileSystems.newFileSystem(uri, new util.HashMap[String, String]).getPath(path)
      }
    }
  }

  /* Get a type, in general don't call this direct, use TypeRequest which will delegate here. If needed this will
   * construct a GenericPlatformTypeDeclaration to specialise a PlatformTypeDeclaration but you must provide from
   * for that to be possible as this allows discovery of the correct type arguments.
   */
  def get(typeName: Name, from: Option[TypeDeclaration]): Option[PlatformTypeDeclaration] = {
    getDeclaration(typeName)

    // TODO: Handle generic request
  }

  /* Get a declaration for a class from a Name. */
  private def getDeclaration(typeName: Name): Option[PlatformTypeDeclaration] = {
    val dotName = DotName(typeName.value)
    declarationCache.getOrElseUpdate(
      typeName, {
        val matched = classNameMap.get(dotName)
        assert(matched.size < 2, s"Found multiple platform type matches for $typeName")
        matched.map(
          name =>
            new PlatformTypeDeclaration(
              classOf[PlatformTypeDeclaration].getClassLoader
                .loadClass(platformPackage + "." + name),
              None
            )
        )
      }
    )
  }

  /* Valid platform class names */
  lazy val classNames: Iterable[DotName] = classNameMap.keys

  /* All the namespaces - excluding our special ones! */
  lazy val namespaces: Set[Name] = classNameMap.keys
    .filter(_.isCompound)
    .map(_.firstName)
    .filterNot(name => name == Names.SObjects || name == Names.Internal)
    .toSet

  /* Map of class names, it's a map just to allow easy recovery of the original case by looking at value */
  private lazy val classNameMap: HashMap[DotName, DotName] = {
    val names = mutable.HashMap[DotName, DotName]()
    indexDir(platformPackagePath, DotName(Seq()), names)
    HashMap[DotName, DotName]() ++ names
  }

  /* Index .class files, we have to index to make sure we get natural case sensitive names, but also used
   * to re-map SObject so they appear in Schema namespace.
   */
  private def indexDir(
    path: java.nio.file.Path,
    prefix: DotName,
    accum: mutable.HashMap[DotName, DotName]
  ): Unit = {
    Files
      .list(path)
      .iterator
      .asScala
      .foreach(entry => {
        val filename = entry.getFileName.toString
        if (
          Files.isRegularFile(entry) && filename.endsWith(".class") &&
          (filename.endsWith("$.class") || !filename.contains('$'))
        ) {
          val dotName = prefix.append(Name(filename.dropRight(".class".length)))
          if (dotName.names.head == Names.SObjects) {
            accum.put(DotName(Names.Schema +: dotName.names.tail), dotName)
          } else {
            accum.put(dotName, dotName)
          }
        } else if (Files.isDirectory(entry)) {
          val safeFilename = filename.replace("/", "").replace("\\", "")
          indexDir(entry, prefix.append(Name(safeFilename)), accum)
        }
      })
  }

  def getTypeName(cls: java.lang.Class[_]): (String, TypeName) = {
    val cname = cls.getCanonicalName
    val names = cname.drop(platformPackage.length + 1).split('.').reverse
    // TODO: Remove this, just to be sure for now
    assert(names.length == 2)
    val params = cls.getTypeParameters.map(_.getName)
    if (params.nonEmpty) {
      (names(1), TypeName(names.head, params))
    } else {
      (names(1), TypeName(names.head))
    }
  }

}
