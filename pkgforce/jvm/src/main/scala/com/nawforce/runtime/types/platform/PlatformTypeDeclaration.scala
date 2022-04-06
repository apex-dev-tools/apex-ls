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
import com.nawforce.pkgforce.names.TypeName.ambiguousAliasMap
import com.nawforce.pkgforce.names.{DotName, Name, Names, TypeName}
import com.nawforce.pkgforce.parsers.{CLASS_NATURE, ENUM_NATURE, INTERFACE_NATURE, Nature}
import com.nawforce.runtime.types.platform.PlatformTypeDeclaration.{
  emptyPaths,
  emptyTypeDeclarations,
  getTypeName
}
import com.nawforce.runtime.workspace.{IModuleTypeDeclaration, IPM}

import java.nio.file.{FileSystemNotFoundException, FileSystems, Files, Paths}
import java.util
import scala.collection.immutable.{ArraySeq, HashMap}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/* Platform type declaration, a wrapper around a com.nawforce.platform Java classes */
class PlatformTypeDeclaration(
  _module: IPM.Module,
  val native: Any,
  override val enclosing: Option[PlatformTypeDeclaration]
) extends IModuleTypeDeclaration {

  module = _module

  final private val cls: java.lang.Class[_] = native.asInstanceOf[java.lang.Class[_]]

  final val nature: Nature = {
    (cls.isEnum, cls.isInterface) match {
      case (true, _) => ENUM_NATURE
      case (_, true) => INTERFACE_NATURE
      case _         => CLASS_NATURE
    }
  }

  override def getFullName: String = {
    val args = if (typeInfo.args.nonEmpty) typeInfo.args.mkString("<", ",", ">") else ""
    if (enclosing.nonEmpty)
      return s"${typeInfo.namespace}.${enclosing.get.getFullName}.${typeInfo.typeName.id}$args"
    s"${typeInfo.namespace}.${typeInfo.typeName.id}$args"
  }

  override def toString: String = {
    val args = if (typeInfo.args.nonEmpty) typeInfo.args.mkString("<", ",", ">") else ""
    val rawNames =
      if (enclosing.nonEmpty)
        Seq(typeInfo.typeName.id.toString, enclosing.get.getFullName, typeInfo.namespace)
      else Seq(typeInfo.typeName.id.toString, typeInfo.namespace)
    val name = TypeName(rawNames.map(Name(_)))

    s"$name$args"
  }

  final val typeInfo: TypeInfo = getTypeName(cls)

  override val paths: Array[String] = emptyPaths

  override val location: Location = Location.default

  override val id: Id = typeInfo.typeName.id

  override val typeNameSegment: TypeNameSegment = typeInfo.typeName

  override def extendsTypeRef: TypeRef = null // TODO

  override def implementsTypeList: TypeList = null // TODO

  override def modifiers: ArraySeq[Modifier] = ArraySeq.empty // TODO

  override def annotations: ArraySeq[Annotation] = ArraySeq.empty // TODO

  override def initializers: ArraySeq[Initializer] = ArraySeq.empty // TODO

  override def innerTypes: ArraySeq[PlatformTypeDeclaration] = {
    if (nature == CLASS_NATURE) {
      ArraySeq.unsafeWrapArray(
        cls.getClasses.map(nested => new PlatformTypeDeclaration(module, nested, Some(this)))
      )
    } else {
      emptyTypeDeclarations
    }
  }

  override def constructors: ArraySeq[ConstructorDeclaration] = ArraySeq.empty // TODO

  override def methods: ArraySeq[MethodDeclaration] = ArraySeq.empty // TODO

  override def properties: ArraySeq[PropertyDeclaration] = ArraySeq.empty // TODO

  override def fields: ArraySeq[FieldDeclaration] = ArraySeq.empty // TODO
}

object PlatformTypeDeclaration {
  final val emptyPaths: Array[String]                                = Array.empty
  final val emptyArgs: Array[String]                                 = Array.empty
  final val emptyTypeDeclarations: ArraySeq[PlatformTypeDeclaration] = ArraySeq.empty
  final val priorityNamespaces: Seq[Name]                            = Seq(Names.System, Names.Schema, Names.Database)

  /* Java package prefix for platform types */
  private val platformPackage = "com.nawforce.runforce"

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

  def get(module: IPM.Module, typeRef: UnresolvedTypeRef): Option[PlatformTypeDeclaration] = {

    // Conversion will fail if typeRef has unresolved type arguments
    val typeNameOpt = asTypeName(typeRef)
    if (typeNameOpt.isEmpty)
      return None

    // Non-generic lookup
    val aliasedTypeName = typeAliasMap.getOrElse(typeNameOpt.get, typeNameOpt.get)
    val tdOpt           = getDeclaration(module, asDotName(aliasedTypeName))
    if (tdOpt.isEmpty) return None

    // Quick fail on wrong number of type variables
    val td            = tdOpt.get
    val typeArguments = td.typeNameSegment.getArguments
    if (typeArguments.length != aliasedTypeName.params.size)
      return None

    if (typeArguments.isEmpty) {
      Some(td)
    } else {
      val resolvedArgs = typeRef.typeNameSegments.last.getArguments.collect {
        case td: IModuleTypeDeclaration => td
      }
      Some(GenericPlatformTypeDeclaration.get(td.module, resolvedArgs, td))
    }
  }

  /* Converts UnresolvedTypeRef to a TypeName, if all type arguments are resolved */
  private def asTypeName(typeRef: UnresolvedTypeRef): Option[TypeName] = {
    asTypeName(typeRef.typeNameSegments.toList, None).map(fullTypeName => {
      var wrapped = fullTypeName
      for (_ <- 0 until typeRef.arraySubscripts)
        wrapped = new TypeName(Names.List$, Seq(wrapped), Some(TypeName.System))
      wrapped
    })
  }

  private def asTypeName(typeName: TypeNameSegment, outer: Option[TypeName]): Option[TypeName] = {
    val resolvedSegments =
      typeName.getArguments.collect { case td: IModuleTypeDeclaration => td }.map(_.typeName)
    if (resolvedSegments.length != typeName.getArguments.length) {
      None
    } else {
      val resolvedTypeNames: Seq[TypeName] =
        resolvedSegments.flatMap(segments => asTypeName(segments.toList))
      Some(new TypeName(Name(typeName.id.toString), resolvedTypeNames, outer))
    }
  }

  def asTypeName(
    typeNameSegments: List[TypeNameSegment],
    outer: Option[TypeName] = None
  ): Option[TypeName] = {
    typeNameSegments match {
      case hd :: tl => asTypeName(hd, outer).flatMap(converted => asTypeName(tl, Some(converted)))
      case Nil      => outer
    }
  }

  private def asDotName(typeName: TypeName): DotName = {
    typeName.outer match {
      case None    => DotName(Seq(typeName.name))
      case Some(x) => asDotName(x).append(typeName.name)
    }
  }

  /* Get a declaration for a class from a Name. */
  private def getDeclaration(
    module: IPM.Module,
    dotName: DotName
  ): Option[PlatformTypeDeclaration] = {
    if (dotName.names.length == 3) {
      val tailName = dotName.names.last.toString
      getDeclaration(module, DotName(dotName.names.take(2))).flatMap(td => {
        td.innerTypes.find(_.id.toString.equalsIgnoreCase(tailName))
      })
    } else if (dotName.names.length == 2) {
      val matched = classNameMap.get(dotName)
      assert(matched.size < 2, s"Found multiple platform type matches for $dotName")
      matched.map(
        name =>
          new PlatformTypeDeclaration(
            module,
            classOf[PlatformTypeDeclaration].getClassLoader
              .loadClass(platformPackage + "." + name),
            None
          )
      )
    } else {
      None
    }
  }

  /* Valid platform class names */
  lazy val classNames: Iterable[DotName] = classNameMap.keys

  /* All the namespaces - excluding our special ones! */
  lazy val namespaces: Seq[Name] = priorityNamespaces ++ classNameMap.keys
    .filter(_.isCompound)
    .map(_.firstName)
    .toSeq
    .distinct
    .filterNot(name => name == Names.SObjects || name == Names.Internal)
    .filterNot(priorityNamespaces.contains)

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

  def getTypeName(cls: java.lang.Class[_]): TypeInfo = {
    val cname  = cls.getCanonicalName
    val names  = cname.drop(platformPackage.length + 1).split('.').reverse
    val params = cls.getTypeParameters.map(_.getName)
    if (params.nonEmpty) {
      TypeInfo(names(1), params, TypeNameSegment(names.head, params))
    } else {
      TypeInfo(names(1), emptyArgs, TypeNameSegment(names.head))
    }
  }

  def createTypeName(name: String, params: ArraySeq[IModuleTypeDeclaration]): TypeNameSegment = {
    val typeName = TypeNameSegment(name)
    typeName.typeArguments = Some(TypeArguments(params))
    typeName
  }

  private val typeAliasMap: Map[TypeName, TypeName] = Map(
    TypeName.Object                 -> TypeName.InternalObject,
    TypeName.ApexPagesPageReference -> TypeName.PageReference
  ) ++ ambiguousAliasMap
}

case class TypeInfo(namespace: String, args: Array[String], typeName: TypeNameSegment)
