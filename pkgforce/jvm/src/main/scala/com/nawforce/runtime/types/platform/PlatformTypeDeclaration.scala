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
import com.nawforce.runtime.types.platform.PlatformTypeDeclaration.{
  createTypeName,
  emptyAnnotations,
  emptyArgs,
  emptyInitializers,
  emptyPaths,
  emptyProperties,
  emptyTypeDeclarations,
  platformPackage
}
import com.nawforce.runtime.workspace.{IModuleTypeDeclaration, IPM}

import java.nio.file.{FileSystemNotFoundException, FileSystems, Files, Paths}
import java.util
import scala.collection.immutable.{ArraySeq, HashMap}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/* Platform type declaration, a wrapper around a com.nawforce.platform Java classes */
class PlatformTypeDeclaration(
  val module: IPM.Module,
  val native: Any,
  override val enclosing: Option[PlatformTypeDeclaration]
) extends IModuleTypeDeclaration {

  final private val cls: java.lang.Class[_] = native.asInstanceOf[java.lang.Class[_]]

  final val nature: Nature = {
    (cls.isEnum, cls.isInterface) match {
      case (true, _) => ENUM_NATURE
      case (_, true) => INTERFACE_NATURE
      case _         => CLASS_NATURE
    }
  }

  protected def getSuperClassTypeInfo: Option[TypeInfo] = {
    if (cls.getSuperclass != null) {
      cls.getSuperclass.getCanonicalName match {
        case "java.lang.Object" => None
        case "java.lang.Enum"   => None
        case _                  => Some(getTypeName(cls.getSuperclass))
      }
    } else {
      None
    }
  }
  //Used for generics
  protected def getNameFromGenericType(name: String): String = {
    name
  }

  final val typeInfo: TypeInfo = getTypeName(cls)

  override val paths: Array[String] = emptyPaths

  override val location: Location = Location.default

  override val id: Id = typeInfo.typeName.id

  override val typeNameSegment: TypeNameSegment = typeInfo.typeName

  override def extendsTypeRef: TypeRef =
    getSuperClassTypeInfo
      .flatMap(_.asOptionalUnresolved)
      .orNull

  override def implementsTypeList: TypeList = {
    val interfaces = cls.getGenericInterfaces
    if (interfaces.nonEmpty) {
      val tl = TypeList()
      interfaces.map(i => getTypeName(i.getClass)).flatMap(_.asOptionalUnresolved).foreach(tl.add)
      return if (tl.typeRefs.nonEmpty) tl else null
    }
    null
  }

  override def modifiers: ArraySeq[Modifier] =
    PlatformModifiers.typeModifiers(cls.getModifiers, nature)

  override def annotations: ArraySeq[Annotation] = emptyAnnotations

  override def initializers: ArraySeq[Initializer] = emptyInitializers

  override def innerTypes: ArraySeq[PlatformTypeDeclaration] = {
    if (nature == CLASS_NATURE) {
      ArraySeq.unsafeWrapArray(
        cls.getClasses.map(nested => new PlatformTypeDeclaration(module, nested, Some(this)))
      )
    } else {
      emptyTypeDeclarations
    }
  }

  override def constructors: ArraySeq[ConstructorDeclaration] =
    ArraySeq
      .unsafeWrapArray(cls.getConstructors)
      .filterNot(_.isSynthetic)
      .map(c => toConstructorDeclaration(c, this))

  override def methods: ArraySeq[MethodDeclaration] = {
    nature match {
      case ENUM_NATURE => ArraySeq.empty //TODO
      case _           => getMethods
    }
  }

  override def properties: ArraySeq[PropertyDeclaration] = emptyProperties

  override def fields: ArraySeq[FieldDeclaration] = getFields

  override def getFullName: String = {
    val ns = if (typeInfo.namespace.nonEmpty) s"${typeInfo.namespace.get}." else ""
    if (enclosing.nonEmpty)
      return s"$ns${enclosing.get.getFullName}.${typeInfo.typeName.toString}"
    s"$ns${typeInfo.typeName.toString}"
  }

  override def toString: String = {
    val args =
      if (typeInfo.args.nonEmpty) typeInfo.args.map(getNameFromGenericType).mkString("<", ",", ">")
      else ""
    var rawNames =
      if (enclosing.nonEmpty)
        Seq(typeInfo.typeName.id.toString, enclosing.get.getFullName)
      else Seq(typeInfo.typeName.id.toString)
    if (typeInfo.namespace.nonEmpty) {
      rawNames = rawNames :+ typeInfo.namespace.get
    }
    val name = TypeName(rawNames.map(Name(_)))
    s"$name$args"
  }

  private def getMethods: ArraySeq[MethodDeclaration] = {
    ArraySeq
      .unsafeWrapArray(cls.getMethods)
      .filter(
        _.getDeclaringClass.getCanonicalName.startsWith(PlatformTypeDeclaration.platformPackage)
      )
      .filterNot(_.isSynthetic)
      .map(toMethodDeclaration)
  }

  private def getFields: ArraySeq[FieldDeclaration] = {
    ArraySeq.unsafeWrapArray(collectFields(cls).values.toArray)
  }

  private def collectFields(
    cls: Class[_],
    accum: mutable.Map[Name, FieldDeclaration] = mutable.Map()
  ): mutable.Map[Name, FieldDeclaration] = {
    if (cls.getCanonicalName.startsWith(PlatformTypeDeclaration.platformPackage)) {
      cls.getDeclaredFields
        .filterNot(_.isSynthetic)
        .foreach(f => {
          val name = Name(f.getName)
          if (!accum.contains(name))
            accum.put(name, toFieldDeclaration(f))
        })
      Option(cls.getSuperclass).foreach(superClass => collectFields(superClass, accum))
    }
    accum
  }

  private def toConstructorDeclaration(
    ctor: java.lang.reflect.Constructor[_],
    td: PlatformTypeDeclaration
  ): ConstructorDeclaration = {
    val modifiers = PlatformModifiers.ctorModifiers(ctor.getModifiers)
    val name      = new QualifiedName()
    name.add(Id(IdToken(td.id.id.contents, Location.default)))
    ConstructorDeclaration(
      ArraySeq.empty,
      modifiers,
      name,
      toFormalParameterList(ctor.getParameters)
    )
  }

  protected def toMethodDeclaration(method: java.lang.reflect.Method): MethodDeclaration = {
    MethodDeclaration(
      emptyAnnotations,
      PlatformModifiers.fieldOrMethodModifiers(method.getModifiers),
      getPlatformTypeDeclFromType(method.getGenericReturnType),
      Id(IdToken(decodeName(method.getName), Location.default)),
      toFormalParameterList(method.getParameters)
    )
  }

  protected def toFieldDeclaration(field: java.lang.reflect.Field): FieldDeclaration = {
    FieldDeclaration(
      emptyAnnotations,
      PlatformModifiers.fieldOrMethodModifiers(field.getModifiers),
      getPlatformTypeDeclFromType(field.getGenericType).get,
      Id(IdToken(decodeName(field.getName), Location.default))
    )
  }

  protected def toFormalParameterList(
    params: Array[java.lang.reflect.Parameter]
  ): FormalParameterList = {
    val fpl = new FormalParameterList()
    params.map(toFormalParameter).foreach(fpl.add)
    fpl
  }

  protected def toFormalParameter(parameter: java.lang.reflect.Parameter): FormalParameter = {
    val p = new FormalParameter
    p.typeRef = getPlatformTypeDeclFromType(parameter.getType)
    p.add(Id(IdToken(parameter.getName, Location.default)))
    p
  }

  protected def decodeName(name: String): String = {
    if (name.endsWith("$"))
      name.substring(0, name.length - 1)
    else
      name
  }

  private def getTypeStringFromType(from: java.lang.reflect.Type): String = {
    from match {
      case cls: Class[_]                         => getTypeName(cls).toString
      case tv: java.lang.reflect.TypeVariable[_] => getNameFromGenericType(tv.getName)
      case pt: java.lang.reflect.ParameterizedType =>
        val cname = pt.getRawType.getTypeName
        assert(cname.startsWith(platformPackage), s"Reference to non-platform type $cname")
        val names = cname.drop(platformPackage.length + 1)
        val params =
          pt.getActualTypeArguments.map(getTypeStringFromType).map(getNameFromGenericType)
        s"${names}<${params.mkString(",")}>"
    }
  }

  protected def getPlatformTypeDeclFromType(
    from: java.lang.reflect.Type
  ): Option[IModuleTypeDeclaration] = {
    val typeRefNameWithGenerics = getTypeStringFromType(from)
    val unresolvedTypeRef = UnresolvedTypeRef(typeRefNameWithGenerics).toOption
    if (unresolvedTypeRef.nonEmpty) {
      preResolveArguments(unresolvedTypeRef, module)
      return PlatformTypeDeclaration.get(module, unresolvedTypeRef.get)
    }
    None
  }

  private def preResolveArguments(un: Option[UnresolvedTypeRef], module: IPM.Module): Unit = {
    if (un.nonEmpty) {
      un.get.typeNameSegments
        .filter(tns => tns.typeArguments.nonEmpty)
        .foreach(tns => {
          val args = tns.getArguments
          val newArgs = args.flatMap {
            case unref: UnresolvedTypeRef => PlatformTypeDeclaration.get(module, unref)
            case other                    => Some(other)
          }
          if (args.nonEmpty && args.length == newArgs.length)
            tns.replaceArguments(newArgs)
        })
    }
  }

  def getTypeName(cls: java.lang.Class[_]): TypeInfo = {
    val cname = cls.getCanonicalName
    if (cname == "void") {
      return TypeInfo(None, Array.empty, createTypeName("void", None))
    }
    if (cname == "java.lang.Object") {
      return TypeInfo(
        Some(TypeName.InternalObject.outer.get.name.value),
        Array(),
        createTypeName(Names.Internal.value, None)
      )

    }

    val names  = cname.drop(platformPackage.length + 1).split('.').reverse
    val params = cls.getTypeParameters.map(_.getName).map(getNameFromGenericType)
    if (params.nonEmpty) {
      TypeInfo(Some(names(1)), params, TypeNameSegment(names.head, params))
    } else {
      TypeInfo(Some(names(1)), emptyArgs, TypeNameSegment(names.head))
    }
  }

}

object PlatformTypeDeclaration {
  final val emptyPaths: Array[String]                                = Array.empty
  final val emptyArgs: Array[String]                                 = Array.empty
  final val emptyTypeDeclarations: ArraySeq[PlatformTypeDeclaration] = ArraySeq.empty
  final val emptyAnnotations: ArraySeq[Annotation]                   = ArraySeq.empty
  final val emptyInitializers: ArraySeq[Initializer]                 = ArraySeq.empty
  final val emptyProperties: ArraySeq[PropertyDeclaration]           = ArraySeq.empty
  final val priorityNamespaces: Seq[Name]                            = Seq(Names.System, Names.Schema, Names.Database)

  /* Java package prefix for platform types */
  private val platformPackage = "com.nawforce.runforce"

  /* Cache of loaded platform declarations */
  private val declarationCache = mutable.Map[DotName, Option[PlatformTypeDeclaration]]()

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
      declarationCache.getOrElseUpdate(
        dotName, {
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
        }
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

  def createTypeName(
    name: String,
    params: Option[ArraySeq[IModuleTypeDeclaration]]
  ): TypeNameSegment = {
    val typeName = TypeNameSegment(name)
    if (params.nonEmpty)
      typeName.typeArguments = Some(TypeArguments(params.get))
    typeName
  }

  private val typeAliasMap: Map[TypeName, TypeName] = Map(
    TypeName.Object                 -> TypeName.InternalObject,
    TypeName.ApexPagesPageReference -> TypeName.PageReference
  ) ++ ambiguousAliasMap

}

case class TypeInfo(namespace: Option[String], args: Array[String], typeName: TypeNameSegment) {

  def asOptionalUnresolved: Option[UnresolvedTypeRef] = {
    UnresolvedTypeRef(toString).toOption
  }

  override def toString: String = {
    val argStr = if (args.nonEmpty) args.mkString("<", ",", ">") else ""
    if (namespace.nonEmpty) s"${namespace.get}.${typeName.toString}$argStr"
    else s"${typeName.toString}$argStr"
  }
}
