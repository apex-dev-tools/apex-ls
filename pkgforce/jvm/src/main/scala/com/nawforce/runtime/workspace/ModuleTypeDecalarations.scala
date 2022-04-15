/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.workspace

import com.financialforce.oparser._
import com.nawforce.pkgforce.memory.IdentityEquality

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

trait IModuleTypeDeclaration extends ITypeDeclaration {
  val module: IPM.Module

  override def enclosing: Option[IModuleTypeDeclaration]
  override def innerTypes: ArraySeq[IModuleTypeDeclaration]

  // Helper to save from dealing with Option in Java
  def namespaceAsString: String = module.namespaceAsString

  // Override to include namespace
  override def getFullName: String = {
    Option(module)
      .flatMap(_.namespace)
      .map(ns => s"$ns.${super.getFullName}")
      .getOrElse(super.getFullName)
  }
}

trait IMutableModuleTypeDeclaration
    extends IMutableTypeDeclaration
    with IModuleTypeDeclaration
    with IdentityEquality {
  override def enclosing: Option[IMutableModuleTypeDeclaration]
  override def innerTypes: ArraySeq[IMutableModuleTypeDeclaration]
}

sealed class TypeDeclaration(
  val module: IPM.Module,
  val path: String,
  val nature: TypeNature,
  _enclosing: IMutableModuleTypeDeclaration
) extends IMutableModuleTypeDeclaration {
  var _location: Location = _

  var _id: Id                       = _
  var _extendsTypeRef: TypeRef      = _
  var _implementsTypeList: TypeList = _

  var _modifiers: mutable.ArrayBuffer[Modifier]       = mutable.ArrayBuffer()
  var _annotations: mutable.ArrayBuffer[Annotation]   = mutable.ArrayBuffer()
  var _initializers: mutable.ArrayBuffer[Initializer] = mutable.ArrayBuffer()

  var _innerTypes: mutable.ArrayBuffer[TypeDeclaration]          = mutable.ArrayBuffer()
  var _constructors: mutable.ArrayBuffer[ConstructorDeclaration] = mutable.ArrayBuffer()
  var _methods: mutable.ArrayBuffer[MethodDeclaration]           = mutable.ArrayBuffer()
  var _properties: mutable.ArrayBuffer[PropertyDeclaration]      = mutable.ArrayBuffer()
  var _fields: mutable.ArrayBuffer[FieldDeclaration]             = mutable.ArrayBuffer()

  override def paths: Array[String] = Array(path)
  override def location: Location   = _location

  override def id: Id = _id

  override def typeNameSegment: TypeNameSegment = new TypeNameSegment(id)

  override def enclosing: Option[IMutableModuleTypeDeclaration] = Option(_enclosing)
  override def extendsTypeRef: TypeRef                          = _extendsTypeRef
  override def implementsTypeList: TypeList                     = _implementsTypeList

  override def modifiers: ArraySeq[Modifier]       = ArraySeq.unsafeWrapArray(_modifiers.toArray)
  override def annotations: ArraySeq[Annotation]   = ArraySeq.unsafeWrapArray(_annotations.toArray)
  override def initializers: ArraySeq[Initializer] = ArraySeq.unsafeWrapArray(_initializers.toArray)

  override def innerTypes: ArraySeq[TypeDeclaration] = ArraySeq.unsafeWrapArray(_innerTypes.toArray)
  override def constructors: ArraySeq[ConstructorDeclaration] =
    ArraySeq.unsafeWrapArray(_constructors.toArray)
  override def methods: ArraySeq[MethodDeclaration] = ArraySeq.unsafeWrapArray(_methods.toArray)
  override def properties: ArraySeq[PropertyDeclaration] =
    ArraySeq.unsafeWrapArray(_properties.toArray)
  override def fields: ArraySeq[FieldDeclaration] = ArraySeq.unsafeWrapArray(_fields.toArray)

  override def setLocation(location: Location): Unit = _location = location
  override def setExtends(typeRef: TypeRef): Unit    = _extendsTypeRef = typeRef

  override def appendInnerType(inner: IMutableTypeDeclaration): Unit = {
    // This is rather messy, we need to accept IMutableTypeDeclaration for the caller(s) but only want to
    // expose as TypeDeclaration, it should not fail at run time, and maybe is fixable via some generics magic
    inner match { case td: TypeDeclaration => _innerTypes.append(td) }
  }
  override def appendConstructor(ctor: ConstructorDeclaration): Unit = _constructors.append(ctor)
  override def appendProperty(prop: PropertyDeclaration): Unit       = _properties.append(prop)
  override def appendField(field: FieldDeclaration): Unit            = _fields.append(field)

  override def add(m: Modifier): Unit           = _modifiers.append(m)
  override def add(a: Annotation): Unit         = _annotations.append(a)
  override def add(tl: TypeList): Unit          = _implementsTypeList = tl
  override def add(md: MethodDeclaration): Unit = _methods.append(md)
  override def add(init: Initializer): Unit     = _initializers.append(init)
  override def add(tr: UnresolvedTypeRef): Unit = _extendsTypeRef = tr
  override def add(i: Id): Unit                 = _id = id

}

object TypeDeclaration {
  final val emptyArrayBuffer = mutable.ArrayBuffer[TypeDeclaration]()
}

class ClassTypeDeclaration(
  _module: IPM.Module,
  path: String,
  enclosing: IMutableModuleTypeDeclaration
) extends TypeDeclaration(_module, path, CLASS_NATURE, enclosing)
    with AnnotationAssignable
    with IdAssignable
    with ModifierAssignable
    with TypeRefAssignable
    with TypeListAssignable
    with MethodDeclarationAssignable
    with InitializerAssignable {

  override def add(a: Annotation): Unit = _annotations.append(a)

  override def add(i: Id): Unit = _id = i

  override def add(m: Modifier): Unit = _modifiers.append(m)

  override def add(tr: UnresolvedTypeRef): Unit = _extendsTypeRef = tr

  override def add(tl: TypeList): Unit = _implementsTypeList = tl

  override def add(md: MethodDeclaration): Unit = _methods.append(md)

  override def add(init: Initializer): Unit = _initializers.append(init)

  override def toString: String = {
    import StringUtils._
    val base =
      s"""Class:      $id
         |Path:       $path
         |Location:   ${id.id.location}
         |Annotation: ${asString(_annotations)}
         |Modifiers:  ${asString(_modifiers)}
         |Extends:    ${_extendsTypeRef}
         |Implements: ${_implementsTypeList}
         |""".stripMargin

    val c =
      if (_constructors.isEmpty) ""
      else
        s"""
           |Constructors:
           |${_constructors.mkString("\n")}
           |""".stripMargin

    val m =
      if (_methods.isEmpty) ""
      else
        s"""
           |Methods:
           |${_methods.mkString("\n")}
           |""".stripMargin

    val p =
      if (_properties.isEmpty) ""
      else
        s"""
           |Properties:
           |${_properties.mkString("\n")}
           |""".stripMargin

    val f =
      if (_fields.isEmpty) ""
      else
        s"""
           |Fields:
           |${_fields.mkString("\n")}
           |""".stripMargin

    val i =
      if (innerTypes.isEmpty) ""
      else
        s"""
           |Inner types:
           |${innerTypes.mkString("\n")}
           |""".stripMargin

    base + c + m + p + f + i
  }
}

class InterfaceTypeDeclaration(
  _module: IPM.Module,
  path: String,
  enclosing: IMutableModuleTypeDeclaration
) extends TypeDeclaration(_module, path, INTERFACE_NATURE, enclosing)
    with AnnotationAssignable
    with IdAssignable
    with ModifierAssignable
    with TypeListAssignable
    with MethodDeclarationAssignable {

  override def add(a: Annotation): Unit = _annotations.append(a)

  override def add(i: Id): Unit = _id = i

  override def add(m: Modifier): Unit = _modifiers.append(m)

  override def add(tl: TypeList): Unit = _implementsTypeList = tl

  override def add(md: MethodDeclaration): Unit = _methods.append(md)

  override def toString: String = {
    import StringUtils._
    s"""Interface:  $id
       |Path:       $path
       |Location:   ${id.id.location}
       |Annotation: ${asString(_annotations)}
       |Modifiers:  ${asString(_modifiers)}
       |Implements: ${_implementsTypeList}
       |Methods:
       |${methods.mkString("\n")}
       |
       |""".stripMargin
  }
}

class EnumTypeDeclaration(
  _module: IPM.Module,
  path: String,
  enclosing: IMutableModuleTypeDeclaration
) extends TypeDeclaration(_module, path, ENUM_NATURE, enclosing)
    with IdAssignable
    with ModifierAssignable {

  override def add(a: Annotation): Unit = _annotations.append(a)

  override def add(i: Id): Unit = _id = i

  override def add(m: Modifier): Unit = _modifiers.append(m)

  override def toString: String = {
    import StringUtils._
    s"""Enum:       $id
       |Path:       $path
       |Location:   ${id.id.location}
       |Annotation: ${asString(_annotations)}
       |Modifiers:  ${asString(_modifiers)}
       |Constants:
       |${fields.map(f => s"${f.id.id.location} ${f.id.id.contents}").mkString("\n")}
       |
       |""".stripMargin
  }
}

object ModuleClassFactory extends TypeDeclFactory[IMutableModuleTypeDeclaration, IPM.Module] {
  override def create(
    module: IPM.Module,
    nature: TypeNature,
    path: String,
    enclosing: Option[IMutableModuleTypeDeclaration]
  ): IMutableModuleTypeDeclaration = {
    nature match {
      case CLASS_NATURE     => new ClassTypeDeclaration(module, path, enclosing.orNull)
      case INTERFACE_NATURE => new InterfaceTypeDeclaration(module, path, enclosing.orNull)
      case ENUM_NATURE      => new EnumTypeDeclaration(module, path, enclosing.orNull)
    }
  }
}

object Compare {

  def compareClassTypeDeclarations(
    first: ClassTypeDeclaration,
    second: ClassTypeDeclaration
  ): Unit = {

    def innerClassTypeDeclarations(o: ClassTypeDeclaration): Seq[ClassTypeDeclaration] = {
      o.innerTypes
        .filter(_.isInstanceOf[ClassTypeDeclaration])
        .map(_.asInstanceOf[ClassTypeDeclaration])
    }

    def innerInterfaceTypeDeclarations(o: ClassTypeDeclaration): Seq[InterfaceTypeDeclaration] = {
      o.innerTypes
        .filter(_.isInstanceOf[InterfaceTypeDeclaration])
        .map(_.asInstanceOf[InterfaceTypeDeclaration])
    }

    def innerEnumTypeDeclarations(o: ClassTypeDeclaration): Seq[EnumTypeDeclaration] = {
      o.innerTypes
        .filter(_.isInstanceOf[EnumTypeDeclaration])
        .map(_.asInstanceOf[EnumTypeDeclaration])
    }

    def compareInnerClasses(
      fst: Seq[ClassTypeDeclaration],
      snd: Seq[ClassTypeDeclaration]
    ): Unit = {

      fst.foreach(f => {
        val sOpt = snd.find(_.id == f.id)
        if (sOpt.isEmpty) {
          throw new Exception(s"Inner class not found ${f.id} in ${first.id}")
        }
        compareClassTypeDeclarations(f, sOpt.get)
      })
    }

    def compareInnerInterfaces(
      fst: Seq[InterfaceTypeDeclaration],
      snd: Seq[InterfaceTypeDeclaration]
    ): Unit = {

      fst.foreach(f => {
        val sOpt = snd.find(_.id == f.id)
        if (sOpt.isEmpty) {
          throw new Exception(s"Inner interface not found ${f.id} in ${first.id}")
        }
        compareInterfaceTypeDeclarations(f, sOpt.get)
      })
    }

    def compareInnerEnums(fst: Seq[EnumTypeDeclaration], snd: Seq[EnumTypeDeclaration]): Unit = {

      fst.foreach(f => {
        val sOpt = snd.find(_.id == f.id)
        if (sOpt.isEmpty) {
          throw new Exception(s"Inner enum not found ${f.id} in ${first.id}")
        }
        compareEnumTypeDeclarations(f, sOpt.get)
      })
    }

    if (first._annotations != second._annotations) {
      throw new Exception(s"Different annotation ${first._annotations} != ${second._annotations}")
    }

    if (first._modifiers != second._modifiers) {
      throw new Exception(s"Different modifiers ${first._modifiers} != ${second._modifiers}")
    }

    if (first.id != second.id) {
      throw new Exception(s"Different or empty class id ${first.id} != ${second.id}")
    }

    if (first._extendsTypeRef != second._extendsTypeRef) {
      throw new Exception(
        s"Different extends ${first._extendsTypeRef} != ${second._extendsTypeRef}"
      )
    }

    if (first._implementsTypeList != second._implementsTypeList) {
      throw new Exception(
        s"Different implements ${first._implementsTypeList} != ${second._implementsTypeList}"
      )
    }

    if (first._initializers.length != second._initializers.length) {
      throw new Exception(
        s"Different initializers ${first._initializers} != ${second._initializers}"
      )
    }

    if (first._constructors != second._constructors) {
      throw new Exception(
        s"Different constructors ${first._constructors} != ${second._constructors}"
      )
    }

    if (first._methods != second._methods) {
      throw new Exception(s"Different methods ${first._methods} != ${second._methods}")
    }

    if (first._properties != second._properties) {
      throw new Exception(s"Different properties ${first._properties} != ${second._properties}")
    }

    if (first._fields != second._fields) {
      throw new Exception(s"Different fields ${first._fields} != ${second._fields}")
    }

    compareInnerClasses(innerClassTypeDeclarations(first), innerClassTypeDeclarations(second))
    compareInnerClasses(innerClassTypeDeclarations(second), innerClassTypeDeclarations(first))

    compareInnerInterfaces(
      innerInterfaceTypeDeclarations(first),
      innerInterfaceTypeDeclarations(second)
    )
    compareInnerInterfaces(
      innerInterfaceTypeDeclarations(second),
      innerInterfaceTypeDeclarations(first)
    )

    compareInnerEnums(innerEnumTypeDeclarations(first), innerEnumTypeDeclarations(second))
    compareInnerEnums(innerEnumTypeDeclarations(second), innerEnumTypeDeclarations(first))
  }

  def compareInterfaceTypeDeclarations(
    first: InterfaceTypeDeclaration,
    second: InterfaceTypeDeclaration
  ): Unit = {

    if (first.annotations != second.annotations) {
      throw new Exception(s"Different annotation ${first.annotations} != ${second.annotations}")
    }

    if (first.modifiers != second.modifiers) {
      throw new Exception(s"Different modifiers ${first.modifiers} != ${second.modifiers}")
    }

    if (first.id != second.id) {
      throw new Exception(s"Different or empty interface id ${first.id} != ${second.id}")
    }

    if (first._implementsTypeList != second._implementsTypeList) {
      throw new Exception(
        s"Different extends ${first._implementsTypeList} != ${second._implementsTypeList}"
      )
    }

    if (first.methods != second.methods) {
      throw new Exception(s"Different methods ${first.methods} != ${second.methods}")
    }
  }

  def compareEnumTypeDeclarations(first: EnumTypeDeclaration, second: EnumTypeDeclaration): Unit = {

    if (first.annotations != second.annotations) {
      throw new Exception(s"Different annotation ${first.annotations} != ${second.annotations}")
    }

    if (first.modifiers != second.modifiers) {
      throw new Exception(s"Different modifiers ${first.modifiers} != ${second.modifiers}")
    }

    if (first.id != second.id) {
      throw new Exception(s"Different or empty enum id ${first.id} != ${second.id}")
    }

    if (first.fields != second.fields) {
      throw new Exception(s"Different constants ${first.fields} != ${second.fields}")
    }
  }
}
