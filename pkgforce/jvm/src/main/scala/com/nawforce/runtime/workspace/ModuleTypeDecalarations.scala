/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.runtime.workspace

import com.financialforce.oparser._
import com.nawforce.pkgforce.memory.IdentityEquality

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

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

  // TODO: These should be private
  var _location: Location = _

  var _id: IdToken                  = _
  var _extendsTypeRef: TypeRef      = _
  var _implementsTypeList: TypeList = _

  var _modifiers: ArraySeq[Modifier]     = Modifiers.emptyArraySeq
  var _annotations: ArraySeq[Annotation] = Annotations.emptyArraySeq

  var _initializers: ArraySeq[Initializer]            = Initializer.emptyArraySeq
  var _innerTypes: ArraySeq[TypeDeclaration]          = TypeDeclaration.emptyArraySeq
  var _constructors: ArraySeq[ConstructorDeclaration] = ConstructorDeclaration.emptyArraySeq
  var _methods: ArraySeq[MethodDeclaration]           = MethodDeclaration.emptyArraySeq
  var _properties: ArraySeq[PropertyDeclaration]      = PropertyDeclaration.emptyArraySeq
  var _fields: ArraySeq[FieldDeclaration]             = FieldDeclaration.emptyArraySeq

  // This is used to stage declaration that need adding to above, see syncBodyDeclarations()
  private var _bodyDecls: mutable.ArrayBuffer[MutableTypeAppendable] = mutable.ArrayBuffer()

  override def paths: Array[String] = Array(path)
  override def location: Location   = _location

  override def id: IdToken = _id

  override def typeNameSegment: TypeNameSegment = new TypeNameSegment(id, TypeArguments.empty)

  override def enclosing: Option[IMutableModuleTypeDeclaration] = Option(_enclosing)
  override def extendsTypeRef: TypeRef                          = _extendsTypeRef
  override def implementsTypeList: TypeList                     = _implementsTypeList

  override def modifiers: ArraySeq[Modifier]       = _modifiers
  override def annotations: ArraySeq[Annotation]   = _annotations
  override def initializers: ArraySeq[Initializer] = ArraySeq.unsafeWrapArray(_initializers.toArray)

  override def innerTypes: ArraySeq[TypeDeclaration] = ArraySeq.unsafeWrapArray(_innerTypes.toArray)
  override def constructors: ArraySeq[ConstructorDeclaration] =
    ArraySeq.unsafeWrapArray(_constructors.toArray)
  override def methods: ArraySeq[MethodDeclaration] = ArraySeq.unsafeWrapArray(_methods.toArray)
  override def properties: ArraySeq[PropertyDeclaration] =
    ArraySeq.unsafeWrapArray(_properties.toArray)
  override def fields: ArraySeq[FieldDeclaration] = ArraySeq.unsafeWrapArray(_fields.toArray)

  override def setId(id: IdToken): Unit                                = _id = id
  override def setLocation(location: Location): Unit                   = _location = location
  override def setExtends(typeRef: TypeRef): Unit                      = _extendsTypeRef = typeRef
  override def setImplements(typeList: TypeList): Unit                 = _implementsTypeList = typeList
  override def setModifiers(modifiers: ArraySeq[Modifier]): Unit       = _modifiers = modifiers
  override def setAnnotations(annotations: ArraySeq[Annotation]): Unit = _annotations = annotations

  override def appendInitializer(init: Initializer): Unit            = _bodyDecls.append(init)
  override def appendInnerType(inner: IMutableTypeDeclaration): Unit = _bodyDecls.append(inner)
  override def appendConstructor(ctor: ConstructorDeclaration): Unit = _bodyDecls.append(ctor)
  override def appendProperty(prop: PropertyDeclaration): Unit       = _bodyDecls.append(prop)
  override def appendField(field: FieldDeclaration): Unit            = _bodyDecls.append(field)
  override def appendMethod(method: MethodDeclaration): Unit         = _bodyDecls.append(method)

  override def onComplete(): Unit = {
    // On completion of the type we distribute out the collected _bodyDecls to the right collection. This gives
    // us the ability to move away from ArrayBuffer's which waste memory to exactly sized ArraySeq's
    val inners = new ArrayBuffer[TypeDeclaration]()
    _bodyDecls
      .groupBy(bd => bd.getClass)
      .foreach(kv => {
        kv._2.head match {
          case _: Initializer =>
            _initializers = ArraySeq.unsafeWrapArray(kv._2.map(_.asInstanceOf[Initializer]).toArray)
          case _: ConstructorDeclaration =>
            _constructors =
              ArraySeq.unsafeWrapArray(kv._2.map(_.asInstanceOf[ConstructorDeclaration]).toArray)
          case _: MethodDeclaration =>
            _methods =
              ArraySeq.unsafeWrapArray(kv._2.map(_.asInstanceOf[MethodDeclaration]).toArray)
          case _: PropertyDeclaration =>
            _properties =
              ArraySeq.unsafeWrapArray(kv._2.map(_.asInstanceOf[PropertyDeclaration]).toArray)
          case _: FieldDeclaration =>
            _fields = ArraySeq.unsafeWrapArray(kv._2.map(_.asInstanceOf[FieldDeclaration]).toArray)
          case _ =>
            // Group all inners together
            inners.addAll(kv._2.map(_.asInstanceOf[TypeDeclaration]))
        }
      })

    if (inners.nonEmpty)
      _innerTypes = ArraySeq.unsafeWrapArray(inners.toArray)

    // onComplete should only be called once, so this should be safe ;-)
    _bodyDecls = null
  }
}

object TypeDeclaration {
  final val emptyArrayBuffer = mutable.ArrayBuffer[TypeDeclaration]()
  final val emptyArraySeq    = ArraySeq[TypeDeclaration]()
}

class ClassTypeDeclaration(
  _module: IPM.Module,
  path: String,
  enclosing: IMutableModuleTypeDeclaration
) extends TypeDeclaration(_module, path, CLASS_NATURE, enclosing) {

  override def toString: String = {
    import StringUtils._
    val base =
      s"""Class:      $id
         |Path:       $path
         |Location:   ${id.location}
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
) extends TypeDeclaration(_module, path, INTERFACE_NATURE, enclosing) {

  override def toString: String = {
    import StringUtils._
    s"""Interface:  $id
       |Path:       $path
       |Location:   ${id.location}
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
) extends TypeDeclaration(_module, path, ENUM_NATURE, enclosing) {

  override def toString: String = {
    import StringUtils._
    s"""Enum:       $id
       |Path:       $path
       |Location:   ${id.location}
       |Annotation: ${asString(_annotations)}
       |Modifiers:  ${asString(_modifiers)}
       |Constants:
       |${fields.map(f => s"${f.id.location} ${f.id.contents}").mkString("\n")}
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
