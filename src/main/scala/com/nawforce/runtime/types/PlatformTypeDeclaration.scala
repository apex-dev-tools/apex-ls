/*
 [The "BSD licence"]
 Copyright (c) 2019 Kevin Jones
 All rights reserved.

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

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package com.nawforce.runtime.types

import java.nio.file.{FileSystemNotFoundException, FileSystems, Files, Paths}
import java.util

import com.nawforce.common.api.{Name, TypeName}
import com.nawforce.common.cst.{Modifier, PUBLIC_MODIFIER}
import com.nawforce.common.finding.TypeRequest.TypeRequest
import com.nawforce.common.finding.{MissingType, WrongTypeArguments}
import com.nawforce.common.names.{DotName, Names, TypeNames, _}
import com.nawforce.common.org.PackageImpl
import com.nawforce.common.path.PathLike
import com.nawforce.common.types.core._
import com.nawforce.common.types.platform.{GenericPlatformTypeDeclaration, PlatformTypes}
import com.nawforce.common.types.synthetic.{CustomFieldDeclaration, CustomMethodDeclaration, CustomParameterDeclaration}

import scala.collection.JavaConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable

class PlatformTypeException(msg: String) extends Exception(msg)

/* Platform type declaration, a wrapper around a com.nawforce.platform Java classes */
case class PlatformTypeDeclaration(native: Any, outer: Option[PlatformTypeDeclaration])
  extends TypeDeclaration {

  val cls: java.lang.Class[_] = native.asInstanceOf[java.lang.Class[_]]

  override lazy val paths: Seq[PathLike] = Seq.empty
  override lazy val packageDeclaration: Option[PackageImpl] = None

  override lazy val name: Name = typeName.name
  override lazy val typeName: TypeName = PlatformTypeDeclaration.typeNameFromClass(cls, cls)
  override lazy val outerTypeName: Option[TypeName] = outer.map(_.typeName)
  override lazy val nature: Nature = {
    (cls.isEnum, cls.isInterface) match {
      case (true, _) => ENUM_NATURE
      case (_, true) => INTERFACE_NATURE
      case _ => CLASS_NATURE
    }
  }
  override val isComplete: Boolean = true

  override lazy val superClass: Option[TypeName] = getSuperClass

  protected def getSuperClass: Option[TypeName] = {
    if (cls.getSuperclass != null) {
      cls.getSuperclass.getCanonicalName match {
        case "java.lang.Object" => None
        case "java.lang.Enum" => None
        case _ => PlatformTypeDeclaration.typeNameOptional(cls.getSuperclass, cls)
      }
    } else {
      None
    }
  }

  override lazy val superClassDeclaration: Option[TypeDeclaration] = {
    superClass.flatMap(sc => PlatformTypes.get(sc, None).toOption)
  }

  override lazy val interfaces: Seq[TypeName] = getInterfaces

  override lazy val interfaceDeclarations: Seq[TypeDeclaration] = {
    getInterfaces.flatMap(id => PlatformTypes.get(id, None).toOption)
  }

  protected def getInterfaces: Seq[TypeName] = cls.getInterfaces.map(i => PlatformTypeDeclaration.typeNameFromClass(i, cls))

  override lazy val modifiers: Seq[Modifier] = PlatformModifiers.typeModifiers(cls.getModifiers, nature)

  override lazy val constructors: Seq[PlatformConstructor] = {
    cls.getConstructors.map(c => new PlatformConstructor(c, this))
  }

  override lazy val nestedTypes: Seq[PlatformTypeDeclaration] =
    cls.getClasses.map(nested => PlatformTypeDeclaration(nested, Some(this)))

  override lazy val blocks: Seq[BlockDeclaration] = Seq.empty

  override lazy val fields: Seq[FieldDeclaration] = getFields

  protected def getFields: Seq[PlatformField] = collectFields(cls).values.toSeq

  private def collectFields(cls: Class[_],
                            accum: mutable.Map[Name, PlatformField] = mutable.Map()): mutable.Map[Name, PlatformField] = {
    if (cls.getCanonicalName.startsWith(PlatformTypeDeclaration.platformPackage)) {
      cls.getDeclaredFields.filter(!_.isSynthetic).foreach(f => {
        val name = Name(f.getName)
        if (!accum.contains(name))
          accum.put(name, new PlatformField(f))
      })
      Option(cls.getSuperclass).foreach(superClass => collectFields(superClass, accum))
    }
    accum
  }

  override def findField(name: Name, staticContext: Option[Boolean]): Option[FieldDeclaration] = {
    if (isSObject) {
      findFieldSObject(name, staticContext)
    } else {
      super.findField(name, staticContext)
    }
  }

  override protected def findFieldSObject(name: Name, staticContext: Option[Boolean]): Option[FieldDeclaration] = {
    val field = super.findFieldSObject(name, staticContext)

    // If SObjectField if Id replace with SObjectFields over SObject so can access nested fields
    field.map(f =>{
      val isIdLike = f.name.value.endsWith("Id") && f.name.value.length>2
      if (isIdLike && staticContext.contains(true)) {
        val relationshipField = super.findFieldSObject(Name(f.name.value.dropRight(2)), staticContext)
        relationshipField match {
          case Some(CustomFieldDeclaration(_, TypeName(Names.SObjectFields$, Seq(sObject), Some(TypeNames.Internal)), _, _)) =>
            CustomFieldDeclaration(f.name, TypeNames.sObjectFields$(sObject), None, asStatic = true)
          case _ => f
        }
      } else {
        f
      }
    })
  }

  override lazy val methods: Seq[MethodDeclaration] = {
    val localMethods = getMethods
    nature match {
      case ENUM_NATURE => PlatformTypeDeclaration.enumMethods
      case _  => localMethods
    }
  }

  protected def getMethods: Seq[PlatformMethod] = {
    val localMethods = cls.getMethods.filter(
      _.getDeclaringClass.getCanonicalName.startsWith(PlatformTypeDeclaration.platformPackage))
    nature match {
      case ENUM_NATURE =>
        assert(localMethods.forall(m => m.getName == "values" || m.getName == "valueOf"),
          s"Enum $name has locally defined methods which are not supported in platform types")
        Seq()
      case _ =>
        localMethods.map(m => new PlatformMethod(m, this))
    }
  }

  override def validate(): Unit = {
    // Always valid because javac said so
  }
}

class PlatformField(val field: java.lang.reflect.Field) extends FieldDeclaration {
  override lazy val name: Name = Name(decodeName(field.getName))
  override lazy val typeName: TypeName =
    PlatformTypeDeclaration.typeNameFromType(field.getGenericType, field.getDeclaringClass)
  override lazy val modifiers: Seq[Modifier] = PlatformModifiers.fieldOrMethodModifiers(field.getModifiers)
  override lazy val readAccess: Modifier = PUBLIC_MODIFIER
  override lazy val writeAccess: Modifier = PUBLIC_MODIFIER
  override lazy val idTarget: Option[TypeName] = None

  def getGenericTypeName: TypeName =
    PlatformTypeDeclaration.typeNameFromType(field.getGenericType, field.getDeclaringClass)

  private def decodeName(name: String): String = {
    if (name.endsWith("$"))
      name.substring(0, name.length-1)
    else
      name
  }
}

class PlatformParameter(val parameter: java.lang.reflect.Parameter, val declaringClass: Class[_]) extends ParameterDeclaration {
  override lazy val name: Name = Name(parameter.getName)
  override lazy val typeName: TypeName = PlatformTypeDeclaration.typeNameFromType(parameter.getParameterizedType, declaringClass)

  def getGenericTypeName: TypeName =
    PlatformTypeDeclaration.typeNameFromType(parameter.getParameterizedType, declaringClass)

  override def toString: String = typeName.toString + " " + name.toString
}

class PlatformConstructor(ctor: java.lang.reflect.Constructor[_], typeDeclaration: PlatformTypeDeclaration)
  extends ConstructorDeclaration {
  lazy val modifiers: Seq[Modifier] = PlatformModifiers.methodModifiers(ctor.getModifiers, typeDeclaration.nature)
  lazy val parameters: Seq[PlatformParameter] = ctor.getParameters.map(p => new PlatformParameter(p, ctor.getDeclaringClass))
  def getDeclaringClass: Class[_] =  ctor.getDeclaringClass

  override def toString: String =
    modifiers.map(_.toString).mkString(" ") + " " + typeDeclaration.typeName.toString + "(" +
      parameters.map(_.toString).mkString(", ") + ")"
}

class PlatformMethod(val method: java.lang.reflect.Method, val typeDeclaration: PlatformTypeDeclaration)
  extends MethodDeclaration {
  lazy val name: Name = Name(decodeName(method.getName))
  lazy val typeName: TypeName = PlatformTypeDeclaration.typeNameFromType(method.getGenericReturnType, method.getDeclaringClass)
  lazy val modifiers: Seq[Modifier] = PlatformModifiers.methodModifiers(method.getModifiers, typeDeclaration.nature)
  lazy val parameters: Seq[ParameterDeclaration] = getParameters

  def getParameters: Seq[PlatformParameter] = method.getParameters.map(p => new PlatformParameter(p, method.getDeclaringClass))

  def getGenericTypeName: TypeName =
    PlatformTypeDeclaration.typeNameFromType(method.getGenericReturnType, method.getDeclaringClass)

  override def toString: String =
    modifiers.map(_.toString).mkString(" ") + " " + typeName.toString + " " + name.toString + "(" +
      parameters.map(_.toString).mkString(", ") + ")"

  private def decodeName(name: String): String = {
    if (name.endsWith("$"))
      name.substring(0, name.length-1)
    else
      name
  }
}

object PlatformTypeDeclaration {
  /* Java package prefix for platform types */
  private val platformPackage = "com.nawforce.platform"

  /* Cache of loaded platform declarations */
  private val declarationCache = mutable.Map[DotName, Option[PlatformTypeDeclaration]]()

  /* Get a Path that leads to platform classes */
  lazy val platformPackagePath: java.nio.file.Path = {
    val path = "/" + platformPackage.replaceAll("\\.", "/")
    val uri = classOf[PlatformTypeDeclaration].getResource(path).toURI
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

  /* Get a type, in general don't call this direct, use TypeRequest which will delegate here if
   * needed. If needed this will construct a GenericPlatformTypeDeclaration to specialise a
   * PlatformTypeDeclaration but it does not handle nested classes, see PlatformTypes for that.
   */
  def get(typeName: TypeName, from: Option[TypeDeclaration]): TypeRequest = {
    val tdOption = getDeclaration(typeName.asDotName)
    if (tdOption.isEmpty)
      return Left(MissingType(typeName))

    // Quick fail on wrong number of type variables
    val td = tdOption.get
    if (td.typeName.params.size != typeName.params.size)
      return Left(WrongTypeArguments(typeName, td.typeName.params.size))

    if (td.typeName.params.nonEmpty)
      GenericPlatformTypeDeclaration.get(typeName, from)
    else
      Right(td)
  }


  /* Get a declaration for a class from a DotName, in general don't call this direct, use TypeRequest which will
   * delegate here if needed. This does not handle generics or inner classes
   */
  def getDeclaration(name: DotName): Option[PlatformTypeDeclaration] = {
    declarationCache.getOrElseUpdate(name, {
      val matched = classNameMap.get(name)
      assert(matched.size < 2, s"Found multiple platform type matches for $name")
      matched.map(name => PlatformTypeDeclaration(
        classOf[PlatformTypeDeclaration].getClassLoader.loadClass(platformPackage + "." + name),
        None))
    })
  }

  /* Valid platform class names */
  lazy val classNames: Iterable[DotName] = classNameMap.keys

  /* All the namespaces - excluding our special ones! */
  lazy val namespaces: Set[Name] = classNameMap.keys.filter(_.isCompound).map(_.firstName)
    .filterNot(name => name == Names.SObjects || name == Names.Internal).toSet

  /* Map of class names, it's a map just to allow easy recovery of the original case by looking at value */
  private lazy val classNameMap: HashMap[DotName, DotName] = {
    val names = mutable.HashMap[DotName, DotName]()
    indexDir(platformPackagePath, DotName(Seq()), names)
    HashMap[DotName, DotName]() ++ names
  }

  /* Index .class files, we have to index to make sure we get natural case sensitive names, but also used
   * to re-map SObject so they appear in Schema namespace.
   */
  private def indexDir(path: java.nio.file.Path, prefix: DotName, accum: mutable.HashMap[DotName, DotName]): Unit = {
    Files.list(path).iterator.asScala.foreach(entry => {
      val filename = entry.getFileName.toString
      if (Files.isRegularFile(entry) && filename.endsWith(".class") &&
        (filename.endsWith("$.class") || !filename.contains('$'))) {
        val dotName = prefix.append(Name(filename.dropRight(".class".length)))
        if (dotName.names.head == Names.SObjects) {
          accum.put(DotName(Names.Schema +: dotName.names.tail), dotName)
        } else {
          accum.put(dotName, dotName)
        }
      }
      else if (Files.isDirectory(entry)) {
        val safeFilename = filename.replace("/", "").replace("\\", "")
        indexDir(entry, prefix.append(Name(safeFilename)), accum)
      }
    })
  }

  /* Create a TypeName from a Java class with null checking */
  private def typeNameOptional(cls: java.lang.Class[_], contextCls: java.lang.Class[_]): Option[TypeName] = {
    cls match {
      case null => None
      case _ => Some(typeNameFromClass(cls, contextCls))
    }
  }

  /* Create a TypeName from a Java Type, handles type variables as well as classes */
  def typeNameFromType(paramTypeNative: Any, contextClsNative: Any): TypeName = {
    val paramType = paramTypeNative.asInstanceOf[java.lang.reflect.Type]
    val contextCls = contextClsNative.asInstanceOf[java.lang.Class[_]]

    paramType match {
      case cls: Class[_] => PlatformTypeDeclaration.typeNameFromClass(cls, contextCls)
      case tv: java.lang.reflect.TypeVariable[_] => TypeName(Name(tv.getName))
      case pt: java.lang.reflect.ParameterizedType =>
        val cname = pt.getRawType.getTypeName
        assert(cname.startsWith(platformPackage), s"Reference to non-platform type $cname in ${contextCls.getCanonicalName}")
        val names = cname.drop(platformPackage.length + 1).split('.').map(n => Name(n)).reverse
        val params = pt.getActualTypeArguments.map(ta => typeNameFromType(ta, contextCls))
        TypeName(names).withParams(params)
    }
  }

  /* Create a TypeName from a Java class */
  def typeNameFromClass(cls: java.lang.Class[_], contextCls: java.lang.Class[_]): TypeName = {
    val cname = if (cls.isArray) cls.getComponentType.getCanonicalName else cls.getCanonicalName
    val typeName =
      if (cname == "java.lang.Object") {
        TypeNames.InternalObject
      } else if (cname == "void") {
        TypeNames.Void
      } else if (cname.startsWith(platformPackage+".SObjects")) {
        val names = cname.drop(platformPackage.length + 10).split('.').map(n => Name(n)).reverse
        val params = cls.getTypeParameters.map(tp => Name(tp.getName))
        TypeName(names :+ Names.Schema).withParams(params.toSeq.map(TypeName(_)))
      } else {
        assert(cname.startsWith(platformPackage), s"Reference to non-platform type $cname in ${contextCls.getCanonicalName}")
        val names = cname.drop(platformPackage.length + 1).split('.').map(n => Name(n)).reverse
        val params = cls.getTypeParameters.map(tp => Name(tp.getName))
        TypeName(names).withParams(params.toSeq.map(TypeName(_)))
      }
    if (cls.isArray)
      TypeNames.listOf(typeName)
    else
      typeName
  }

  /* Standard methods to be exposed on enums */
  private lazy val enumMethods: Seq[MethodDeclaration] =
    Seq(
      CustomMethodDeclaration(None, Name("name"), TypeNames.String, Seq()),
      CustomMethodDeclaration(None, Name("original"), TypeNames.Integer, Seq()),
      CustomMethodDeclaration(None, Name("values"), TypeNames.listOf(TypeNames.String), Seq(), asStatic = true),
      CustomMethodDeclaration(None, Name("equals"), TypeNames.Boolean,
        Seq(CustomParameterDeclaration(Name("other"), TypeNames.InternalObject))),
      CustomMethodDeclaration(None, Name("hashCode"), TypeNames.Integer, Seq())
    )
}


