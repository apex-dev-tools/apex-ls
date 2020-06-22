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
package com.nawforce.common.types.apex

import com.nawforce.common.api._
import com.nawforce.common.cst._
import com.nawforce.common.documents._
import com.nawforce.common.memory.Monitor
import com.nawforce.common.names.{TypeNames, _}
import com.nawforce.common.org.{OrgImpl, PackageImpl}
import com.nawforce.common.path.PathLike
import com.nawforce.common.types.apex
import com.nawforce.common.types.core._
import com.nawforce.common.types.other.{Component, Interview, Label, Page}
import com.nawforce.runtime.parsers.ApexParser.{ModifierContext, TypeDeclarationContext}
import com.nawforce.runtime.parsers.{CodeParser, Source, SourceData}
import upickle.default.writeBinary

import scala.collection.mutable

/* Apex type declaration, a wrapper around the Apex parser output. This is the base for classes, interfaces & enums*/
abstract class FullDeclaration(val source: Source, val pkg: PackageImpl, val outerTypeName: Option[TypeName],
                               val id: Id, _modifiers: ModifierResults,
                               val superClass: Option[TypeName], val interfaces: Array[TypeName],
                               val bodyDeclarations: Array[ClassBodyDeclaration])
  extends ClassBodyDeclaration(_modifiers) with ApexClassDeclaration with ApexFullDeclaration {

  lazy val sourceHash: Int = source.hash
  override val path: PathLike = source.path
  override val paths: Array[PathLike] = Array(path)
  override val packageDeclaration: Option[PackageImpl] = Some(pkg)

  override val nameLocation: LocationImpl = id.location
  override val name: Name = id.name
  override val nature: Nature
  var flushedToCache = false

  override lazy val nestedTypes: Array[TypeDeclaration] = _nestedTypes.asInstanceOf[Array[TypeDeclaration]]
  private lazy val _nestedTypes: Array[FullDeclaration] = {
    bodyDeclarations.flatMap {
      case x: FullDeclaration => Some(x)
      case _ => None
    }
  }

  override lazy val blocks: Array[BlockDeclaration] = _blocks.asInstanceOf[Array[BlockDeclaration]]
  private lazy val _blocks: Array[ApexInitialiserBlock] = {
    bodyDeclarations.flatMap {
      case x: ApexInitialiserBlock => Some(x)
      case _ => None
    }
  }

  lazy val localFields: Array[ApexFieldLike] = {
    bodyDeclarations.flatMap {
      case x: ApexFieldDeclaration => Some(x)
      case x: ApexPropertyDeclaration => Some(x)
      case _ => None
    }
  }

  override lazy val constructors: Array[ConstructorDeclaration] = _constructors.asInstanceOf[Array[ConstructorDeclaration]]
  private lazy val _constructors: Array[ApexConstructorDeclaration] = {
    bodyDeclarations.flatMap {
      case x: ApexConstructorDeclaration => Some(x)
      case _ => None
    }
  }

  override lazy val localMethods: Array[MethodDeclaration] = _localMethods.asInstanceOf[Array[MethodDeclaration]]
  lazy val _localMethods: Array[ApexVisibleMethodLike] = {
    bodyDeclarations.flatMap({
      case m: ApexVisibleMethodLike => Some(m)
      case _ => None
    })
  }

  override def flush(pc: ParsedCache, context: PackageContext): Unit = {
    if (!flushedToCache) {
      val diagnostics = pkg.org.issues.getDiagnostics(location.path).toArray
      pc.upsert(source.asUTF8, writeBinary(ApexSummary(summary(shapeOnly = false), diagnostics)), context)
      flushedToCache = true
    }
  }

  override def propagateAllDependencies(): Unit = {
    // Not needed, dependencies are propagated by default during validation
  }

  def validate(withPropagation: Boolean): Unit = {
    ServerOps.debugTime(s"Validated ${location.path}") {
      // Validate inside a parsing context as LazyBlock may call parser
      CST.sourceContext.withValue(Some(source)) {
        val context = new TypeVerifyContext(None, this, withPropagation)
        modifierIssues.foreach(context.log)
        verify(context)
        if (withPropagation)
          propagateOuterDependencies()

        // Re-validation may update diagnostics which now need flushing
        flushedToCache = false
      }
    }
  }

  protected def verify(context: TypeVerifyContext): Unit = {
    // Check super class is visible
    superClassDeclaration.foreach(context.addDependency)
    if (superClass.nonEmpty) {
      if (superClassDeclaration.isEmpty) {
        context.missingType(id.location, superClass.get)
      } else if (superClassDeclaration.get.nature != CLASS_NATURE) {
        OrgImpl.logError(id.location, s"Parent type '${superClass.get.asDotName}' must be a class")
      } else if (superClassDeclaration.get.modifiers.intersect(Seq(VIRTUAL_MODIFIER, ABSTRACT_MODIFIER)).isEmpty) {
        OrgImpl.logError(id.location, s"Parent class '${superClass.get.asDotName}' must be declared virtual or abstract")
      }
    }

    // Check for duplicate nested types
    val duplicateNestedType = (this +: nestedTypes).toSeq.groupBy(_.name).collect { case (_, Seq(_, y, _*)) => y }
    duplicateNestedType.foreach(td =>
      OrgImpl.logError(td.asInstanceOf[apex.FullDeclaration].location, s"Duplicate type name '${td.name.toString}'"))

    // Check interfaces are visible
    interfaces.foreach(interface => {
      val td = context.getTypeAndAddDependency(interface, context.thisType).toOption
      if (td.isEmpty) {
        if (!context.pkg.isGhostedType(interface))
          context.missingType(id.location, interface)
      } else if (td.get.nature != INTERFACE_NATURE)
        OrgImpl.logError(id.location, s"Type '${interface.toString}' must be an interface")
    })

    // Detail check each body declaration
    bodyDeclarations.foreach(bd => bd.validate(new BodyDeclarationVerifyContext(context, bd)))

    // Log dependencies logged against this context
    setDepends(context.dependencies)
  }

  override def collectDependenciesByTypeName(dependsOn: mutable.Set[TypeId]): Unit = {
    val dependents = mutable.Set[Dependent]()
    collectDependencies(dependents)
    dependents.foreach {
      case ad: ApexClassDeclaration =>
        dependsOn.add(ad.outerTypeId)
      case _: Label =>
        dependsOn.add(TypeId(pkg, TypeNames.Label))
      case _: Interview =>
        dependsOn.add(TypeId(pkg, TypeNames.Interview))
      case _: Page =>
        dependsOn.add(TypeId(pkg, TypeNames.Page))
      case _: Component =>
        dependsOn.add(TypeId(pkg, TypeNames.Component))
      case _ => ()
    }
  }

  override def collectDependencies(dependsOn: mutable.Set[Dependent]): Unit = {
    super.collectDependencies(dependsOn)
    bodyDeclarations.foreach(_.collectDependencies(dependsOn))
  }

  override def summary: TypeSummary = {
    summary(shapeOnly = false)
  }

  // Override to avoid super class access (use local fields & methods) & provide location information
  override def summary(shapeOnly: Boolean): TypeSummary = {
    TypeSummary (
      if (shapeOnly) 0 else sourceHash,
      if (shapeOnly) None else Some(new RangeLocation(id.location.start.toPosition, id.location.end.toPosition)),
      name.toString,
      typeName,
      nature.value,
      modifiers.map(_.toString).sorted,
      superClass,
      interfaces,
      _blocks.map(_.summary(shapeOnly)),
      localFields.map(_.summary(shapeOnly)).sortBy(_.name),
      _constructors.map(_.summary(shapeOnly)).sortBy(_.parameters.length),
      _localMethods.map(_.summary(shapeOnly)).sortBy(_.name),
      _nestedTypes.map(_.summary(shapeOnly)).sortBy(_.name),
      if (shapeOnly) Array.empty else dependencySummary()
    )
  }
}

object FullDeclaration {
  def create(pkg: PackageImpl, path: PathLike, data: SourceData): Option[FullDeclaration] = {
    val parser = CodeParser(path, data)
    parser.parseClass() match {
      case Left(err) =>
        OrgImpl.logError(LineLocationImpl(path.toString, err.line), err.message)
        None
      case Right(cu) =>
        Some(CompilationUnit.construct(parser, pkg, cu).typeDeclaration)
    }
  }

  def construct(parser: CodeParser, pkg: PackageImpl,
                outerTypeName: Option[TypeName], typeDecl: TypeDeclarationContext)
      : FullDeclaration = {

    val modifiers: Seq[ModifierContext] = CodeParser.toScala(typeDecl.modifier())
    val isOuter = outerTypeName.isEmpty

    val cst = CodeParser.toScala(typeDecl.classDeclaration())
      .map(cd => ClassDeclaration.construct(parser, pkg, outerTypeName,
        ApexModifiers.classModifiers(parser, modifiers, outer = isOuter, cd.id()),
        cd)
      )
    .orElse(CodeParser.toScala(typeDecl.interfaceDeclaration())
      .map(id => InterfaceDeclaration.construct(parser, pkg, outerTypeName,
        ApexModifiers.interfaceModifiers(parser, modifiers, outer = isOuter, id.id()),
        id)
      ))
    .orElse(CodeParser.toScala(typeDecl.enumDeclaration())
      .map(ed => EnumDeclaration.construct(parser, pkg, outerTypeName,
        ApexModifiers.enumModifiers(parser, modifiers, outer = isOuter, ed.id()),
        ed)
      ))

    if (cst.isEmpty)
      throw new CSTException()
    else {
       Monitor.push(cst.get)
       cst.get.withContext(typeDecl)
    }
  }
}
