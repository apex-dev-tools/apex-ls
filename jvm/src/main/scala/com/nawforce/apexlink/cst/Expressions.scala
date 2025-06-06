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

package com.nawforce.apexlink.cst

import com.nawforce.apexlink.cst.Expression.mergeableSObjects
import com.nawforce.apexlink.diagnostics.IssueOps
import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.names.TypeNames._
import com.nawforce.apexlink.org.{OPM, Referenceable}
import com.nawforce.apexlink.types.apex.{ApexClassDeclaration, ApexConstructorLike}
import com.nawforce.apexlink.types.core.{FieldDeclaration, MethodDeclaration, TypeDeclaration}
import com.nawforce.apexlink.types.other.{AnyDeclaration, RecordSetDeclaration}
import com.nawforce.apexlink.types.platform.{PlatformTypeDeclaration, PlatformTypes}
import com.nawforce.apexlink.types.synthetic.CustomConstructorDeclaration
import com.nawforce.pkgforce.diagnostics.{ERROR_CATEGORY, Issue, WARNING_CATEGORY}
import com.nawforce.pkgforce.names.{EncodedName, Name, Names, TypeName}
import com.nawforce.pkgforce.path.{Locatable, Location, PathLocation}
import com.nawforce.runtime.parsers.CodeParser
import io.github.apexdevtools.apexparser.ApexParser._

import scala.collection.immutable.ArraySeq

/** Context used during expression verification to indicate focus & return state.
  *
  * Declaration provides the current context TypeDeclaration. isStatic=None is used as a marker that we have yet to
  * enter an explicit static/instance context as you find on the outermost expression used in an instance method. In
  * this state declaration == this & both static/instance resolution is allowed. If isStatic is set the specific
  * expression should become restricted to either static or instance resolution.
  *
  * @param isStatic static or instance or either context
  * @param declaration input/return type declaration, for return None is used to mean unknown/indeterminable
  * @param locatable position of code that generated this context to support introspection
  */
case class ExprContext(
  isStatic: Option[Boolean],
  declaration: Option[TypeDeclaration],
  locatable: Option[Locatable] = None
) {
  def isVoid = false

  def isDefined: Boolean =
    declaration.nonEmpty && !declaration.exists(_.isInstanceOf[AnyDeclaration])

  def typeDeclaration: TypeDeclaration = declaration.get

  def typeName: TypeName = declaration.get.typeName
}

/** ExprContext of type 'void'
  *
  * void is not commonly used in expressions, essentially just as method return type so we use simple
  * null-object style implementation for it.
  *
  * @param locatable position of code that generated this context to support introspection
  */
private class VoidExprContext(locatable: Option[Locatable])
    extends ExprContext(None, None, locatable) {
  override def isVoid = true
}

object VoidExprContext {
  def apply(locatable: Any): ExprContext = {
    locatable match {
      case l: Locatable => new VoidExprContext(Some(l))
      case _            => new VoidExprContext(None)
    }
  }
}

object ExprContext {
  val empty: ExprContext = new ExprContext(None, None)

  def apply(isStatic: Option[Boolean], declaration: TypeDeclaration): ExprContext = {
    new ExprContext(isStatic, Some(declaration))
  }

  /* We allow flex here on the locatable type as it a cross-cutting concern of many sorts of things */
  def apply(
    isStatic: Option[Boolean],
    declaration: Option[TypeDeclaration],
    locatable: Any
  ): ExprContext = {
    locatable match {
      case l: Locatable => new ExprContext(isStatic, declaration, Some(l))
      case _            => new ExprContext(isStatic, declaration, None)
    }
  }
}

/** base for any type of expression, provides helpers for type validation */
sealed abstract class Expression extends CST {
  def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext

  def verify(context: BlockVerifyContext): ExprContext = {
    val staticContext = if (context.isStatic) Some(true) else None
    verify(ExprContext(staticContext, context.thisType), new ExpressionVerifyContext(context))
  }

  /** Verify an expression result type is an exception
    *
    * @param context    verify context to use
    * @param prefix     for the log issue
    */
  def verifyIsExceptionInstance(
    context: BlockVerifyContext,
    prefix: String
  ): (Boolean, ExprContext) = {
    val verifyResult = verify(context)
    if (
      verifyResult.isDefined && (verifyResult.isStatic
        .contains(true) || !verifyResult.typeName.name.endsWith(Names.Exception))
    ) {
      val resultQualifier = if (verifyResult.isStatic.contains(true)) "type" else "instance"
      context.log(
        Issue(
          ERROR_CATEGORY,
          location,
          s"$prefix expression should return an Exception instance, not a '${verifyResult.typeName}' $resultQualifier"
        )
      )
      (false, verifyResult)
    } else {
      (true, verifyResult)
    }
  }

  /** Verify an expression result type matches a specific type logging an issue if not
    *
    * @param context    verify context to use
    * @param typeNames  set of permitted types
    * @param isStatic   check for static or instance value
    * @param prefix     for the log issue
    */
  def verifyIs(
    context: BlockVerifyContext,
    typeNames: Set[TypeName],
    isStatic: Boolean,
    prefix: String
  ): (Boolean, ExprContext) = {
    val verifyResult = verify(context)
    if (
      verifyResult.isDefined && (!verifyResult.isStatic.contains(isStatic) ||
        !typeNames.contains(verifyResult.typeName))
    ) {
      val resultQualifier = if (verifyResult.isStatic.contains(true)) "type" else "instance"
      val qualifier       = if (isStatic) "type" else "instance"
      val requiredTypes = if (typeNames.size == 1) {
        s"a '${typeNames.head}' $qualifier"
      } else {
        typeNames.map(n => s"'$n'").mkString("one of ", " or ", s" ${qualifier}s")
      }
      context.log(
        Issue(
          ERROR_CATEGORY,
          location,
          s"$prefix expression should return $requiredTypes, not a '${verifyResult.typeName}' $resultQualifier"
        )
      )
      (false, verifyResult)
    } else {
      (true, verifyResult)
    }
  }

  /** Verify an expression result type is an SObject or SObject List/RecordSet
    *
    * @param context   verify context to use
    * @param prefix    for the log issue
    */
  def verifyIsSObjectOrSObjectList(
    context: BlockVerifyContext,
    prefix: String
  ): (Boolean, ExprContext) = {
    val verifyResult = verify(context)
    if (
      verifyResult.isDefined &&
      (verifyResult.isStatic
        .contains(true) || !isSObjectOrSObjectList(verifyResult.typeName, context))
    ) {
      val resultQualifier = if (verifyResult.isStatic.contains(true)) "type" else "instance"
      context.log(
        Issue(
          ERROR_CATEGORY,
          location,
          s"$prefix expression should return an SObject or list of SObjects, not a '${verifyResult.typeName}' $resultQualifier"
        )
      )
      (false, verifyResult)
    } else {
      (true, verifyResult)
    }
  }

  private def isSObjectOrSObjectList(typeName: TypeName, context: VerifyContext): Boolean = {
    if (
      typeName == TypeNames.SObject || typeName == TypeNames.listOf(
        SObject
      ) || typeName == TypeNames.recordSetOf(SObject)
    )
      return true

    val sObjectTypeName =
      if (typeName.isList || typeName.isRecordSet) typeName.params.head else typeName
    context.getTypeFor(sObjectTypeName, context.thisType).toOption.forall(_.isSObject)
  }

  /** Verify an expression result type is an SObject suitable for merging. Only leads, contacts,
    * cases, and accounts can be merged. Returns typeName on success.
    *
    * @param context verify context to use
    * @param prefix  for the log issue
    */
  def verifyIsMergeableSObject(context: BlockVerifyContext, prefix: String): Option[TypeName] = {
    val verifyResult = verify(context)
    if (
      verifyResult.isDefined &&
      (verifyResult.isStatic.contains(true) ||
        !verifyResult.typeName.outer.contains(TypeName.Schema) ||
        !mergeableSObjects.contains(verifyResult.typeName.name))
    ) {
      val resultQualifier       = if (verifyResult.isStatic.contains(true)) "type" else "instance"
      val mergeableSObjectNames = mergeableSObjects.map(_.toString).mkString(" or ")
      context.log(
        Issue(
          ERROR_CATEGORY,
          location,
          s"$prefix expression should return a $mergeableSObjectNames SObject, not a '${verifyResult.typeName}' $resultQualifier"
        )
      )
      None
    } else {
      verifyResult.declaration.map(_.typeName)
    }
  }

  /** Verify an expression result type is an SObject or SObject List/RecordSet suitable for merging. Only
    * leads, contacts, cases, and accounts can be merged. Returns typeName on success.
    *
    * @param context verify context to use
    * @param prefix  for the log issue
    */
  def verifyIsMergeableSObjectOrSObjectList(
    context: BlockVerifyContext,
    prefix: String
  ): Option[TypeName] = {
    val verifyResult      = verify(context)
    val mergeableTypeName = getMergeableTypeNameFromSObjectOrSObjectList(verifyResult.typeName)
    if (
      verifyResult.isDefined && verifyResult.isStatic.contains(true) || mergeableTypeName.isEmpty
    ) {
      val resultQualifier       = if (verifyResult.isStatic.contains(true)) "type" else "instance"
      val mergeableSObjectNames = mergeableSObjects.map(_.toString).mkString(" or ")
      context.log(
        Issue(
          ERROR_CATEGORY,
          location,
          s"$prefix expression should return a $mergeableSObjectNames SObject or list of SObjects, not a '${verifyResult.typeName}' $resultQualifier"
        )
      )
      None
    } else {
      mergeableTypeName
    }
  }

  private def getMergeableTypeNameFromSObjectOrSObjectList(typeName: TypeName): Option[TypeName] = {
    val sObjectTypeName = {
      if (typeName.isList || typeName.isRecordSet) typeName.params.head else typeName
    }
    if (
      sObjectTypeName.outer.contains(
        TypeNames.Schema
      ) && sObjectTypeName.params.isEmpty && mergeableSObjects
        .contains(sObjectTypeName.name)
    ) Some(sObjectTypeName)
    else None
  }
}

final case class EmptyExpr() extends Expression {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    ExprContext.empty
  }
}

object DotExpression {
  def findField(
    name: Name,
    td: TypeDeclaration,
    module: OPM.Module,
    staticContext: Option[Boolean]
  ): Option[FieldDeclaration] = {
    val encodedName   = EncodedName(name)
    val namespaceName = encodedName.defaultNamespace(module.namespace)
    td.findField(namespaceName.fullName, staticContext)
      .orElse({
        if (encodedName != namespaceName) td.findField(encodedName.fullName, staticContext)
        else None
      })
  }
}

final case class DotExpressionWithId(expression: Expression, safeNavigation: Boolean, target: Id)
    extends Expression {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    assert(input.declaration.nonEmpty)

    // When we have a leading IdPrimary there are a couple of special cases to handle, we could with a better
    // understanding of how these are handled, likely it via some parser hack
    getInterceptPrimary
      .flatMap(primary => {
        val td       = input.declaration.get
        val localVar = context.isVar(primary.id.name)
        val field = DotExpression
          .findField(primary.id.name, td, context.module, input.isStatic)

        if (localVar.isEmpty && field.isEmpty) {
          interceptMissingId(input, context, primary.id)
        } else if (localVar.isEmpty && field.nonEmpty) {
          // Handle cases of shadowing
          interceptFoundField(input, context, primary.id, field.get)
        } else {
          None
        }
      })
      .map(result => context.saveResult(this)(result))
      .getOrElse(verifyInternal(input, context))
  }

  private def getInterceptPrimary: Option[IdPrimary] = {
    expression match {
      case PrimaryExpression(primary: IdPrimary) if !safeNavigation => Some(primary)
      case _                                                        => None
    }
  }

  private def interceptMissingId(
    input: ExprContext,
    context: ExpressionVerifyContext,
    id: Id
  ): Option[ExprContext] = {
    // It might be a namespace
    if (isNamespace(id.name, input.declaration.get)) {
      val typeName = TypeName(target.name, Nil, Some(TypeName(id.name))).intern
      val td       = context.getTypeAndAddDependency(typeName, context.thisType).toOption
      td.map(td =>
        context.saveResult(this) {
          ExprContext(isStatic = Some(true), Some(td), td)
        }
      )
    } else {
      TypeResolver(TypeName(id.name), context.module).toOption match {
        // It might be a static reference to an outer class that failed normal analysis due to class name shadowing
        // This occurs where say A has an inner of B and in C with an inner of A, we reference 'A.B' which is valid.
        case Some(td: ApexClassDeclaration) => verifyShadowedStatic(td, context)
        case _                              => None
      }
    }
  }

  private def interceptFoundField(
    input: ExprContext,
    context: ExpressionVerifyContext,
    id: Id,
    field: FieldDeclaration
  ): Option[ExprContext] = {
    // It may be a Platform reference shadowed by an existing field
    // e.g. String contact; contact.Name -> "String has no property Name"
    // Test the target field exists on the shadowed field or try fallback to the static
    val targetField = context
      .getTypeFor(field.typeName, input.declaration.get)
      .toOption
      .flatMap(f => DotExpression.findField(target.name, f, context.module, Some(false)))

    if (targetField.isEmpty) {
      TypeResolver(TypeName(id.name), context.module).toOption match {
        case Some(td: PlatformTypeDeclaration) => verifyShadowedStatic(td, context)
        case Some(td) if td.isSObject          => verifyShadowedStatic(td, context)
        case _                                 => None
      }
    } else {
      None
    }
  }

  private def verifyShadowedStatic(
    td: TypeDeclaration,
    context: ExpressionVerifyContext
  ): Option[ExprContext] = {
    val result = verifyWithId(ExprContext(isStatic = Some(true), td), context)
    if (result.isDefined) {
      context.addDependency(td)
      Some(result)
    } else {
      None
    }
  }

  private def verifyInternal(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    val inter = expression.verify(input, context)
    if (inter.isDefined) {
      if (inter.isStatic.contains(true) && safeNavigation) {
        context.logError(
          location,
          "Safe navigation operator (?.) can not be used on static references"
        )
        ExprContext.empty
      } else {
        context.saveResult(this) {
          verifyWithId(inter, context)
        }
      }
    } else {
      ExprContext.empty
    }
  }

  private def isNamespace(name: Name, td: TypeDeclaration): Boolean = {
    if (td.moduleDeclaration.nonEmpty)
      td.moduleDeclaration.get.pkg.namespaces.contains(name)
    else
      PlatformTypeDeclaration.namespaces.contains(name)
  }

  private def verifyWithId(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    val inputType = input.declaration.get

    val name = target.name
    val field: Option[FieldDeclaration] =
      DotExpression.findField(name, inputType, context.module, input.isStatic)
    if (field.nonEmpty) {
      context.addDependency(field.get)
      if (input.isStatic.contains(true) && !field.get.thisTypeIdOpt.contains(context.typeId))
        Referenceable.addReferencingLocation(inputType, field.get, location, context.thisType)
      else
        Referenceable.addReferencingLocation(field.get, location, context.thisType)
      val target = context.getTypeAndAddDependency(field.get.typeName, inputType).toOption
      if (target.isEmpty) {
        context.missingType(location, field.get.typeName)
        return ExprContext.empty
      }
      return ExprContext(isStatic = Some(false), target, field.get)
    }

    // TODO: Private/protected types?
    if (input.isStatic.contains(true)) {
      val nt = inputType.findLocalType(TypeName(name))
      if (nt.nonEmpty) {
        return ExprContext(isStatic = Some(true), nt.get)
      }
    }

    // Field is missing
    // ignore if we not have a complete type or the field is using a ghosted namespace
    if (inputType.isComplete && !context.module.isGhostedFieldName(name)) {
      if (inputType.isSObject || inputType.isInstanceOf[RecordSetDeclaration]) {
        context.log(IssueOps.unknownFieldOnSObject(location, name, inputType.typeName))
      } else {
        context.log(IssueOps.unknownFieldOrType(location, name, inputType.typeName))
      }
    }
    ExprContext.empty
  }
}

final case class DotExpressionWithMethod(
  expression: Expression,
  safeNavigation: Boolean,
  target: Option[MethodCallWithId]
) extends Expression {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    assert(input.declaration.nonEmpty)

    interceptAmbiguousMethodCall(input, context)
      .getOrElse({
        val inter = expression.verify(input, context)
        if (inter.isDefined) {
          if (inter.isStatic.contains(true) && safeNavigation) {
            context.logError(
              location,
              "Safe navigation operator (?.) can not be used on static references"
            )
            ExprContext.empty
          } else {
            target
              .map(target =>
                target.verify(location, inter.typeDeclaration, inter.isStatic, input, context)
              )
              .getOrElse(ExprContext.empty)
          }
        } else {
          // When we can't find method we should still verify args for dependency side effects
          target.map(target => target.arguments.map(_.verify(input, context)))
          ExprContext.empty
        }
      })
  }

  /** Intercept static method call to BusinessHours or Site as these operate on System.* rather than
    * Schema.* classes. This hack avoids having to pass additional context into platform type
    * loading to disambiguate.
    */
  private def interceptAmbiguousMethodCall(
    input: ExprContext,
    context: ExpressionVerifyContext
  ): Option[ExprContext] = {
    expression match {
      case PrimaryExpression(primary: IdPrimary)
          if DotExpressionWithMethod.isAmbiguousName.contains(primary.id.name) &&
            context.isVar(primary.id.name).isEmpty &&
            DotExpression
              .findField(primary.id.name, input.typeDeclaration, context.module, None)
              .isEmpty =>
        context
          .getTypeAndAddDependency(
            TypeName(primary.id.name, Nil, Some(TypeNames.System)),
            context.thisType
          )
          .toOption
          .flatMap(td =>
            target.map(target => target.verify(location, td, Some(true), input, context))
          )
      case _ => None
    }
  }
}

object DotExpressionWithMethod {
  private val isAmbiguousName = Set(Name("BusinessHours"), Name("Site"), Name("Network"))
}

final case class ArrayExpression(expression: Expression, arrayExpression: Expression)
    extends Expression {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {

    val index =
      arrayExpression.verify(ExprContext(input.isStatic, context.thisType), context)
    if (index.declaration.isEmpty)
      return ExprContext.empty
    if (index.typeName != TypeNames.Integer) {
      context.logError(
        arrayExpression.location,
        s"Array indexes must be Integers, found '${index.typeName}'"
      )
      return ExprContext.empty
    }

    val inter = expression.verify(input, context)
    if (!inter.isDefined)
      return ExprContext.empty

    val listType = inter.typeName.getArrayType
    if (inter.isStatic.contains(true) || listType.isEmpty) {
      context.logError(
        location,
        s"Only Lists can be de-referenced as an array, found '${inter.typeName}'"
      )
      return ExprContext.empty
    }

    context.getTypeAndAddDependency(listType.get, context.thisType) match {
      case Left(_) =>
        context.missingType(location, listType.get)
        ExprContext.empty
      case Right(td) =>
        ExprContext(isStatic = Some(false), td)
    }
  }
}

abstract class MethodCall extends Expression

final case class MethodCallWithId(target: Id, arguments: ArraySeq[Expression]) extends MethodCall {

  var cachedMethod: Option[MethodDeclaration] = None

  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    verify(location, input.typeDeclaration, input.isStatic, input, context)
  }

  def verify(
    location: PathLocation,
    callee: TypeDeclaration,
    staticContext: Option[Boolean],
    input: ExprContext,
    context: ExpressionVerifyContext
  ): ExprContext = {
    val argTypes = arguments.map(arg => {
      val argExpr = arg.verify(input, context)
      interceptFoundArg(input, context, arg, argExpr)
    })

    callee.findMethod(target.name, argTypes, staticContext, context) match {
      case Right(method) =>
        cachedMethod = Some(method)
        context.addDependency(method)
        if (staticContext.contains(true) && !method.thisTypeIdOpt.contains(context.typeId))
          Referenceable.addReferencingLocation(callee, method, location, context.thisType)
        else
          Referenceable.addReferencingLocation(method, location, context.thisType)
        if (method.typeName != TypeNames.Void) {
          val td = context.getTypeAndAddDependency(method.typeName, context.thisType)
          td match {
            case Left(error) =>
              if (!context.module.isGhostedType(method.typeName))
                context.log(error.asIssue(location))
              context.saveResult(this, target.location.location) {
                ExprContext(None, None, method)
              }
            case Right(td) =>
              context.saveResult(this, target.location.location) {
                ExprContext(isStatic = Some(false), Some(td), method)
              }
          }
        } else {
          context.saveResult(this, target.location.location) {
            VoidExprContext(method)
          }
        }

      case Left(err) =>
        if (callee.isComplete) {
          if (argTypes.contains(TypeNames.Any)) {
            context.log(Issue(WARNING_CATEGORY, location, s"$err, likely due to unknown type"))
          } else {
            context.logMissing(location, s"$err")
          }
        }
        ExprContext.empty
    }
  }

  /** Given a method declaration, if this method callout belongs to that method declaration then returns the location
    * if the callout. If the callout does not match to the method declaration, returns None.
    */
  def getTargetLocationForMethodCallOut(md: ApexMethodDeclaration): Option[Location] = {
    cachedMethod match {
      case Some(mdFromCallout: ApexMethodDeclaration) =>
        if (mdFromCallout.idPathLocation == md.idPathLocation) {
          Some(target.location.location)
        } else {
          None
        }
      case _ => None
    }
  }

  private def interceptFoundArg(
    input: ExprContext,
    context: ExpressionVerifyContext,
    arg: Expression,
    foundType: ExprContext
  ): TypeName = {
    if (foundType.isDefined) {
      // handle static type reference, expecting instance but most likely missing variable
      if (foundType.isStatic.contains(true)) {
        arg match {
          case PrimaryExpression(IdPrimary(id)) =>
            context.missingIdentifier(arg.location, input.typeName, id.name)
          case _ =>
        }
      }

      foundType.typeName
    } else {
      // If we failed to get argument type (maybe due to ghosting), use null as assignable to anything
      TypeNames.Any
    }
  }

}

final case class MethodCallCtor(isSuper: Boolean, arguments: ArraySeq[Expression])
    extends MethodCall {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    // Verify args so vars don't show as unused and map to typeNames
    val args = arguments.map(_.verify(input, context))
    if (args.forall(_.isDefined)) {
      val ctorSearchContext = if (isSuper) context.superType else Some(context.thisType)

      ctorSearchContext match {
        case Some(td) =>
          td.findConstructor(args.map(arg => arg.typeName), context) match {
            case Left(error) =>
              context.logError(location, error)
              ExprContext.empty
            case Right(ctor: ApexConstructorLike) =>
              Referenceable.addReferencingLocation(td, ctor, location, context.thisType)
              context.saveResult(this, ctor.idLocation) {
                ExprContext(Some(false), Some(td), ctor)
              }
            case Right(ctor: CustomConstructorDeclaration) =>
              context.saveResult(this, ctor.nameLocation) {
                ExprContext(Some(false), Some(td), ctor)
              }
            case Right(ctor) =>
              ExprContext(Some(false), Some(td), ctor)
          }
        case _ => ExprContext.empty
      }
    } else ExprContext.empty
  }
}

object MethodCall {
  def construct(from: MethodCallContext): MethodCall = {
    CodeParser
      .toScala(from.id())
      .map(id => {
        MethodCallWithId(Id.construct(id), expressions(from.expressionList())).withContext(from)
      })
      .getOrElse({
        MethodCallCtor(
          CodeParser.toScala(from.SUPER()).nonEmpty,
          expressions(from.expressionList())
        ).withContext(from)
      })
  }

  def construct(from: DotMethodCallContext): MethodCallWithId = {
    MethodCallWithId(Id.constructAny(from.anyId()), expressions(from.expressionList()))
      .withContext(from)
  }

  private def expressions(from: ExpressionListContext): ArraySeq[Expression] = {
    CodeParser
      .toScala(from)
      .map(el => CodeParser.toScala(el.expression()).map(e => Expression.construct(e)))
      .getOrElse(Expression.emptyExpressions)
  }
}

final case class NewExpression(creator: Creator) extends Expression {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    context.saveResult(this) {
      creator.verify(input, context)
    }
  }
}

final case class CastExpression(typeName: TypeName, expression: Expression) extends Expression {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    expression.verify(input, context)

    val castType = context.getTypeAndAddDependency(typeName, context.thisType).toOption
    if (castType.isEmpty) {
      context.missingType(location, typeName)
      ExprContext.empty
    } else {
      Referenceable.addReferencingLocation(castType.get, location, context.thisType)
      ExprContext(isStatic = Some(false), castType.get)
    }
  }
}

final case class SubExpression(expression: Expression) extends Expression {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    expression.verify(input, context)
  }
}

final case class PostfixExpression(expression: Expression, op: String) extends Expression {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    val inter = expression.verify(input, context)
    if (!inter.isDefined)
      return inter

    val td = inter.declaration.get
    td.typeName match {
      case TypeNames.Integer | TypeNames.Long | TypeNames.Decimal | TypeNames.Double
          if inter.isStatic.contains(false) =>
        inter
      case _ =>
        context.logError(
          location,
          s"Postfix increment/decrement is not supported on type '${td.typeName}'"
        )
        ExprContext.empty
    }
  }
}

final case class PrefixExpression(expression: Expression, op: String) extends Expression {

  def isAssignmentOperation: Boolean = op == "++" || op == "--"

  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    val inter = expression.verify(input, context)
    if (!inter.isDefined)
      return inter

    val td = inter.declaration.get
    td.typeName match {
      case TypeNames.Integer | TypeNames.Long | TypeNames.Decimal | TypeNames.Double
          if inter.isStatic.contains(false) =>
        inter
      case _ if inter.isStatic.contains(false) && op == "+" =>
        ExprContext(isStatic = Some(false), PlatformTypes.stringType)
      case _ =>
        context.logError(location, s"Prefix operations are not supported on type '${td.typeName}'")
        ExprContext.empty
    }
  }
}

final case class NegationExpression(expression: Expression, isBitwise: Boolean) extends Expression {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    val inter = expression.verify(input, context)
    if (!inter.isDefined)
      return inter

    val td = inter.declaration.get
    td.typeName match {
      case TypeNames.Boolean if !isBitwise && inter.isStatic.contains(false) => inter
      case TypeNames.Integer if isBitwise && inter.isStatic.contains(false)  => inter
      case TypeNames.Long if isBitwise && inter.isStatic.contains(false)     => inter
      case _ =>
        context.logError(location, s"Negation operations is not supported on type '${td.typeName}'")
        ExprContext.empty
    }
  }
}

final case class BinaryExpression(lhs: Expression, rhs: Expression, op: String) extends Expression {
  private lazy val operation: Operation = op match {
    case "="    => AssignmentOperation
    case "&&"   => LogicalOperation
    case "||"   => LogicalOperation
    case "??"   => ConditionalOperation
    case "=="   => EqualityOperation
    case "!="   => EqualityOperation
    case "<>"   => EqualityOperation
    case "==="  => ExactEqualityOperation
    case "!=="  => ExactEqualityOperation
    case "<"    => CompareOperation
    case ">"    => CompareOperation
    case "<="   => CompareOperation
    case ">="   => CompareOperation
    case "+"    => PlusOperation
    case "-"    => ArithmeticOperation
    case "*"    => ArithmeticOperation
    case "/"    => ArithmeticOperation
    case "+="   => ArithmeticAddSubtractAssignmentOperation
    case "-="   => ArithmeticAddSubtractAssignmentOperation
    case "*="   => ArithmeticMultiplyDivideAssignmentOperation
    case "/="   => ArithmeticMultiplyDivideAssignmentOperation
    case "&"    => BitwiseOperation
    case "|"    => BitwiseOperation
    case "^"    => BitwiseOperation
    case "<<"   => BitwiseOperation
    case ">>"   => BitwiseOperation
    case ">>>"  => BitwiseOperation
    case "^="   => BitwiseAssignmentOperation
    case "&="   => BitwiseAssignmentOperation
    case "|="   => BitwiseAssignmentOperation
    case "<<="  => BitwiseAssignmentOperation
    case ">>="  => BitwiseAssignmentOperation
    case ">>>=" => BitwiseAssignmentOperation
  }

  def isAssignmentOperation: Boolean = operation.isAssignmentOperation

  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    val leftInter  = lhs.verify(input, context)
    val rightInter = rhs.verify(input, context)

    if (!leftInter.isDefined || !rightInter.isDefined)
      return ExprContext.empty

    if (leftInter.isStatic.contains(true))
      context.logError(
        location,
        s"Expecting instance for operation, not type '${leftInter.typeName}'"
      )

    if (rightInter.isStatic.contains(true))
      context.logError(
        location,
        s"Expecting instance for operation, not type '${rightInter.typeName}'"
      )

    // TODO Remove temporary warning, add getReadOnlyError calls back to each operation verify
    operation match {
      case AssignmentOperation | BitwiseAssignmentOperation |
          ArithmeticAddSubtractAssignmentOperation | ArithmeticMultiplyDivideAssignmentOperation =>
        operation
          .getReadOnlyError(leftInter, context)
          .foreach(msg => context.log(Issue(WARNING_CATEGORY, location, msg)))
      case _ =>
    }

    operation.verify(leftInter, rightInter, op, context) match {
      case Left(error) =>
        context.logError(location, error)
        ExprContext.empty
      case Right(context) => context
    }
  }
}

final case class InstanceOfExpression(expression: Expression, typeName: TypeName)
    extends Expression {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    val instanceOfType = context.getTypeAndAddDependency(typeName, context.thisType).toOption
    if (instanceOfType.isEmpty)
      context.missingType(location, typeName)
    else
      Referenceable.addReferencingLocation(instanceOfType.get, location, context.thisType)
    expression.verify(input, context)
    ExprContext(isStatic = Some(false), PlatformTypes.booleanType)
  }
}

final case class QueryExpression(query: Expression, lhs: Expression, rhs: Expression)
    extends Expression {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    query.verify(input, context)
    val leftInter  = lhs.verify(input, context)
    val rightInter = rhs.verify(input, context)

    if (!leftInter.isDefined || !rightInter.isDefined)
      return ExprContext.empty

    ConditionalOperation.verify(leftInter, rightInter, "?", context) match {
      case Left(error) =>
        context.logError(location, error)
        ExprContext.empty
      case Right(context) => context
    }
  }
}

final case class PrimaryExpression(var primary: Primary) extends Expression {
  override def verify(input: ExprContext, context: ExpressionVerifyContext): ExprContext = {
    context.saveResult(this) {
      primary.verify(ExprContext(isStatic = input.isStatic, context.thisType), context)
    }
  }
}

object Expression {
  val emptyExpressions: ArraySeq[Expression] = ArraySeq()

  val mergeableSObjects: Set[Name] = Set("Lead", "Contact", "Case", "Account").map(Name(_))

  def construct(from: ExpressionContext): Expression = {
    val cst: Expression = {
      from match {
        case expr: DotExpressionContext =>
          CodeParser
            .toScala(expr.anyId())
            .map(id => {
              DotExpressionWithId(
                Expression.construct(expr.expression()),
                CodeParser.toScala(expr.DOT()).isEmpty,
                Id.constructAny(id)
              )
            })
            .getOrElse({
              DotExpressionWithMethod(
                Expression.construct(expr.expression()),
                CodeParser.toScala(expr.DOT()).isEmpty,
                CodeParser
                  .toScala(expr.dotMethodCall())
                  .map(method => {
                    MethodCall.construct(method)
                  })
              )
            })

        case expr: ArrayExpressionContext =>
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 2)
            ArrayExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1))
            )
          else
            EmptyExpr()

        case expr: MethodCallExpressionContext =>
          MethodCall.construct(expr.methodCall())

        case expr: NewExpressionContext =>
          NewExpression(Creator.construct(expr.creator()))

        case expr: CastExpressionContext =>
          CastExpression(
            TypeReference.construct(expr.typeRef()),
            Expression.construct(expr.expression())
          )

        case expr: SubExpressionContext =>
          SubExpression(Expression.construct(expr.expression()))

        case expr: PostOpExpressionContext =>
          val op = CodeParser
            .toScala(expr.INC())
            .orElse(CodeParser.toScala(expr.DEC()))
          PostfixExpression(Expression.construct(expr.expression()), CodeParser.getText(op.get))

        case expr: PreOpExpressionContext =>
          val op = CodeParser
            .toScala(expr.ADD())
            .orElse(CodeParser.toScala(expr.DEC()))
            .orElse(CodeParser.toScala(expr.INC()))
            .orElse(CodeParser.toScala(expr.SUB()))
          PrefixExpression(Expression.construct(expr.expression()), CodeParser.getText(op.get))

        case expr: NegExpressionContext =>
          val op = CodeParser
            .toScala(expr.BANG())
            .orElse(CodeParser.toScala(expr.TILDE()))
          NegationExpression(
            Expression.construct(expr.expression()),
            CodeParser.getText(op.get) == "~"
          )

        case expr: Arth1ExpressionContext =>
          val op = CodeParser
            .toScala(expr.DIV())
            .orElse(CodeParser.toScala(expr.MUL()))
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 2) {
            BinaryExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1)),
              CodeParser.getText(op.get)
            )
          } else {
            EmptyExpr()
          }

        case expr: Arth2ExpressionContext =>
          val op = CodeParser
            .toScala(expr.ADD())
            .orElse(CodeParser.toScala(expr.SUB()))
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 2) {
            BinaryExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1)),
              CodeParser.getText(op.get)
            )
          } else {
            EmptyExpr()
          }

        case expr: BitExpressionContext =>
          val gt = ">" * CodeParser.toScala(expr.GT()).size
          val lt = "<" * CodeParser.toScala(expr.LT()).size
          assert(gt.nonEmpty != lt.nonEmpty)
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 2) {
            BinaryExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1)),
              gt + lt
            )
          } else {
            EmptyExpr()
          }

        case expr: CmpExpressionContext =>
          val assign = CodeParser.toScala(expr.ASSIGN()).nonEmpty
          val op = CodeParser.getText(
            CodeParser.toScala(expr.GT()).orElse(CodeParser.toScala(expr.LT())).get
          )
          val opText = (assign, op) match {
            case (true, ">") => ">="
            case (true, "<") => "<="
            case (false, op) => op
            case _           => assert(false); ""
          }
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 2) {
            BinaryExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1)),
              opText
            )
          } else {
            EmptyExpr()
          }

        case expr: InstanceOfExpressionContext =>
          InstanceOfExpression(
            Expression.construct(expr.expression()),
            TypeReference.construct(expr.typeRef())
          )

        case expr: EqualityExpressionContext =>
          val op = CodeParser
            .toScala(expr.EQUAL())
            .orElse(CodeParser.toScala(expr.LESSANDGREATER()))
            .orElse(CodeParser.toScala(expr.NOTEQUAL()))
            .orElse(CodeParser.toScala(expr.TRIPLEEQUAL()))
            .orElse(CodeParser.toScala(expr.TRIPLENOTEQUAL()))
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 2) {
            BinaryExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1)),
              CodeParser.getText(op.get)
            )
          } else {
            EmptyExpr()
          }

        case expr: BitAndExpressionContext =>
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 2) {
            BinaryExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1)),
              "&"
            )
          } else {
            EmptyExpr()
          }

        case expr: BitNotExpressionContext =>
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 2) {
            BinaryExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1)),
              "^"
            )
          } else {
            EmptyExpr()
          }

        case expr: BitOrExpressionContext =>
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 2) {
            BinaryExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1)),
              "|"
            )
          } else {
            EmptyExpr()
          }

        case expr: LogAndExpressionContext =>
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 2) {
            BinaryExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1)),
              "&&"
            )
          } else {
            EmptyExpr()
          }

        case expr: LogOrExpressionContext =>
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 2) {
            BinaryExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1)),
              "||"
            )
          } else {
            EmptyExpr()
          }

        case expr: CoalExpressionContext =>
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 2) {
            BinaryExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1)),
              "??"
            )
          } else {
            EmptyExpr()
          }

        case expr: CondExpressionContext =>
          val expressions = CodeParser.toScala(expr.expression())
          if (expressions.length == 3) {
            QueryExpression(
              Expression.construct(expressions.head),
              Expression.construct(expressions(1)),
              Expression.construct(expressions(2))
            )
          } else {
            EmptyExpr()
          }

        case expr: AssignExpressionContext =>
          val op = CodeParser
            .toScala(expr.ADD_ASSIGN())
            .orElse(CodeParser.toScala(expr.AND_ASSIGN()))
            .orElse(CodeParser.toScala(expr.ASSIGN()))
            .orElse(CodeParser.toScala(expr.DIV_ASSIGN()))
            .orElse(CodeParser.toScala(expr.LSHIFT_ASSIGN()))
            .orElse(CodeParser.toScala(expr.MUL_ASSIGN()))
            .orElse(CodeParser.toScala(expr.OR_ASSIGN()))
            .orElse(CodeParser.toScala(expr.RSHIFT_ASSIGN()))
            .orElse(CodeParser.toScala(expr.SUB_ASSIGN()))
            .orElse(CodeParser.toScala(expr.URSHIFT_ASSIGN()))
            .orElse(CodeParser.toScala(expr.XOR_ASSIGN()))
          val expressions = CodeParser.toScala(expr.expression())
          BinaryExpression(
            Expression.construct(expressions.head),
            Expression.construct(expressions(1)),
            CodeParser.getText(op.get)
          )
        case expr: PrimaryExpressionContext =>
          PrimaryExpression(Primary.construct(expr.primary()))

        case _ => EmptyExpr()
      }
    }
    cst.withContext(from)
  }

  def construct(expression: ArraySeq[ExpressionContext]): ArraySeq[Expression] = {
    expression.map(x => Expression.construct(x))
  }
}

object Arguments {
  def construct(from: ArgumentsContext): ArraySeq[Expression] = {
    val el = CodeParser.toScala(from.expressionList())
    if (el.nonEmpty) {
      Expression.construct(CodeParser.toScala(el.get.expression()))
    } else {
      Expression.emptyExpressions
    }
  }
}
