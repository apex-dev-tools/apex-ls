/*
 Copyright (c) 2026 Kevin Jones, All rights reserved.
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

import com.nawforce.apexlink.cst.AssignableSupport.{ScalarSObjectCoercion, recordSetConversion}
import com.nawforce.apexlink.names.TypeNames
import com.nawforce.apexlink.names.TypeNames.TypeNameUtils
import com.nawforce.apexlink.types.core.{FieldDeclaration, Parameters, TypeDeclaration}
import com.nawforce.pkgforce.diagnostics.{Issue, WARNING_CATEGORY}
import com.nawforce.pkgforce.names.TypeName

import scala.collection.immutable.ArraySeq

/** CST-facing support for RecordSet field access and call-site diagnostics. */
private[cst] object RecordSetSupport {

  /** A verified argument keeps its expression, resolved value, and overload type aligned. */
  final case class ArgumentValue(expression: Expression, value: ExprContext, typeName: TypeName)

  object ArgumentValue {
    def apply(expression: Expression, value: ExprContext): ArgumentValue =
      ArgumentValue(expression, value, value.declaration.map(_.typeName).getOrElse(TypeNames.Any))
  }

  /** Standard child relationships are exposed by the platform model as Lists, while custom child
    * relationships use the internal RecordSet type. Normalize instance access so both follow the
    * same Apex RecordSet conversion and overload-resolution rules.
    */
  def instanceFieldType(field: FieldDeclaration, receiver: TypeDeclaration): TypeName = {
    val fieldType = field.typeName
    if (
      receiver.isSObject && fieldType.isList && fieldType.params.size == 1 &&
      (fieldType.params.head == TypeNames.SObject || fieldType.params.head.outer.contains(
        TypeNames.Schema
      ))
    ) {
      TypeNames.recordSetOf(fieldType.params.head)
    } else {
      fieldType
    }
  }

  /** A child relationship RecordSet cannot be used as the receiver of another field access. Keep
    * this provenance check separate from direct SOQL RecordSets, whose fields may be accessed via
    * the platform's cardinality-dependent scalar conversion.
    */
  def isChildRelationshipReceiver(input: ExprContext): Boolean =
    input.typeName.isRecordSet && input.locatable.exists {
      case field: FieldDeclaration => field.typeName.isList || field.typeName.isRecordSet
      case _                       => false
    }

  /** Emit warnings after overload resolution, using expression-owned source locations. */
  def warnForParameters(
    parameters: Parameters,
    arguments: ArraySeq[ArgumentValue],
    context: ExpressionVerifyContext
  ): Unit = {
    parameters.parameters.zip(arguments).foreach { case (parameter, argument) =>
      val scalarCoercion =
        recordSetConversion(parameter.typeName, argument.typeName, context) match {
          case Some(_: ScalarSObjectCoercion) => true
          case _                              => false
        }
      if (argument.value.isDefined && scalarCoercion) {
        context.log(
          Issue(
            WARNING_CATEGORY,
            argument.expression.location,
            s"RecordSet coerced to '${parameter.typeName}'; runtime requires exactly one row"
          )
        )
      }
    }
  }
}
