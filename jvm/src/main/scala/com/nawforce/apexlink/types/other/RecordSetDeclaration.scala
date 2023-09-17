/*
 * Copyright (c) 2023 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.types.other

import com.nawforce.apexlink.cst.VerifyContext
import com.nawforce.apexlink.finding.TypeResolver
import com.nawforce.apexlink.org.OPM.Module
import com.nawforce.apexlink.types.core.{
  BasicTypeDeclaration,
  FieldDeclaration,
  MethodDeclaration,
  TypeDeclaration
}
import com.nawforce.pkgforce.names.{Name, TypeName}
import com.nawforce.pkgforce.path.PathLike

import scala.collection.immutable.ArraySeq

/** RecordSet returned from a SOQL query.
  *
  * There are odd in that they appear to be lists but you can also access fields like they are SObjects. The platform
  * Internal.RecordSet provides the list functionality. Here we just override findField so field access works. This
  * type need injecting into the each module it is used from so that we locate up the SObject used in that module as
  * well.
  *
  * @param module containing module for type
  * @param baseDeclaration the platform type declaration this will override in the module
  */
class RecordSetDeclaration(module: Module, baseDeclaration: TypeDeclaration)
    extends BasicTypeDeclaration(PathLike.emptyPaths, module, baseDeclaration.typeName) {

  override lazy val isComplete: Boolean = {
    TypeResolver(typeName.params.head, module).toOption.exists(_.isComplete)
  }

  override def findField(name: Name, staticContext: Option[Boolean]): Option[FieldDeclaration] = {
    TypeResolver(typeName.params.head, module).toOption
      .flatMap(sObjectDeclaration => sObjectDeclaration.findField(name, staticContext))
  }

  override def findMethod(
    name: Name,
    params: ArraySeq[TypeName],
    staticContext: Option[Boolean],
    verifyContext: VerifyContext
  ): Either[String, MethodDeclaration] = {
    baseDeclaration.findMethod(name, params, staticContext, verifyContext)
  }

}
