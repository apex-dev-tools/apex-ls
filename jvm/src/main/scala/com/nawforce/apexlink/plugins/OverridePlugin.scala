/*
 * Copyright (c) 2024 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.plugins

import com.nawforce.apexlink.cst._
import com.nawforce.apexlink.types.apex.ApexMethodLike
import com.nawforce.apexlink.types.core.DependentType
import com.nawforce.pkgforce.diagnostics.{Diagnostic, ERROR_CATEGORY, Issue}
import com.nawforce.pkgforce.modifiers._

import scala.annotation.unused

/** Provides plugin for generating unused warnings on a single type
  *
  * @param td type being handled by this plugin
  */
@unused
class OverridePlugin(td: DependentType) extends Plugin(td) {

  override def onEnumValidated(td: EnumDeclaration): Seq[DependentType] = Seq.empty

  override def onInterfaceValidated(td: InterfaceDeclaration): Seq[DependentType] = Seq.empty

  override def onClassValidated(td: ClassDeclaration): Seq[DependentType] = {
    // Ignore methods in test classes
    if (td.inTest)
      return Seq.empty

    // Hack: Analysis requires a methodMap as it establishes shadow relationships
    td.methodMap

    td.localMethods
      .flatMap {
        case am: ApexMethodLike if am.visibility == PRIVATE_MODIFIER && am.shadowedBy.nonEmpty =>
          Some(am)
        case _ => None
      }
      .map(method => {
        new Issue(
          method.location.path,
          Diagnostic(
            ERROR_CATEGORY,
            method.idLocation,
            s"Override of private abstract/virtual method will fail in v61, '${method.signature}'"
          )
        )
      })
      .foreach(td.module.pkg.org.issues.log)

    // No dependent processing needed, this is a standalone analysis
    Seq.empty
  }
}
