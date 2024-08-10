/*
 * Copyright (c) 2024 Certinia Inc. All rights reserved.
 */
package com.nawforce.apexlink.plugins

import com.nawforce.apexlink.cst._
import com.nawforce.apexlink.plugins.OverridePlugin.extensibleModifiers
import com.nawforce.apexlink.types.apex.ApexMethodLike
import com.nawforce.apexlink.types.core.DependentType
import com.nawforce.pkgforce.diagnostics.{Diagnostic, ERROR_CATEGORY, Issue}
import com.nawforce.pkgforce.modifiers._

/** Plugin for detecting where a private method override is occurring in pre-v61 code. Flags both super class
  * and base class methods to make easier to spot.
  *
  * @param td type being handled by this plugin
  */
class OverridePlugin(td: DependentType) extends Plugin(td) {

  override def onEnumValidated(td: EnumDeclaration): Seq[DependentType] = Seq.empty

  override def onInterfaceValidated(td: InterfaceDeclaration): Seq[DependentType] = Seq.empty

  override def onClassValidated(td: ClassDeclaration): Seq[DependentType] = {
    // Bail early if not extending or virtual/abstract
    if (td.modifiers.intersect(extensibleModifiers).isEmpty && td.superClass.isEmpty)
      return Seq.empty

    // Hack: Analysis requires a methodMap as it establishes shadow relationships
    td.methodMap

    td.localMethods
      .collect { case m: ApexMethodLike => m }
      .foreach(method => {
        // This private method is being overridden
        if (method.visibility == PRIVATE_MODIFIER && method.shadowedBy.nonEmpty) {
          val overrides = method.shadowedBy.flatMap(_.thisTypeIdOpt).map(_.toString).mkString(", ")
          td.module.pkg.org.issues.log(
            new Issue(
              method.location.path,
              Diagnostic(
                ERROR_CATEGORY,
                method.idLocation,
                s"The overrides of this private method will fail in v61, see $overrides"
              )
            )
          )
        }

        // This method is overriding a private method
        findPrivateShadow(method)
          .foreach(shadow => {
            td.module.pkg.org.issues.log(
              new Issue(
                method.location.path,
                Diagnostic(
                  ERROR_CATEGORY,
                  method.idLocation,
                  s"This override of a private method will fail in v61, see ${shadow.location.toString}"
                )
              )
            )
          })
      })

    // No dependent processing needed, this is a standalone analysis
    Seq.empty
  }

  private def findPrivateShadow(method: ApexMethodLike): Option[ApexMethodLike] = {
    val shadows       = method.shadows.collect { case m: ApexMethodLike => m }
    val privateShadow = shadows.find(_.visibility == PRIVATE_MODIFIER)
    privateShadow.orElse(shadows.collectFirst(Function.unlift(findPrivateShadow)))
  }
}

object OverridePlugin {
  final val extensibleModifiers = Seq(ABSTRACT_MODIFIER, VIRTUAL_MODIFIER)
}
