/*
 Copyright (c) 2021 Kevin Jones, All rights reserved.
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

package com.nawforce.apexlink.plugins

import com.nawforce.apexlink.cst.{Block, ScopeVerifyContext}
import com.nawforce.apexlink.types.core.DependentType

import java.lang.reflect.Constructor

class PluginDispatcher(td: DependentType, plugins: Seq[Plugin], isLibrary: Boolean = false)
    extends Plugin(td, isLibrary) {

  override def onTypeValidated(): Seq[DependentType] = {
    plugins.flatMap(_.onTypeValidated())
  }

  override def onScopeValidated(isStatic: Boolean, context: ScopeVerifyContext): Unit = {
    plugins.foreach(_.onScopeValidated(isStatic, context))
  }
}

object PluginDispatcher {
  def apply(
    plugins: Seq[Constructor[_ <: Plugin]],
    td: DependentType,
    isLibrary: Boolean
  ): PluginDispatcher = {
    new PluginDispatcher(
      td,
      plugins.map(plugin => {
        try {
          plugin.newInstance(td, isLibrary)
        } catch {
          case e: Exception =>
            throw new RuntimeException(
              s"Failed to instantiate plugin ${plugin.getDeclaringClass.getSimpleName} with library flag $isLibrary",
              e
            )
        }
      }),
      isLibrary
    )
  }
}
