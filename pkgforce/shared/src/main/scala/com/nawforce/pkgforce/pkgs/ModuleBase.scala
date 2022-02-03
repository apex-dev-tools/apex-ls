/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.pkgforce.pkgs

import com.nawforce.pkgforce.documents.DocumentIndex

abstract class ModuleBase[+Org] {
  val pkg: PackageBase[Org, ModuleBase[Org]]
  val index: DocumentIndex
  val dependents: Seq[ModuleBase[Org]]

  def freeze(): Unit

  val baseModules: Seq[ModuleBase[Org]] = dependents.reverse
}
