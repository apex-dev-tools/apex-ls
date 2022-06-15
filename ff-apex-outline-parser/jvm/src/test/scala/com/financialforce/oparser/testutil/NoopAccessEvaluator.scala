/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser.testutil

import apex.jorje.semantic.compiler.sfdc.{AccessEvaluator, PlaceholderOrgPerm}
import apex.jorje.semantic.compiler.{Namespace, StructuredVersion}
import apex.jorje.semantic.symbol.`type`.{SObjectTypeInfo, TypeInfo}

/**
  * Empty classes to provide a concrete implementation for the apex compiler
  */
class NoopAccessEvaluator extends AccessEvaluator {
  override def hasPermission(placeholderOrgPerm: PlaceholderOrgPerm): Boolean = false

  override def hasPermissionForPermGuard(namespace: Namespace, s: String): Boolean = false

  override def hasPrivateApi: Boolean = false

  override def hasLocalizedTranslation: Boolean = false

  override def hasInternalSfdc: Boolean = false

  override def isTrustedApplication(typeInfo: TypeInfo): Boolean = false

  override def isReservedNamespace(namespace: Namespace): Boolean = false

  override def isReservedNamespace(namespace: Namespace, b: Boolean): Boolean = false

  override def isAccessibleSystemNamespace(namespace: Namespace): Boolean = false

  override def isAccessibleOrTrustedNamespace(namespace: Namespace): Boolean = false

  override def isNamespaceGuardNamespace(namespace: Namespace): Boolean = false

  override def isRunningTests: Boolean = false

  override def isReallyRunningTests: Boolean = false

  override def isSfdc: Boolean = false

  override def hasApexParameterizedTypes: Boolean = false

  override def isValidPackageVersion(
    namespace: Namespace,
    structuredVersion: StructuredVersion
  ): Boolean = false

  override def isManagedPackageInstalled(namespace: Namespace): Boolean = false

  override def isSetupEntityVisibleToType(
    sObjectTypeInfo: SObjectTypeInfo,
    typeInfo: TypeInfo
  ): Boolean = false

  override def hasConnectDeserializer(typeInfo: TypeInfo): Boolean = false

  override def hasRemoteAction(typeInfo: TypeInfo): Boolean = false

  override def hasRemoteActionPerm: Boolean = false

  override def isGlobalComponent(typeInfo: TypeInfo): Boolean = false

  override def useTestValueForAnonymousScriptLengthLimit(): Boolean = false

  override def isSecondGenerationPackagingNamespace(namespace: Namespace): Boolean = false

  override def hasNamespaceGuardedAccess(namespace: Namespace, s: String): Boolean = false

  override def doesLightningWebComponentExist(s: String): Boolean = false
}

object NoopAccessEvaluator {

  def apply(): NoopAccessEvaluator = {
    new NoopAccessEvaluator()
  }
}
