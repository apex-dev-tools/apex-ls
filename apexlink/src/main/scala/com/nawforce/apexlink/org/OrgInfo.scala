/*
 Copyright (c) 2022 Kevin Jones, All rights reserved.
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
package com.nawforce.apexlink.org

import com.nawforce.apexlink.org.OPM.OrgImpl
import com.nawforce.pkgforce.diagnostics.{Diagnostic, ERROR_CATEGORY, Issue}
import com.nawforce.pkgforce.path.PathLocation

import scala.util.DynamicVariable

/**
  * Access to the 'current' org, this should be deprecated now we have the OPM hierarchy.
  */
object OrgInfo {

  /** Access the in-scope Org */
  private[nawforce] val current: DynamicVariable[OrgImpl] = new DynamicVariable[OrgImpl](null)

  /** Log an issue against the in-scope org */
  private[nawforce] def log(issue: Issue): Unit = {
    if (issue.path != null)
      current.value.issueManager.add(issue)
  }

  /** Log a general error against the in-scope org */
  private[nawforce] def logError(pathLocation: PathLocation, message: String): Unit = {
    log(new Issue(pathLocation.path, Diagnostic(ERROR_CATEGORY, pathLocation.location, message)))
  }
}
