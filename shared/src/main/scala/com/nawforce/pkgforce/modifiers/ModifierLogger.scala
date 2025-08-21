/*
 Copyright (c) 2017 Kevin Jones, All rights reserved.
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

package com.nawforce.pkgforce.modifiers

import com.nawforce.pkgforce.diagnostics.{Diagnostic, ERROR_CATEGORY, Issue, WARNING_CATEGORY}

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

/** Specialized logger for collecting issues during modifier parsing and validation.
  * Provides convenience methods for logging errors and warnings with LogEntryContext.
  */
class ModifierLogger {
  private var issueLog: ArrayBuffer[Issue] = _

  def isEmpty: Boolean = issueLog == null

  private def log(issue: Issue): Unit = {
    if (issueLog == null)
      issueLog = new ArrayBuffer[Issue]()
    issueLog.append(issue)
  }

  def issues: ArraySeq[Issue] = {
    if (isEmpty)
      Issue.emptyArray
    else
      ArraySeq.unsafeWrapArray(issueLog.toArray)
  }

  def logError(context: LogEntryContext, message: String): Unit = {
    log(Issue(context.path, Diagnostic(ERROR_CATEGORY, context.location, message)))
  }

  def logWarning(context: LogEntryContext, message: String): Unit = {
    log(Issue(context.path, Diagnostic(WARNING_CATEGORY, context.location, message)))
  }
}
