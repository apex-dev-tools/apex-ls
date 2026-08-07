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

package io.github.apexdevtools.apexls

import com.nawforce.apexlink.api.Org
import com.nawforce.apexlink.rpc.OpenOptions
import com.nawforce.runtime.platform.Path

import scala.util.control.NonFatal

private[apexls] final case class BatchDispatchFailure(error: BatchError, status: Int)

private[apexls] trait BatchWorkspaceLoader {
  def load(options: BatchOptions): Either[BatchDispatchFailure, Org]
}

private[apexls] object DefaultBatchWorkspaceLoader extends BatchWorkspaceLoader {
  private final val StatusArgument = 1
  private final val StatusInternal = 3

  override def load(options: BatchOptions): Either[BatchDispatchFailure, Org] = {
    val workspace = Path(options.workspace)
    if (!workspace.exists || !workspace.isDirectory) {
      return Left(
        BatchDispatchFailure(
          BatchError("INVALID_SCOPE", s"Workspace '${options.workspace}' is not a directory"),
          StatusArgument
        )
      )
    }
    if (!workspace.join("sfdx-project.json").isFile) {
      return Left(
        BatchDispatchFailure(
          BatchError(
            "INVALID_SCOPE",
            s"Workspace '${options.workspace}' does not contain sfdx-project.json"
          ),
          StatusArgument
        )
      )
    }

    try {
      val openOptions = OpenOptions
        .default()
        .withLoggingLevel("none")
        .withAutoFlush(enabled = false)
        .withCache(options.cacheEnabled)
        .withCacheDirectory(options.cacheDirectory.getOrElse(""))
      val org = Org.newOrg(workspace, openOptions)
      if (org.getProjectConfig().isEmpty) {
        return Left(
          BatchDispatchFailure(
            BatchError("WORKSPACE_LOAD_FAILED", s"Unable to load workspace '${options.workspace}'"),
            StatusInternal
          )
        )
      }
      if (options.cacheEnabled) {
        org.flush()
      }
      Right(org)
    } catch {
      case NonFatal(exception) =>
        Left(
          BatchDispatchFailure(
            BatchError(
              "WORKSPACE_LOAD_FAILED",
              Option(exception.getMessage)
                .filter(_.nonEmpty)
                .getOrElse(exception.getClass.getSimpleName)
            ),
            StatusInternal
          )
        )
    }
  }
}
