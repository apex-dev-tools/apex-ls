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

import com.nawforce.apexlink.api.ServerOps
import com.nawforce.pkgforce.diagnostics.LoggerOps
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.platform.Environment

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

private[apexls] trait BatchCommandTestSupport {
  protected def project(namespace: String, directories: Seq[String]): String = {
    val packageDirectories = directories.map(path => s"""{"path":"$path"}""").mkString(",")
    s"""{"packageDirectories":[$packageDirectories],"namespace":"$namespace"}"""
  }

  protected def fileName(path: String): String = java.nio.file.Paths.get(path).getFileName.toString

  protected def invoke(
    workspace: PathLike,
    command: String,
    cacheEnabled: Boolean,
    commandArguments: String*
  ): Invocation = {
    val cacheArguments =
      if (cacheEnabled) Seq("--cache-dir", workspace.join("cache").toString) else Seq("--no-cache")
    invokeRaw(
      (Seq(command, "--workspace", workspace.toString) ++ cacheArguments ++ commandArguments): _*
    )
  }

  protected def invokeRaw(args: String*): Invocation = {
    val stdout                 = new ByteArrayOutputStream()
    val stderr                 = new ByteArrayOutputStream()
    val originalCacheDirectory = Environment.getCacheDirOverride
    val originalAutoFlush      = ServerOps.isAutoFlushEnabled
    val originalParser         = ServerOps.getCurrentParser
    val originalLoggingLevel   = LoggerOps.getLoggingLevel
    val status                 = Batch.run(args.toArray, stdout, stderr)
    val stdoutText             = new String(stdout.toByteArray, StandardCharsets.UTF_8)

    assert(Environment.getCacheDirOverride == originalCacheDirectory)
    assert(ServerOps.isAutoFlushEnabled == originalAutoFlush)
    assert(ServerOps.getCurrentParser == originalParser)
    assert(LoggerOps.getLoggingLevel == originalLoggingLevel)
    assert(stdoutText.count(_ == '\n') == 1)
    new Invocation(
      status,
      stdoutText,
      new String(stderr.toByteArray, StandardCharsets.UTF_8),
      ujson.read(stdoutText)
    )
  }

  protected final class Invocation(
    val status: Int,
    val stdout: String,
    val stderr: String,
    val json: ujson.Value
  )
}
