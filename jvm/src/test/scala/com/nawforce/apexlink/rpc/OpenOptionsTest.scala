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
package com.nawforce.apexlink.rpc

import io.github.shogowada.scala.jsonrpc.serializers.JSONRPCPickler.{read, write}
import org.scalatest.funsuite.AnyFunSuite

class OpenOptionsTest extends AnyFunSuite {

  test("withUnused remains source compatible alongside withUnusedOnError") {
    val options =
      OpenOptions.default().withUnused(enabled = false).withUnusedOnError(enabled = true)

    assert(options.unused.contains(false))
    assert(options.unusedOnError.contains(true))
  }

  test("legacy RPC options without blockPrefetchThreads retain the default") {
    val legacyJson = ujson.read(write(OpenOptions.default().withBlockPrefetchThreads(2)))
    legacyJson.obj.remove("blockPrefetchThreads")

    val options = read[OpenOptions](legacyJson.render())
    assert(options.blockPrefetchThreads.isEmpty)
  }

  test("legacy RPC options without unusedOnError retain the default") {
    val legacyJson = ujson.read(write(OpenOptions.default()))
    legacyJson.obj.remove("unusedOnError")

    val options = read[OpenOptions](legacyJson.render())
    assert(options.unusedOnError.isEmpty)
  }
}
