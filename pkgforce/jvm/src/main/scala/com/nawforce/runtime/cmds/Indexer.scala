/*
 Copyright (c) 2020 Kevin Jones, All rights reserved.
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
package com.nawforce.runtime.cmds

import com.nawforce.pkgforce.api.MDIndex
import com.nawforce.runtime.platform.Path

object Indexer {

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      println("Missing argument, expecting directory to index")
      System.exit(-1)
    }

    if (args.length > 1) {
      println("Too many arguments, expecting directory to index")
      System.exit(-1)
    }

    val start = System.currentTimeMillis()
    val index = new MDIndex(Path(args(0)))
    val stop  = System.currentTimeMillis()

    index
      .issuesForFiles(null, false, 100)
      .foreach(issue => println(issue.asString))

    println(s"Index loading took ${stop - start}ms")

    Thread.sleep(30 * 1000)
  }
}
