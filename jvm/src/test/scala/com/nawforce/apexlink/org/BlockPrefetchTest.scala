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

package com.nawforce.apexlink.org

import com.nawforce.apexlink.TestHelper
import com.nawforce.apexlink.api.{Org, ServerOps}
import com.nawforce.apexlink.rpc.OpenOptions
import com.nawforce.pkgforce.path.PathLike
import com.nawforce.runtime.FileSystemHelper
import org.scalatest.funsuite.AnyFunSuite

/** Prefetching method bodies must not change what is reported. The bodies are parsed away from the
  * validation that would normally log their syntax errors, so those errors have to be held until
  * the declaration is verified.
  */
class BlockPrefetchTest extends AnyFunSuite with TestHelper {

  private val classes = (1 to 8)
    .map(i =>
      s"Good$i.cls" ->
        s"public class Good$i {public void func() {Integer a = $i; System.debug(a);}}"
    )
    .toMap ++
    Map(
      "Bad.cls" ->
        """public class Bad {
          |  public void first() {Integer a = ; System.debug(a);}
          |  public void second() {Integer b = ; System.debug(b);}
          |}""".stripMargin
    )

  private def withPrefetch[T](threads: Int)(op: => T): T = {
    val previous = ServerOps.setBlockPrefetchThreads(threads)
    try op
    finally ServerOps.setBlockPrefetchThreads(previous)
  }

  /** All issues of a load, without the temporary directory that varies between them. */
  private def issuesWithPrefetch(threads: Int): Seq[String] = {
    withPrefetch(threads) {
      FileSystemHelper.run(classes) { root: PathLike =>
        val org = createOrg(root)
        org.issues
          .issuesForFileInternal(root.join("Bad.cls"))
          .map(issue =>
            s"${issue.diagnostic.category.name} ${issue.diagnostic.location.displayPosition} " +
              issue.diagnostic.message
          )
          .toSeq
      }
    }
  }

  test("body syntax error is reported when prefetching") {
    val expected = issuesWithPrefetch(0)
    assert(expected.count(_.startsWith("Syntax")) == 2)
    // The bad body must still be verified after its parse errors are reported
    assert(expected.exists(_.startsWith("Missing")))

    (1 to 3).foreach(_ => assert(issuesWithPrefetch(2) == expected))
    (1 to 3).foreach(_ => assert(issuesWithPrefetch(4) == expected))
  }

  test("thread count is limited to the supported values") {
    withPrefetch(0) {
      assert(ServerOps.setBlockPrefetchThreads(2) == 0)
      assert(ServerOps.getBlockPrefetchThreads == 2)
      assert(ServerOps.setBlockPrefetchThreads(3) == 2)
      assert(ServerOps.getBlockPrefetchThreads == 2)
      assert(ServerOps.setBlockPrefetchThreads(4) == 2)
      assert(ServerOps.setBlockPrefetchThreads(-1) == 4)
      assert(ServerOps.getBlockPrefetchThreads == 4)
    }
  }

  test("an org snapshots the effective process-wide setting") {
    withPrefetch(0) {
      FileSystemHelper.run(classes) { root: PathLike =>
        val defaultOrg = Org.newOrg(root, OpenOptions.default()).asInstanceOf[OPM.OrgImpl]
        assert(defaultOrg.blockPrefetchThreads == 0)

        val configuredOrg = Org
          .newOrg(root, OpenOptions.default().withBlockPrefetchThreads(2))
          .asInstanceOf[OPM.OrgImpl]
        assert(configuredOrg.blockPrefetchThreads == 2)

        ServerOps.setBlockPrefetchThreads(4)
        assert(configuredOrg.blockPrefetchThreads == 2)

        val inheritedOrg = Org.newOrg(root, OpenOptions.default()).asInstanceOf[OPM.OrgImpl]
        assert(inheritedOrg.blockPrefetchThreads == 4)
      }
    }
  }
}
