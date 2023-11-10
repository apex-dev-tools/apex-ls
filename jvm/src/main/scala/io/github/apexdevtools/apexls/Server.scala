/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */

package io.github.apexdevtools.apexls

import com.nawforce.apexlink.rpc.RPCServer

/** Start apex-ls as an RPC server.
  *
  * RPC messages are passed over stdin/stdout. See @OrgAPI for details of messages supported.
  */
object Server {
  def main(args: Array[String]): Unit = {
    try {
      new RPCServer().run()
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        System.exit(-1)
    }
  }
}
