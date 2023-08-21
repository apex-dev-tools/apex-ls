/*
 * Copyright (c) 2022 FinancialForce.com, inc. All rights reserved.
 */
package com.nawforce.apexlink

import com.nawforce.apexlink.api.{ANTLRParser, OutlineParserSingleThreaded, ServerOps}

object ParserHelper {

  def setParser(requestedParser: Option[String] = None): Unit = {
    val parser =
      requestedParser.getOrElse(
        Option(System.getProperty("parser")).getOrElse(ANTLRParser.shortName)
      )

    parser.toLowerCase match {
      case OutlineParserSingleThreaded.shortName =>
        ServerOps.setCurrentParser(OutlineParserSingleThreaded)
      case _ => ServerOps.setCurrentParser(ANTLRParser)
    }
  }
}
