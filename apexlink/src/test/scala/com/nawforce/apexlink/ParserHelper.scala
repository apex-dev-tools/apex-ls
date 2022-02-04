package com.nawforce.apexlink

import com.nawforce.apexlink.api.ServerOps

object ParserHelper {

  val ANTLR_PARSER   = "antlr"
  val OUTLINE_PARSER = "outlineparser"

  def setParser(requestedParser: Option[String] = None): Unit = {
    val parser =
      requestedParser.getOrElse(Option(System.getProperty("parser")).getOrElse(ANTLR_PARSER))

    parser.toLowerCase match {
      case OUTLINE_PARSER => ServerOps.setUseOutlineParser(true)
      case _              => ServerOps.setUseOutlineParser(false)
    }
  }
}
