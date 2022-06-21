/*
 * Copyright (c) 2021 FinancialForce.com, inc. All rights reserved.
 */
package com.financialforce.oparser

import com.financialforce.types.base.Location

class Buffer(backing: String) {

  private var startLine       = 0
  private var startLineOffset = 0
  private var endLine         = 0
  private var endLineOffset   = 0
  private var startCharOffset = 0
  private var endCharOffset   = 0
  private var startByteOffset = 0
  private var endByteOffset   = 0
  private var capturing       = false

  private val emptyString = ""

  def captured(): (String, Location) = {
    (
      if (capturing) backing.substring(startCharOffset, endCharOffset + 1)
      else emptyString,
      Location(startLine, startLineOffset, startByteOffset, endLine, endLineOffset, endByteOffset)
    )
  }

  def clear(): Unit = {
    startCharOffset = 0
    endCharOffset = 0
    startLine = 0
    startLineOffset = 0
    endLine = 0
    endLineOffset = 0
    startByteOffset = 0
    endByteOffset = 0
    capturing = false
  }

  def append(byteOffset: Int, charOffset: Int, line: Int, lineOffset: Int): Unit = {
    if (!capturing) {
      capturing = true
      startByteOffset = byteOffset
      startCharOffset = charOffset
      startLine = line
      startLineOffset = lineOffset
    }
    endByteOffset = byteOffset
    endCharOffset = charOffset
    endLine = line
    endLineOffset = lineOffset
  }

}
