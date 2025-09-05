/*
 Copyright (c) 2025 Kevin Jones, All rights reserved.
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
package com.nawforce.pkgforce.sfdx

/** Enumeration for ForceIgnore implementation versions */
sealed abstract class ForceIgnoreVersion(val value: String)

object ForceIgnoreVersion {

  /** Legacy ForceIgnore implementation */
  case object V1 extends ForceIgnoreVersion("v1")

  /** ForceIgnoreV2 with exact node-ignore 5.3.2 compatibility */
  case object V2 extends ForceIgnoreVersion("v2")

  /** Parse string value to enum */
  def fromString(value: String): Option[ForceIgnoreVersion] = value match {
    case "v1" => Some(V1)
    case "v2" => Some(V2)
    case _    => None
  }

  /** Default version */
  val default: ForceIgnoreVersion = V2

  /** All valid versions */
  val all: Seq[ForceIgnoreVersion] = Seq(V1, V2)

  /** All valid string values */
  val validValues: Seq[String] = all.map(_.value)
}
