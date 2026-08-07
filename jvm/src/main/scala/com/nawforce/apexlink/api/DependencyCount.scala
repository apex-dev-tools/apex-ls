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

package com.nawforce.apexlink.api

/** Dependency information for an Apex class or trigger.
  *
  * A left maximum represents either no configured maximum (`None`) or an error reading or parsing
  * the configured maximum (`Some(message)`).
  */
final case class DependencyCount(
  path: String,
  count: Int,
  maxDependencyCount: Either[Option[String], Int]
)
