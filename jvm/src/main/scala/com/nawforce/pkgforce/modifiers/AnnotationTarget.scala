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
package com.nawforce.pkgforce.modifiers

/** The kind of declaration an annotation was written on.
  *
  * Some annotation properties are legal only on some kinds of declaration, `cacheable` on
  * `@AuraEnabled` being the example. The plural names are the ones the platform compiler uses in
  * its own messages.
  */
sealed abstract class AnnotationTarget(val pluralName: String) {
  override def toString: String = pluralName
}

object AnnotationTarget {
  case object Classes extends AnnotationTarget("classes")

  case object Interfaces extends AnnotationTarget("interfaces")

  case object Enums extends AnnotationTarget("enums")

  case object Methods extends AnnotationTarget("methods")

  case object Fields extends AnnotationTarget("fields")

  case object Properties extends AnnotationTarget("properties")

  case object Constructors extends AnnotationTarget("constructors")

  case object Parameters extends AnnotationTarget("parameters")

  case object Locals extends AnnotationTarget("locals")
}
