/*
 Copyright (c) 2019 Kevin Jones, All rights reserved.
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
package com.nawforce.pkgforce.names

/** A name with optional namespace prefixing & optional type suffix such as foo&#95;&#95;field&#95;&#95;c. Only a
  * small set of suffixes are supported. It's not clear what the full list used by Salesforce is.
  * This class is safe to use on non-encoded names which don't contain &#95;&#95;, although you can not
  * default the namespace on them.
  *
  * Where we only have two parts of the name then if the last parts is not a valid suffix the handling
  * will assume the first part is a namespace and the second the name, as in Page.pkg1&#95;&#95;TestPage.
  *
  * The code deals with a few exception cases where splitting on &#95;&#95; would give either a wrong
  * name or ext part. For subfields the subfield name is combined with the extension. For supporting
  * SObjects such as MyObject&#95;&#95;Feed the 'Feed' is considered an extension in the same way that 'c'
  * would be.
  */
final case class EncodedName(name: Name, ext: Option[Name], namespace: Option[Name]) {

  /** The name & suffix excluding any namespace. */
  lazy val localName: Name = Name(name.value + ext.map("__" + _.value).getOrElse(""))

  /** The name & suffix including any namespace. */
  lazy val fullName: Name = Name(namespace.map(_.value + "__").getOrElse("") + localName.value)

  /** An encoded name using the provided namespace if one was not provided. */
  def defaultNamespace(ns: Option[Name]): EncodedName = {
    if (ext.nonEmpty && namespace.isEmpty)
      EncodedName(name, ext, ns)
    else
      this
  }

  def asTypeName: TypeName = {
    TypeName(localName, Nil, namespace.map(TypeName(_)))
  }
}

object EncodedName {

  /** Standard set of extensions, note that the subfield extension "s" is missing as this
    * often needs special handling because it does not fit the &lt;ns&#95;&#95;>name&#95;&#95;ext pattern
    */
  private final val extensions = Set("c", "r", "e", "b", "mdt", "share", "history", "feed")

  def apply(name: Name): EncodedName = {
    apply(name.value)
  }

  def apply(name: Name, defaultNamespace: Option[Name]): EncodedName = {
    apply(name.value, defaultNamespace)
  }

  def apply(name: String, defaultNamespace: Option[Name] = None): EncodedName = {
    val parts = nameSplit(name)
    parts.size match {
      case 3 =>
        val normalExt = parts(2).toLowerCase
        if (extensions.contains(normalExt) || normalExt.endsWith("__s")) {
          EncodedName(Name(parts(1)), Some(Name(parts(2))), Some(Name(parts.head)))
        } else {
          // If extension is not recognised don't split
          EncodedName(Name(name), None, None)
        }
      case 2 =>
        val normalExt = parts(1).toLowerCase
        if (extensions.contains(normalExt) || normalExt.endsWith("__s"))
          EncodedName(Name(parts.head), Some(Name(parts(1))), defaultNamespace)
        else
          EncodedName(Name(parts(1)), None, Some(Name(parts.head)))
      case 0 => EncodedName(Name(name), None, None)
    }
  }

  /** Split a name around leading and trailing pair of underscore separators, e.g. 'ns&#95;&#95;Custom&#95;&#95;c' =>
    * Seq("ns", "Custom", "c"). Subfields are returned in last segment, e.g. Field&#95;&#95;Subfield&#95;&#95;s =>
    * Seq("Field", "Subfield&#95;&#95;s")
    * @return Empty Seq when no separators found or Seq of length 2-3.
    */
  private def nameSplit(name: String): Seq[String] = {
    val headSplit = name.indexOf("__", 0)
    if (headSplit < 1 || headSplit + 2 == name.length)
      return Seq.empty

    var tailSplit = 0
    if (name.endsWith("__s") && name.length >= 4) {
      tailSplit = name.lastIndexOf("__", name.length() - 4)
      if (tailSplit == -1)
        return Seq.empty
    } else {
      tailSplit = name.lastIndexOf("__", name.length() - 2)
    }

    if (tailSplit == -1 || tailSplit <= headSplit)
      Seq(name.substring(0, headSplit), name.substring(headSplit + 2))
    else
      Seq(
        name.substring(0, headSplit),
        name.substring(headSplit + 2, tailSplit),
        name.substring(tailSplit + 2)
      )
  }

  /** Test if name contains an encoded name that may need a namespace adding for type
    * resolving. This is broken out from the EncodedName handling to improve performance.
    */
  def encodedNeedsNamespace(name: Name): Boolean = {
    val value  = name.value
    val suffix = extractSuffixOrReturn(value)
    if (suffix eq value)
      return false

    val firstSeparator = value.indexOf("__")
    if (firstSeparator == value.length - suffix.length - 2) {
      true
    } else {
      val secondSeparator = value.indexOf("__", firstSeparator + 1)
      if (secondSeparator == value.length - suffix.length - 2) {
        suffix == "s"
      } else {
        false
      }
    }
  }

  private def extractSuffixOrReturn(name: String): String = {
    val len = name.length
    if (len >= 4) {
      name.charAt(len - 1) match {
        case 'c' | 'C' if name.charAt(len - 2) == '_' && name.charAt(len - 3) == '_' => "c"
        case 'r' | 'R' if name.charAt(len - 2) == '_' && name.charAt(len - 3) == '_' => "r"
        case 'e' | 'E' if name.charAt(len - 2) == '_' && name.charAt(len - 3) == '_' => "e"
        case 'b' | 'B' if name.charAt(len - 2) == '_' && name.charAt(len - 3) == '_' => "b"
        case 's' | 'S' if name.charAt(len - 2) == '_' && name.charAt(len - 3) == '_' => "s"
        case 't' | 'T' if len >= 6 && name.toLowerCase.endsWith("__mdt")             => "mdt"
        case 'e' | 'E' if len >= 8 && name.toLowerCase.endsWith("__share")           => "share"
        case 'y' | 'Y' if len >= 10 && name.toLowerCase.endsWith("__history")        => "history"
        case 'd' | 'D' if len >= 7 && name.toLowerCase.endsWith("__feed")            => "feed"
        case _                                                                       => name
      }
    } else {
      name
    }
  }

}
