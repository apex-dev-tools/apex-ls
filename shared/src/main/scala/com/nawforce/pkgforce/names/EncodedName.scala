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

/** A name with optional namespace prefixing & optional type suffix such as foo__field__c. Only a
  * small set of suffixes are supported. It's not clear what the full list used by Salesforce is.
  * This class is safe to use on non-encoded names which don't contain \_\_, although you can not
  * default the namespace on them.
  *
  * Where namespace, name and suffix are provided the code will assert on a bad suffix. Where
  * we only have two parts of the name then if the last parts is not a valid suffix the handling
  * will assume the first part is a namespace and the second the name, as in Page.pkg1\_\_TestPage.
  *
  * The code deals with a few exception cases where splitting on \_\_ would give either a wrong
  * name or ext part. For subfields the subfield name is combined with the extension. For supporting
  * SObjects such as MyObject\_\_Feed the 'Feed' is considered an extension in the same way that 'c'
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
}
