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

/** Interpretation of an annotation value as it was written.
  *
  * Values arrive with their quotes intact and are only ever `true`, `false` or a single quoted
  * string in legal Apex. Anything else is a literal the grammar accepts so that it can be reported
  * here rather than lost to a parse error.
  */
object AnnotationValue {
  def isStringLiteral(value: String): Boolean =
    value.length >= 2 && value.head == '\'' && value.last == '\''

  def stringContent(value: String): String = value.substring(1, value.length - 1)

  def isBooleanLiteral(value: String): Boolean =
    value.equalsIgnoreCase("true") || value.equalsIgnoreCase("false")

  /** The value the platform reads, a quoted string is unwrapped before it is interpreted. */
  def content(value: String): String =
    if (isStringLiteral(value)) stringContent(value) else value

  /** True as `cacheable`, `SeeAllData` and `IsParallel` are read, only 'true' is true and every
    * other string coerces to false.
    */
  def coercesToTrue(value: String): Boolean = content(value).equalsIgnoreCase("true")

  /** True as `required` is read, only 'false' is false and every other string coerces to true.
    *
    * The platform is not consistent between the two readings, `cacheable='yes'` counts as false
    * while `required='yes'` counts as true. Both are org verified, see apex-ls#556.
    */
  def coercesToTrueUnlessFalse(value: String): Boolean =
    !content(value).equalsIgnoreCase("false")
}

/** Type of an annotation property, as the platform compiler reports it. */
sealed abstract class AnnotationPropertyType(val typeName: String) {

  /** Is the value, as written, acceptable for this type? */
  def accepts(value: String): Boolean
}

object AnnotationPropertyType {

  /** A Boolean, which also accepts a quoted string, the platform coerces it whatever it holds. */
  case object BooleanType extends AnnotationPropertyType("Boolean") {
    override def accepts(value: String): Boolean =
      AnnotationValue.isBooleanLiteral(value) || AnnotationValue.isStringLiteral(value)
  }

  /** A string, which must be single quoted, a Boolean is not coerced in this direction. */
  case object StringType extends AnnotationPropertyType("String") {
    override def accepts(value: String): Boolean = AnnotationValue.isStringLiteral(value)
  }
}

/** A property of an annotation.
  *
  * `values` restricts an otherwise free string to a known set, `notAllowedOn` names the kinds of
  * declaration the platform rejects the property on, and `valueCheck` carries a format rule,
  * returning a message when the content of the string is wrong. The check is given the content
  * with its quotes already removed.
  *
  * `notAllowedOn` lists what was observed to be rejected rather than what is allowed. A target the
  * annotation itself cannot be used on is not listed, the annotation's own placement rule reports
  * that, and reporting the property as well would be the same defect twice.
  */
final case class AnnotationProperty(
  name: String,
  propertyType: AnnotationPropertyType,
  values: Option[Set[String]] = None,
  notAllowedOn: Set[AnnotationTarget] = Set(),
  valueCheck: Option[String => Option[String]] = None
) {
  val key: String                        = name.toLowerCase
  val allowedValues: Option[Set[String]] = values.map(_.map(_.toLowerCase))
}

/** A rule over more than one property of the same annotation.
  *
  * `isInvalid` is given the winning value of each property, keyed by the lower cased property
  * name. A name written more than once is legal and the platform reads the last of them, so that
  * is the value a rule sees. The diagnostic is reported against `anchor`.
  */
final case class AnnotationCombination(
  anchor: String,
  message: String,
  isInvalid: Map[String, String] => Boolean
)

/** A rule requiring at least one of a set of properties to be written.
  *
  * The platform applies these to the parameter list however it was written, so the bare form with
  * no parentheses at all is rejected just as the empty parenthesised form is, and writing some
  * other property does not satisfy the rule.
  */
final case class AnnotationRequirement(properties: Set[String], message: String) {
  val keys: Set[String] = properties.map(_.toLowerCase)
}

/** What is known about one annotation's parameters.
  *
  * Only annotations whose parameter set has been established against an org appear in the table,
  * an annotation that is absent is not validated at all. That is deliberate, an unknown annotation
  * must never become an error, so that a platform annotation we have not seen yet still analyses.
  */
final case class AnnotationDefinition(
  name: String,
  properties: Seq[AnnotationProperty] = Seq(),
  bareValueProperty: Option[String] = None,
  requires: Option[AnnotationRequirement] = None,
  combinations: Seq[AnnotationCombination] = Seq()
) {
  private val byName: Map[String, AnnotationProperty] = properties.map(p => (p.key, p)).toMap

  def property(name: String): Option[AnnotationProperty] = byName.get(name.toLowerCase)

  def bareValue: Option[AnnotationProperty] = bareValueProperty.flatMap(property)
}

/** The annotation property table.
  *
  * Every entry is backed by an observed compile result against an API 68.0 org, see the
  * investigation on apex-ls#326 and its extension on apex-ls#556. Where the org's behaviour was
  * not established the property is left as a plain string rather than guessed at, so that nothing
  * here reports on code the platform accepts.
  *
  * One thing is left deliberately unvalidated: `@InvocableMethod`'s `configurationEditor` must
  * name a Lightning Web Component and `iconName` has a format, both recorded as B10. apex-ls
  * cannot resolve a component name and the icon format was never established, so both stay plain
  * strings. That is a permanent omission rather than an open gap.
  */
object AnnotationDefinition {

  import AnnotationPropertyType._

  /* B21, D40 and D41: cacheable and scope are rejected on a field and on a property, the two
     positions where @AuraEnabled is legal but the property is not. Nothing was probed for
     continuation, so it carries no restriction. */
  private val notOnFieldsOrProperties =
    Set[AnnotationTarget](AnnotationTarget.Fields, AnnotationTarget.Properties)

  /* M01-M16: all four documented values are accepted on both properties, case insensitively, and
     anything else is rejected as an unknown value. */
  private val jsonAccessValues =
    Set("never", "sameNamespace", "samePackage", "always")

  private def urlMappingCheck(content: String): Option[String] = {
    Option.when(!content.startsWith("/"))("Rest Resource url must begin with a forward slash, '/'")
  }

  private val definitions: Seq[AnnotationDefinition] =
    Seq(
      AnnotationDefinition(
        "AuraEnabled",
        Seq(
          AnnotationProperty("cacheable", BooleanType, notAllowedOn = notOnFieldsOrProperties),
          AnnotationProperty("continuation", BooleanType),
          AnnotationProperty(
            "scope",
            StringType,
            values = Some(Set("global")),
            notAllowedOn = notOnFieldsOrProperties
          )
        ),
        combinations = Seq(
          AnnotationCombination(
            "scope",
            "Invalid combination of values for properties cacheable and scope on AuraEnabled",
            values =>
              values.contains("scope") &&
                !values.get("cacheable").exists(AnnotationValue.coercesToTrue)
          )
        )
      ),
      AnnotationDefinition("Deprecated"),
      AnnotationDefinition("Future", Seq(AnnotationProperty("callout", BooleanType))),
      AnnotationDefinition("HttpDelete"),
      AnnotationDefinition("HttpGet"),
      AnnotationDefinition("HttpPatch"),
      AnnotationDefinition("HttpPost"),
      AnnotationDefinition("HttpPut"),
      /* configurationEditor and iconName carry format rules that are deliberately not implemented,
         see the note on this object. */
      AnnotationDefinition(
        "InvocableMethod",
        Seq(
          AnnotationProperty("label", StringType),
          AnnotationProperty("description", StringType),
          AnnotationProperty("category", StringType),
          AnnotationProperty("configurationEditor", StringType),
          AnnotationProperty("iconName", StringType),
          AnnotationProperty("callout", BooleanType)
        )
      ),
      AnnotationDefinition(
        "InvocableVariable",
        Seq(
          AnnotationProperty("label", StringType),
          AnnotationProperty("description", StringType),
          AnnotationProperty("required", BooleanType),
          AnnotationProperty("defaultValue", StringType),
          AnnotationProperty("placeholderText", StringType)
        ),
        combinations = Seq(
          AnnotationCombination(
            "defaultValue",
            "Invalid combination of values for properties required and defaultValue on InvocableVariable",
            values =>
              values.contains("defaultvalue") &&
                values.get("required").exists(AnnotationValue.coercesToTrueUnlessFalse)
          )
        )
      ),
      AnnotationDefinition(
        "IsTest",
        Seq(
          AnnotationProperty("SeeAllData", BooleanType),
          AnnotationProperty("IsParallel", BooleanType),
          AnnotationProperty("OnInstall", BooleanType)
        ),
        combinations = Seq(
          AnnotationCombination(
            "IsParallel",
            "Test class annotated with @isTest(IsParallel=true) cannot also be annotated with @isTest(SeeAllData=true)",
            values =>
              values.get("isparallel").exists(AnnotationValue.coercesToTrue) &&
                values.get("seealldata").exists(AnnotationValue.coercesToTrue)
          )
        )
      ),
      AnnotationDefinition(
        "JsonAccess",
        Seq(
          AnnotationProperty("serializable", StringType, values = Some(jsonAccessValues)),
          AnnotationProperty("deserializable", StringType, values = Some(jsonAccessValues))
        ),
        requires = Some(
          AnnotationRequirement(
            Set("serializable", "deserializable"),
            "At least one JSON serialization control parameter must be specified"
          )
        )
      ),
      AnnotationDefinition("NamespaceAccessible"),
      AnnotationDefinition("ReadOnly"),
      AnnotationDefinition("RemoteAction"),
      AnnotationDefinition(
        "RestResource",
        Seq(AnnotationProperty("urlMapping", StringType, valueCheck = Some(urlMappingCheck))),
        requires =
          Some(AnnotationRequirement(Set("urlMapping"), "Required property is missing: urlMapping"))
      ),
      AnnotationDefinition(
        "SuppressWarnings",
        Seq(AnnotationProperty("value", StringType)),
        bareValueProperty = Some("value")
      ),
      AnnotationDefinition("TearDown"),
      AnnotationDefinition("TestSetup"),
      AnnotationDefinition("TestVisible")
    )

  private val byName: Map[String, AnnotationDefinition] =
    definitions.map(d => (d.name.toLowerCase, d)).toMap

  def apply(name: String): Option[AnnotationDefinition] = byName.get(name.toLowerCase)
}
