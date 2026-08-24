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

package com.nawforce.apexlink.names

import com.nawforce.pkgforce.names.{Name, TypeName}

/** Platform types that Apex refuses to expose over @AuraEnabled methods, fields & parameters.
  *
  * The list is not documented by Salesforce, it was obtained by compiling each platform type known
  * to us as an @AuraEnabled return type against an API 68.0 org. The same list is used for return
  * types, parameter types and field types. Types outside it, including user classes, user enums,
  * user interfaces and user exception classes, are all accepted.
  */
object AuraEnabledTypes {

  private def namespaced(namespace: String, names: String*): (Name, Set[Name]) = {
    (Name(namespace), names.map(Name(_)).toSet)
  }

  /** Namespace to the type names disallowed within it. Every entry is a two level name, matching
    * is by name only so type arguments never take part, see [[isDisallowed]].
    */
  private val disallowedTypes: Map[Name, Set[Name]] = Map(
    namespaced(
      "System",
      "Address",
      "ApexPages",
      "Approval",
      "AssertException",
      "AsyncException",
      "Aura",
      "AuraHandledException",
      "BigObjectException",
      "CalloutException",
      "CanvasException",
      "Cookie",
      "Database",
      "DmlException",
      "DuplicateMessageException",
      "EmailException",
      "EmailTemplateRenderException",
      "EventBus",
      "Exception",
      "ExternalObjectException",
      "FatalCursorException",
      "FinalException",
      "FlexQueue",
      "FlowException",
      "FormulaEvaluationException",
      "FormulaValidationException",
      "HandledException",
      "Http",
      "HttpRequest",
      "HttpResponse",
      "InvalidParameterValueException",
      "InvalidReadOnlyUserDmlException",
      "JSONException",
      "LicenseException",
      "LimitException",
      "Limits",
      "ListException",
      "Location",
      "MathException",
      "Messaging",
      "NoAccessException",
      "NoDataFoundException",
      "NoSuchElementException",
      "NullPointerException",
      "PageReference",
      "PlatformCacheException",
      "ProcedureException",
      "QueryException",
      "QuickAction",
      "RequiredFeatureMissingException",
      "ResetPasswordResult",
      "SObjectException",
      "Savepoint",
      "Schema",
      "Search",
      "SearchException",
      "SecurityException",
      "SelectOption",
      "SerializationException",
      "Set",
      "StringException",
      "System",
      "TouchHandledException",
      "TransientCursorException",
      "TypeException",
      "UnexpectedException",
      "Version",
      "VisualforceException",
      "WaveTemplateException",
      "WebServiceCallout",
      "XmlException",
      "XmlStreamReader",
      "XmlStreamWriter"
    ),
    namespaced(
      "Schema",
      "ChildRelationship",
      "DataCategory",
      "DataCategoryGroupSobjectTypePair",
      "DescribeColorResult",
      "DescribeDataCategoryGroupResult",
      "DescribeDataCategoryGroupStructureResult",
      "DescribeFieldResult",
      "DescribeIconResult",
      "DescribeSObjectResult",
      "DescribeTabResult",
      "DescribeTabSetResult",
      "FieldSet",
      "FieldSetMember",
      "FilteredLookupInfo",
      "PicklistEntry",
      "RecordTypeInfo",
      "SObjectField",
      "SObjectType",
      "SObjectTypeFieldSets",
      "SObjectTypeFields"
    ),
    namespaced(
      "Database",
      "DeleteResult",
      "DeletedRecord",
      "DuplicateError",
      "EmptyRecycleBinResult",
      "Error",
      "GetDeletedResult",
      "GetUpdatedResult",
      "LeadConvert",
      "LeadConvertResult",
      "MergeRequest",
      "MergeResult",
      "QueryLocator",
      "SaveResult",
      "UndeleteResult",
      "UpsertResult"
    ),
    namespaced(
      "Messaging",
      "Email",
      "EmailAttachment",
      "EmailFileAttachment",
      "MassEmailMessage",
      "RenderEmailTemplateBodyResult",
      "RenderEmailTemplateError",
      "SendEmailError",
      "SendEmailResult",
      "SingleEmailMessage"
    ),
    namespaced(
      "ApexPages",
      "IdeaStandardController",
      "IdeaStandardSetController",
      "KnowledgeArticleVersionStandardController",
      "Message",
      "StandardController",
      "StandardSetController"
    ),
    namespaced(
      "Approval",
      "LockResult",
      "ProcessRequest",
      "ProcessResult",
      "ProcessSubmitRequest",
      "ProcessWorkitemRequest",
      "UnlockResult"
    ),
    namespaced(
      "QuickAction",
      "Control",
      "DescribeAvailableQuickActionResult",
      "DescribeLayoutComponent",
      "DescribeLayoutItem",
      "DescribeLayoutRow",
      "DescribeLayoutSection",
      "DescribeQuickActionDefaultValue",
      "DescribeQuickActionParameter",
      "DescribeQuickActionResult",
      "EmptySpace",
      "ExpandedLookup",
      "Field",
      "FieldLayoutComponent",
      "QuickActionRequest",
      "QuickActionResult",
      "QuickActionTemplateResult",
      "ReportChartComponent",
      "SControl",
      "Separator",
      "VisualforcePage"
    ),
    namespaced(
      "Datacloud",
      "AdditionalInformationMap",
      "DuplicateResult",
      "FieldDiff",
      "FieldDifferenceType",
      "FindDuplicatesResult",
      "MatchRecord",
      "MatchResult"
    ),
    namespaced(
      "Search",
      "KnowledgeSuggestionFilter",
      "QuestionSuggestionFilter",
      "SuggestionOption"
    ),
    namespaced("dom", "Document", "XmlNode"),
    namespaced("eventbus", "ChangeEventHeader"),
    namespaced("Package", "Version")
  )

  /** Is this type, or any type used as an argument of it, disallowed by @AuraEnabled? The check is
    * over the whole type argument tree, so Map<Schema.SObjectType, List<SObject>> is disallowed,
    * but it does not extend into the members of a user type.
    *
    * Only a namespace qualified two level name can match, so a nested type such as
    * Messaging.InboundEmail.BinaryAttachment is not caught by its outermost name. Names compare
    * case insensitively, as Apex identifiers do.
    */
  def isDisallowed(typeName: TypeName): Boolean = {
    typeName.outer.exists(namespace =>
      namespace.outer.isEmpty && namespace.params.isEmpty &&
        disallowedTypes.get(namespace.name).exists(_.contains(typeName.name))
    ) || typeName.params.exists(isDisallowed)
  }
}
