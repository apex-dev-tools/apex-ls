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

import com.nawforce.pkgforce.memory.CleanableCache

import scala.collection.mutable

/* Common names & Name interning support. */
object Names extends CleanableCache {
  private var nameCache = mutable.HashMap[String, Name]()

  def apply(name: String): Name             = cache(name)
  def apply(name: Name): Name               = cache(name.value)
  def safeApply(name: String): Option[Name] = Option(name).filterNot(_.isEmpty).map(n => Name(n))

  private def cache(value: String): Name = {
    nameCache.getOrElseUpdate(value, { new Name(value) })
  }

  override def clean(): Unit = {
    nameCache = new mutable.HashMap[String, Name]()
  }

  lazy val Empty: Name                   = cache("")
  lazy val System: Name                  = cache("System")
  lazy val Schema: Name                  = cache("Schema")
  lazy val Database: Name                = cache("Database")
  lazy val Batchable: Name               = cache("Batchable")
  lazy val Schedulable: Name             = cache("Schedulable")
  lazy val Queueable: Name               = cache("Queueable")
  lazy val Finalizer: Name               = cache("Finalizer")
  lazy val Void: Name                    = cache("void")
  lazy val Class: Name                   = cache("Class")
  lazy val List$ : Name                  = cache("List")
  lazy val Set$ : Name                   = cache("Set")
  lazy val Map$ : Name                   = cache("Map")
  lazy val Type: Name                    = cache("Type")
  lazy val Object: Name                  = cache("Object")
  lazy val Object$ : Name                = cache("Object$")
  lazy val Interface$ : Name             = cache("Interface$")
  lazy val Null$ : Name                  = cache("Null$")
  lazy val Any$ : Name                   = cache("Any$")
  lazy val RecordSet$ : Name             = cache("RecordSet$")
  lazy val SObject: Name                 = cache("SObject")
  lazy val SObjects: Name                = cache("SObjects")
  lazy val Internal: Name                = cache("Internal")
  lazy val Boolean: Name                 = cache("Boolean")
  lazy val ApexPages: Name               = cache("ApexPages")
  lazy val PageReference: Name           = cache("PageReference")
  lazy val Label: Name                   = cache("Label")
  lazy val Page: Name                    = cache("Page")
  lazy val Component: Name               = cache("Component")
  lazy val Apex: Name                    = cache("Apex")
  lazy val Chatter: Name                 = cache("Chatter")
  lazy val Flow: Name                    = cache("Flow")
  lazy val Interview: Name               = cache("Interview")
  lazy val c: Name                       = cache("c")
  lazy val String: Name                  = cache("String")
  lazy val Id: Name                      = cache("Id")
  lazy val Date: Name                    = cache("Date")
  lazy val Datetime: Name                = cache("Datetime")
  lazy val Time: Name                    = cache("Time")
  lazy val Decimal: Name                 = cache("Decimal")
  lazy val Double: Name                  = cache("Double")
  lazy val Blob: Name                    = cache("Blob")
  lazy val Location: Name                = cache("Location")
  lazy val Address: Name                 = cache("Address")
  lazy val NameName: Name                = cache("Name")
  lazy val AccessLevel: Name             = cache("AccessLevel")
  lazy val Parent: Name                  = cache("Parent")
  lazy val ParentId: Name                = cache("ParentId")
  lazy val RowCause: Name                = cache("RowCause")
  lazy val UserOrGroupId: Name           = cache("UserOrGroupId")
  lazy val BestCommentId: Name           = cache("BestCommentId")
  lazy val Body: Name                    = cache("Body")
  lazy val CommentCount: Name            = cache("CommentCount")
  lazy val ConnectionId: Name            = cache("ConnectionId")
  lazy val InsertedById: Name            = cache("InsertedById")
  lazy val IsRichText: Name              = cache("IsRichText")
  lazy val LikeCount: Name               = cache("LikeCount")
  lazy val LinkUrl: Name                 = cache("LinkUrl")
  lazy val NetworkScope: Name            = cache("NetworkScope")
  lazy val RelatedRecordId: Name         = cache("RelatedRecordId")
  lazy val Title: Name                   = cache("Title")
  lazy val Visibility: Name              = cache("Visibility")
  lazy val Field: Name                   = cache("Field")
  lazy val NewValue: Name                = cache("NewValue")
  lazy val OldValue: Name                = cache("OldValue")
  lazy val Integer: Name                 = cache("Integer")
  lazy val Long: Name                    = cache("Long")
  lazy val RecordTypeId: Name            = cache("RecordTypeId")
  lazy val SetupOwnerId: Name            = cache("SetupOwnerId")
  lazy val SObjectType: Name             = cache("SObjectType")
  lazy val SObjectField: Name            = cache("SObjectField")
  lazy val FieldSet: Name                = cache("FieldSet")
  lazy val DescribeFieldResult: Name     = cache("DescribeFieldResult")
  lazy val SObjectTypeFieldSets: Name    = cache("SObjectTypeFieldSets")
  lazy val DescribeSObjectResult$ : Name = cache("DescribeSObjectResult$")
  lazy val SObjectType$ : Name           = cache("SObjectType$")
  lazy val SObjectTypeFields$ : Name     = cache("SObjectTypeFields$")
  lazy val SObjectTypeFieldSets$ : Name  = cache("SObjectTypeFieldSets$")
  lazy val SObjectFields$ : Name         = cache("SObjectFields$")
  lazy val SObjectFieldRowCause$ : Name  = cache("SObjectFieldRowCause$")
  lazy val Trigger$ : Name               = cache("Trigger$")
  lazy val Activity: Name                = cache("Activity")
  lazy val Task: Name                    = cache("Task")
  lazy val Event: Name                   = cache("Event")
  lazy val DeveloperName: Name           = cache("DeveloperName")
  lazy val IsProtected: Name             = cache("IsProtected")
  lazy val Language: Name                = cache("Language")
  lazy val MasterLabel: Name             = cache("MasterLabel")
  lazy val NamespacePrefix: Name         = cache("NamespacePrefix")
  lazy val QualifiedAPIName: Name        = cache("QualifiedAPIName")
  lazy val User: Name                    = cache("User")
  lazy val UserRecordAccess: Name        = cache("UserRecordAccess")
  lazy val ReplayId: Name                = cache("ReplayId")
  lazy val EventUuid: Name               = cache("EventUuid")
  lazy val Fields: Name                  = cache("Fields")
  lazy val GetSObjectType: Name          = cache("GetSObjectType")
  lazy val Clone: Name                   = cache("clone")
  lazy val BatchableContext: Name        = cache("BatchableContext")
  lazy val Trigger: Name                 = cache("Trigger")
  lazy val ListName: Name                = cache("List")
  lazy val SetName: Name                 = cache("Set")
  lazy val MapName: Name                 = cache("Map")
  lazy val Exception: Name               = cache("Exception")
  lazy val Version: Name                 = cache("Version")
  lazy val Approval: Name                = cache("Approval")
  lazy val Datacloud: Name               = cache("Datacloud")
  lazy val Messaging: Name               = cache("Messaging")
  lazy val Package: Name                 = cache("Package")
  lazy val QuickAction: Name             = cache("QuickAction")
  lazy val Search: Name                  = cache("Search")
  lazy val dom: Name                     = cache("dom")
  lazy val eventbus: Name                = cache("eventbus")
  lazy val licensing: Name               = cache("licensing")
  lazy val hashCode$ : Name              = cache("hashCode$")
  lazy val equals$ : Name                = cache("equals$")
  lazy val toString$ : Name              = cache("toString$")
}
