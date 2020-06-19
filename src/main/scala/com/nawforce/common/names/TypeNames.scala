package com.nawforce.common.names

import java.lang.ref.WeakReference

import com.nawforce.common.api.{Name, TypeName}
import upickle.default.{macroRW, ReadWriter => RW}

import scala.collection.mutable

object TypeNames {
  implicit val rw: RW[TypeName] = macroRW

  private val cache = mutable.WeakHashMap[TypeName, WeakReference[TypeName]]()

  lazy val Void: TypeName = TypeName(Name("void")).intern
  lazy val Object: TypeName = TypeName(Name("Object")).intern

  lazy val Internal: TypeName = TypeName(Names.Internal).intern
  lazy val Null: TypeName = TypeName(Names.Null$, Nil, Some(TypeNames.Internal)).intern
  lazy val Any: TypeName = TypeName(Names.Any$, Nil, Some(TypeNames.Internal)).intern
  lazy val RecordSet: TypeName = TypeName(Names.RecordSet$, Seq(TypeNames.SObject), Some(TypeNames.Internal)).intern
  lazy val InternalObject: TypeName = TypeName(Names.Object$, Nil, Some(TypeNames.Internal)).intern

  lazy val System: TypeName = TypeName(Names.System).intern
  lazy val Long: TypeName = TypeName(Names.Long, Nil, Some(TypeNames.System)).intern
  lazy val Integer: TypeName = TypeName(Names.Integer, Nil, Some(TypeNames.System)).intern
  lazy val Double: TypeName = TypeName(Names.Double, Nil, Some(TypeNames.System)).intern
  lazy val Decimal: TypeName = TypeName(Names.Decimal, Nil, Some(TypeNames.System)).intern
  lazy val String: TypeName = TypeName(Names.String, Nil, Some(TypeNames.System)).intern
  lazy val Boolean: TypeName = TypeName(Names.Boolean, Nil, Some(TypeNames.System)).intern
  lazy val Date: TypeName = TypeName(Names.Date, Nil, Some(TypeNames.System)).intern
  lazy val Datetime: TypeName = TypeName(Names.Datetime, Nil, Some(TypeNames.System)).intern
  lazy val Time: TypeName = TypeName(Names.Time, Nil, Some(TypeNames.System)).intern
  lazy val Blob: TypeName = TypeName(Names.Blob, Nil, Some(TypeNames.System)).intern
  lazy val Location: TypeName = TypeName(Names.Location, Nil, Some(TypeNames.System)).intern
  lazy val Address: TypeName = TypeName(Names.Address, Nil, Some(TypeNames.System)).intern

  lazy val Id: TypeName = TypeName(Names.Id, Nil, Some(TypeNames.System)).intern
  lazy val TypeType: TypeName = TypeName(Names.Type, Nil, Some(TypeNames.System)).intern
  lazy val PageReference: TypeName = TypeName(Names.PageReference, Nil, Some(TypeNames.System)).intern
  lazy val SObject: TypeName = TypeName(Names.SObject, Nil, Some(TypeNames.System)).intern

  lazy val ApexPages: TypeName = TypeName(Names.ApexPages).intern
  lazy val ApexPagesPageReference: TypeName = TypeName(Names.PageReference, Nil, Some(TypeNames.ApexPages)).intern
  lazy val ApexPagesComponent: TypeName = TypeName(Names.Component, Nil, Some(TypeNames.ApexPages)).intern
  lazy val ApexComponent: TypeName = TypeName(Names.Apex, Nil, Some(TypeNames.Component)).intern
  lazy val ChatterComponent: TypeName = TypeName(Names.Chatter, Nil, Some(TypeNames.Component)).intern

  lazy val Schema: TypeName = TypeName(Names.Schema).intern
  lazy val SObjectType: TypeName = TypeName(Names.SObjectType, Nil, Some(TypeNames.Schema)).intern
  lazy val SObjectField: TypeName = TypeName(Names.SObjectField, Nil, Some(TypeNames.Schema)).intern
  lazy val FieldSet: TypeName = TypeName(Names.FieldSet, Nil, Some(TypeNames.Schema)).intern
  lazy val DescribeSObjectResult: TypeName = TypeName(Names.DescribeSObjectResult, Nil, Some(TypeNames.Schema)).intern
  lazy val DescribeFieldResult: TypeName = TypeName(Names.DescribeFieldResult, Nil, Some(TypeNames.Schema)).intern
  lazy val SObjectTypeFieldSets: TypeName = TypeName(Names.SObjectTypeFieldSets, Nil, Some(TypeNames.Schema)).intern

  lazy val DescribeSObjectResult$: TypeName = TypeName(Names.DescribeSObjectResult$, Nil, Some(TypeNames.Internal)).intern
  lazy val SObjectType$: TypeName = TypeName(Names.SObjectType$, Nil, Some(TypeNames.Internal)).intern
  lazy val SObjectTypeFields$: TypeName = TypeName(Names.SObjectTypeFields$, Nil, Some(TypeNames.Internal)).intern
  lazy val SObjectTypeFieldSets$: TypeName = TypeName(Names.SObjectTypeFieldSets$, Nil, Some(TypeNames.Internal)).intern
  lazy val SObjectFields$: TypeName = TypeName(Names.SObjectFields$, Nil, Some(TypeNames.Internal)).intern
  lazy val SObjectFieldRowCause$: TypeName = TypeName(Names.SObjectFieldRowCause$, Nil, Some(TypeNames.Internal)).intern
  lazy val Trigger$: TypeName = TypeName(Names.Trigger$, Nil, Some(TypeNames.Internal)).intern

  lazy val Database: TypeName = TypeName(Names.Database).intern
  lazy val BatchableContext: TypeName = TypeName(Names.BatchableContext, Nil, Some(TypeNames.Database)).intern

  lazy val User: TypeName = TypeName(Names.User).intern
  lazy val UserRecordAccess: TypeName = TypeName(Names.UserRecordAccess).intern

  lazy val Label: TypeName = TypeName(Names.Label, Nil, Some(TypeNames.System)).intern
  lazy val Flow: TypeName = TypeName(Names.Flow).intern
  lazy val Interview: TypeName = TypeName(Names.Interview, Nil, Some(TypeNames.Flow)).intern
  lazy val Component: TypeName = TypeName(Names.Component, Nil, None).intern
  lazy val Page: TypeName = TypeName(Names.Page, Nil, None).intern

  def describeSObjectResultOf(typeName: TypeName): TypeName = DescribeSObjectResult$.withParams(Seq(typeName)).intern
  def sObjectType$(typeName: TypeName): TypeName = SObjectType$.withParams(Seq(typeName)).intern
  def sObjectTypeFields$(typeName: TypeName): TypeName = SObjectTypeFields$.withParams(Seq(typeName)).intern
  def sObjectTypeFieldSets$(typeName: TypeName): TypeName = SObjectTypeFieldSets$.withParams(Seq(typeName)).intern
  def sObjectFields$(typeName: TypeName): TypeName = SObjectFields$.withParams(Seq(typeName)).intern
  def trigger(typeName: TypeName): TypeName = Trigger$.withParams(Seq(typeName)).intern

  def listOf(typeName: TypeName): TypeName = TypeName(Names.List$, Seq(typeName), Some(TypeNames.System)).intern
  def mapOf(keyType: TypeName, valueType: TypeName): TypeName = TypeName(Names.Map$, Seq(keyType, valueType), Some(TypeNames.System)).intern
  def recordSetOf(typeName: TypeName): TypeName = TypeName(Names.RecordSet$, Seq(typeName), Some(TypeNames.Internal)).intern

  /** Interning support for TypeName, used to reduce memory load, mainly from cached data. */
  implicit class TypeNameOps(typeName: TypeName) {
    def intern: TypeName = {
      TypeNames.intern(
        TypeName(intern(typeName.name), typeName.params.map(_.intern), typeName.outer.map(_.intern))
      )
    }

    private def intern(name: Name): Name = {
      Names(name.value)
    }
  }

  def intern(typeName: TypeName): TypeName = {
    cache.getOrElseUpdate(typeName, new WeakReference(typeName)).get
  }

}
