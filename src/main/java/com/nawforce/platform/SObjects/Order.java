/*
 [The "BSD licence"]
 Copyright (c) 2019 Kevin Jones
 All rights reserved.

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

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.nawforce.platform.SObjects;

import com.nawforce.platform.Internal.SObjectType$;
import com.nawforce.platform.Internal.SObjectTypeFieldSets$;
import com.nawforce.platform.Internal.SObjectTypeFields$;
import com.nawforce.platform.System.Boolean;
import com.nawforce.platform.System.String;
import com.nawforce.platform.System.*;


@SuppressWarnings("unused")
public class Order extends SObject {
	public static SObjectType$<Order> SObjectType;
	public static SObjectTypeFields$<Order> Fields;
	public static SObjectTypeFieldSets$<Order> FieldSets;
	public Id RecordTypeId;
	public Id AccountId;
	public Account Account;
	public Id ActivatedById;
	public User ActivatedBy;
	public Datetime ActivatedDate;
	public Id BillToContactId;
	public Contact BillToContact;
	public Address BillingAddress;
	public com.nawforce.platform.System.String BillingCity;
	public com.nawforce.platform.System.String BillingCountry;
	public com.nawforce.platform.System.String BillingGeocodeAccuracy;
	public Decimal BillingLatitude;
	public Decimal BillingLongitude;
	public com.nawforce.platform.System.String BillingPostalCode;
	public com.nawforce.platform.System.String BillingState;
	public com.nawforce.platform.System.String BillingStreet;
	public Id CompanyAuthorizedById;
	public User CompanyAuthorizedBy;
	public Date CompanyAuthorizedDate;
	public Id ContractId;
	public Contract Contract;
	public com.nawforce.platform.System.String CurrencyIsoCode;
	public Id CustomerAuthorizedById;
	public Contact CustomerAuthorizedBy;
	public Date CustomerAuthorizedDate;
	public com.nawforce.platform.System.String Description;
	public Date EffectiveDate;
	public Date EndDate;
	public Boolean IsReductionOrder;
	public Datetime LastReferencedDate;
	public Datetime LastViewedDate;
	public com.nawforce.platform.System.String Name;
	public com.nawforce.platform.System.String OrderNumber;
	public com.nawforce.platform.System.String OrderReferenceNumber;
	public Id OriginalOrderId;
	public Order OriginalOrder;
	public Id OwnerId;
	public Group Owner;
	public Date PoDate;
	public com.nawforce.platform.System.String PoNumber;
	public Id Pricebook2Id;
	public Pricebook2 Pricebook2;
	public Id ShipToContactId;
	public Contact ShipToContact;
	public Address ShippingAddress;
	public com.nawforce.platform.System.String ShippingCity;
	public com.nawforce.platform.System.String ShippingCountry;
	public com.nawforce.platform.System.String ShippingGeocodeAccuracy;
	public Decimal ShippingLatitude;
	public Decimal ShippingLongitude;
	public com.nawforce.platform.System.String ShippingPostalCode;
	public com.nawforce.platform.System.String ShippingState;
	public com.nawforce.platform.System.String ShippingStreet;
	public com.nawforce.platform.System.String Status;
	public com.nawforce.platform.System.String StatusCode;
	public Decimal TotalAmount;
	public String Type;

	public ActivityHistory[] ActivityHistories;
	public AttachedContentDocument[] AttachedContentDocuments;
	public Attachment[] Attachments;
	public CombinedAttachment[] CombinedAttachments;
	public ContentDocumentLink[] ContentDocumentLinks;
	public EmailMessage[] Emails;
	public Event[] Events;
	public EntitySubscription[] FeedSubscriptionsForEntity;
	public OrderFeed[] Feeds;
	public OrderHistory[] Histories;
	public Note[] Notes;
	public NoteAndAttachment[] NotesAndAttachments;
	public OpenActivity[] OpenActivities;
	public OrderItem[] OrderItems;
	public Order[] Orders;
	public ProcessInstance[] ProcessInstances;
	public ProcessInstanceHistory[] ProcessSteps;
	public RecordActionHistory[] RecordActionHistories;
	public RecordAction[] RecordActions;
	public OrderShare[] Shares;
	public Task[] Tasks;
	public TopicAssignment[] TopicAssignments;
}
