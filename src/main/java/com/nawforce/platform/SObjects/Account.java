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

import com.nawforce.platform.Internal.SObjectFields$;
import com.nawforce.platform.System.Integer;
import com.nawforce.platform.System.String;
import com.nawforce.platform.System.*;

@SuppressWarnings("unused")
public class Account extends SObject {
	public static SObjectType$<Account> SObjectType;
	public static SObjectFields$<Account> Fields;


	public Id RecordTypeId;
	public String AccountNumber;
	public String AccountSource;
	public Decimal AnnualRevenue;
	public Address BillingAddress;
	public String BillingCity;
	public String BillingCountry;
	public String BillingGeocodeAccuracy;
	public Decimal BillingLatitude;
	public Decimal BillingLongitude;
	public String BillingPostalCode;
	public String BillingState;
	public String BillingStreet;
	public String CurrencyIsoCode;
	public Id DandbCompanyId;
	public DandBCompany DandbCompany;
	public String Description;
	public String DunsNumber;
	public String Fax;
	public String Industry;
	public String Jigsaw;
	public String JigsawCompanyId;
	public Date LastActivityDate;
	public Datetime LastReferencedDate;
	public Datetime LastViewedDate;
	public Id MasterRecordId;
	public Account MasterRecord;
	public String NaicsCode;
	public String NaicsDesc;
	public String Name;
	public Integer NumberOfEmployees;
	public Id OwnerId;
	public User Owner;
	public String Ownership;
	public Id ParentId;
	public Account Parent;
	public String Phone;
	public String PhotoUrl;
	public String Rating;
	public Address ShippingAddress;
	public String ShippingCity;
	public String ShippingCountry;
	public String ShippingGeocodeAccuracy;
	public Decimal ShippingLatitude;
	public Decimal ShippingLongitude;
	public String ShippingPostalCode;
	public String ShippingState;
	public String ShippingStreet;
	public String Sic;
	public String SicDesc;
	public String Site;
	public String TickerSymbol;
	public String Tradestyle;
	public String Type;
	public String Website;
	public String YearStarted;

	public AccountContactRole[] AccountContactRoles;
	public AccountPartner[] AccountPartnersFrom;
	public AccountPartner[] AccountPartnersTo;
	public ActivityHistory[] ActivityHistories;
	public Asset[] Assets;
	public AttachedContentDocument[] AttachedContentDocuments;
	public Attachment[] Attachments;
	public Case[] Cases;
	public Account[] ChildAccounts;
	public CombinedAttachment[] CombinedAttachments;
	public Contact[] Contacts;
	public ContentDocumentLink[] ContentDocumentLinks;
	public Contract[] Contracts;
	public DuplicateRecordItem[] DuplicateRecordItems;
	public EmailMessage[] Emails;
	public Event[] Events;
	public EntitySubscription[] FeedSubscriptionsForEntity;
	public AccountFeed[] Feeds;
	public AccountHistory[] Histories;
	public Note[] Notes;
	public NoteAndAttachment[] NotesAndAttachments;
	public OpenActivity[] OpenActivities;
	public Opportunity[] Opportunities;
	public OpportunityPartner[] OpportunityPartnersTo;
	public Order[] Orders;
	public Partner[] PartnersFrom;
	public Partner[] PartnersTo;
	public ProcessInstance[] ProcessInstances;
	public ProcessInstanceHistory[] ProcessSteps;
	public Asset[] ProvidedAssets;
	public RecordActionHistory[] RecordActionHistories;
	public RecordAction[] RecordActions;
	public CollaborationGroupRecord[] RecordAssociatedGroups;
	public Asset[] ServicedAssets;
	public AccountShare[] Shares;
	public Task[] Tasks;
	public TopicAssignment[] TopicAssignments;
	public User[] Users;
}
