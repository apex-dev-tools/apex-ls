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

import com.nawforce.platform.Internal.SObjectFields$;
import com.nawforce.platform.Internal.SObjectType$;
import com.nawforce.platform.System.String;
import com.nawforce.platform.System.*;


@SuppressWarnings("unused")
public class ContactChangeEvent extends SObject {
	public static SObjectType$<ContactChangeEvent> SObjectType;
	public static SObjectFields$<ContactChangeEvent> Fields;

	public Id AccountId;
	public Account Account;
	public String AssistantName;
	public String AssistantPhone;
	public Date Birthdate;
	public Object ChangeEventHeader;
	public Id CreatedById;
	public User CreatedBy;
	public Datetime CreatedDate;
	public String CurrencyIsoCode;
	public String Department;
	public String Description;
	public String Email;
	public Datetime EmailBouncedDate;
	public String EmailBouncedReason;
	public String Fax;
	public String FirstName;
	public String HomePhone;
	public Id Id;
	public Id IndividualId;
	public Individual Individual;
	public String Jigsaw;
	public String JigsawContactId;
	public Datetime LastCURequestDate;
	public Datetime LastCUUpdateDate;
	public Id LastModifiedById;
	public User LastModifiedBy;
	public Datetime LastModifiedDate;
	public String LastName;
	public String LeadSource;
	public Address MailingAddress;
	public String MailingCity;
	public String MailingCountry;
	public String MailingGeocodeAccuracy;
	public Decimal MailingLatitude;
	public Decimal MailingLongitude;
	public String MailingPostalCode;
	public String MailingState;
	public String MailingStreet;
	public String MobilePhone;
	public String Name;
	public Address OtherAddress;
	public String OtherCity;
	public String OtherCountry;
	public String OtherGeocodeAccuracy;
	public Decimal OtherLatitude;
	public Decimal OtherLongitude;
	public String OtherPhone;
	public String OtherPostalCode;
	public String OtherState;
	public String OtherStreet;
	public Id OwnerId;
	public User Owner;
	public String Phone;
	public String ReplayId;
	public Id ReportsToId;
	public Contact ReportsTo;
	public String Salutation;
	public String Title;
}
