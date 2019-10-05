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
import com.nawforce.platform.System.Integer;
import com.nawforce.platform.System.String;
import com.nawforce.platform.System.*;


@SuppressWarnings("unused")
public class Organization extends SObject {
	public static SObjectType$<Organization> SObjectType;
	public static SObjectTypeFields$<Organization> Fields;
	public static SObjectTypeFieldSets$<Organization> FieldSets;
	public com.nawforce.platform.System.Address Address;
	public String City;
	public String ComplianceBccEmail;
	public String Country;
	public String DefaultAccountAccess;
	public String DefaultCalendarAccess;
	public String DefaultCampaignAccess;
	public String DefaultCaseAccess;
	public String DefaultContactAccess;
	public String DefaultLeadAccess;
	public String DefaultLocaleSidKey;
	public String DefaultOpportunityAccess;
	public String DefaultPricebookAccess;
	public String Division;
	public String Fax;
	public Integer FiscalYearStartMonth;
	public String GeocodeAccuracy;
	public String InstanceName;
	public com.nawforce.platform.System.Boolean IsReadOnly;
	public com.nawforce.platform.System.Boolean IsSandbox;
	public String LanguageLocaleKey;
	public Decimal Latitude;
	public Decimal Longitude;
	public Integer MonthlyPageViewsEntitlement;
	public Integer MonthlyPageViewsUsed;
	public String Name;
	public String NamespacePrefix;
	public Integer NumKnowledgeService;
	public String OrganizationType;
	public String Phone;
	public String PostalCode;
	public com.nawforce.platform.System.Boolean PreferencesActivityAnalyticsEnabled;
	public com.nawforce.platform.System.Boolean PreferencesAutoSelectIndividualOnMerge;
	public com.nawforce.platform.System.Boolean PreferencesConsentManagementEnabled;
	public com.nawforce.platform.System.Boolean PreferencesIndividualAutoCreateEnabled;
	public com.nawforce.platform.System.Boolean PreferencesLightningLoginEnabled;
	public com.nawforce.platform.System.Boolean PreferencesOnlyLLPermUserAllowed;
	public com.nawforce.platform.System.Boolean PreferencesRequireOpportunityProducts;
	public com.nawforce.platform.System.Boolean PreferencesTerminateOldestSession;
	public com.nawforce.platform.System.Boolean PreferencesTransactionSecurityPolicy;
	public String PrimaryContact;
	public com.nawforce.platform.System.Boolean ReceivesAdminInfoEmails;
	public com.nawforce.platform.System.Boolean ReceivesInfoEmails;
	public String SignupCountryIsoCode;
	public String State;
	public String Street;
	public String TimeZoneSidKey;
	public Datetime TrialExpirationDate;
	public String UiSkin;
	public Boolean UsesStartDateAsFiscalYearName;
	public String WebToCaseDefaultOrigin;

	public AttachedContentDocument[] AttachedContentDocuments;
	public CombinedAttachment[] CombinedAttachments;
	public ContentDocumentLink[] ContentDocumentLinks;
	public CustomBrand[] CustomBrands;
}
