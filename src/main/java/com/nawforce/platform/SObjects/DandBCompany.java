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
public class DandBCompany extends SObject {
	public static SObjectType$<DandBCompany> SObjectType;
	public static SObjectFields$<DandBCompany> Fields;

	public com.nawforce.platform.System.Address Address;
	public String City;
	public String CompanyCurrencyIsoCode;
	public String Country;
	public String CountryAccessCode;
	public String CurrencyCode;
	public String CurrencyIsoCode;
	public String Description;
	public String DomesticUltimateBusinessName;
	public String DomesticUltimateDunsNumber;
	public String DunsNumber;
	public Decimal EmployeeQuantityGrowthRate;
	public Decimal EmployeesHere;
	public String EmployeesHereReliability;
	public Decimal EmployeesTotal;
	public String EmployeesTotalReliability;
	public com.nawforce.platform.System.Integer FamilyMembers;
	public String Fax;
	public String FifthNaics;
	public String FifthNaicsDesc;
	public String FifthSic;
	public String FifthSic8;
	public String FifthSic8Desc;
	public String FifthSicDesc;
	public String FipsMsaCode;
	public String FipsMsaDesc;
	public com.nawforce.platform.System.Integer FortuneRank;
	public String FourthNaics;
	public String FourthNaicsDesc;
	public String FourthSic;
	public String FourthSic8;
	public String FourthSic8Desc;
	public String FourthSicDesc;
	public String GeoCodeAccuracy;
	public String GeocodeAccuracyStandard;
	public String GlobalUltimateBusinessName;
	public String GlobalUltimateDunsNumber;
	public Decimal GlobalUltimateTotalEmployees;
	public String ImportExportAgent;
	public String IncludedInSnP500;
	public Datetime LastReferencedDate;
	public Datetime LastViewedDate;
	public String Latitude;
	public String LegalStatus;
	public String LocationStatus;
	public String Longitude;
	public Address MailingAddress;
	public String MailingCity;
	public String MailingCountry;
	public String MailingGeocodeAccuracy;
	public String MailingPostalCode;
	public String MailingState;
	public String MailingStreet;
	public String MarketingPreScreen;
	public String MarketingSegmentationCluster;
	public String MinorityOwned;
	public String Name;
	public String NationalId;
	public String NationalIdType;
	public String OutOfBusiness;
	public String OwnOrRent;
	public String ParentOrHqBusinessName;
	public String ParentOrHqDunsNumber;
	public String Phone;
	public String PostalCode;
	public com.nawforce.platform.System.Integer PremisesMeasure;
	public String PremisesMeasureReliability;
	public String PremisesMeasureUnit;
	public String PrimaryNaics;
	public String PrimaryNaicsDesc;
	public String PrimarySic;
	public String PrimarySic8;
	public String PrimarySic8Desc;
	public String PrimarySicDesc;
	public Integer PriorYearEmployees;
	public Decimal PriorYearRevenue;
	public String PublicIndicator;
	public Decimal SalesTurnoverGrowthRate;
	public Decimal SalesVolume;
	public String SalesVolumeReliability;
	public String SecondNaics;
	public String SecondNaicsDesc;
	public String SecondSic;
	public String SecondSic8;
	public String SecondSic8Desc;
	public String SecondSicDesc;
	public String SixthNaics;
	public String SixthNaicsDesc;
	public String SixthSic;
	public String SixthSic8;
	public String SixthSic8Desc;
	public String SixthSicDesc;
	public String SmallBusiness;
	public String State;
	public String StockExchange;
	public String StockSymbol;
	public String Street;
	public String Subsidiary;
	public String ThirdNaics;
	public String ThirdNaicsDesc;
	public String ThirdSic;
	public String ThirdSic8;
	public String ThirdSic8Desc;
	public String ThirdSicDesc;
	public String TradeStyle1;
	public String TradeStyle2;
	public String TradeStyle3;
	public String TradeStyle4;
	public String TradeStyle5;
	public String URL;
	public String UsTaxId;
	public String WomenOwned;
	public String YearStarted;

	public Account[] Accounts;
	public Lead[] Leads;
}
