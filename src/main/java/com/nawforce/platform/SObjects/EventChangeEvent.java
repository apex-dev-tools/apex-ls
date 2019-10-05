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
import com.nawforce.platform.System.String;
import com.nawforce.platform.System.*;


@SuppressWarnings("unused")
public class EventChangeEvent extends SObject {
	public static SObjectType$<EventChangeEvent> SObjectType;
	public static SObjectTypeFields$<EventChangeEvent> Fields;
	public static SObjectTypeFieldSets$<EventChangeEvent> FieldSets;
	public Id AccountId;
	public Account Account;
	public Date ActivityDate;
	public Datetime ActivityDateTime;
	public Object ChangeEventHeader;
	public com.nawforce.platform.System.String CurrencyIsoCode;
	public com.nawforce.platform.System.String Description;
	public com.nawforce.platform.System.Integer DurationInMinutes;
	public com.nawforce.platform.System.String GroupEventType;
	public com.nawforce.platform.System.Boolean IsAllDayEvent;
	public com.nawforce.platform.System.Boolean IsChild;
	public com.nawforce.platform.System.Boolean IsGroupEvent;
	public com.nawforce.platform.System.Boolean IsPrivate;
	public com.nawforce.platform.System.Boolean IsRecurrence;
	public com.nawforce.platform.System.Boolean IsRecurrence2Exclusion;
	public com.nawforce.platform.System.Boolean IsReminderSet;
	public com.nawforce.platform.System.String Location;
	public Id OwnerId;
	public User Owner;
	public com.nawforce.platform.System.String Recurrence2PatternText;
	public com.nawforce.platform.System.String Recurrence2PatternVersion;
	public Id RecurrenceActivityId;
	public Event RecurrenceActivity;
	public com.nawforce.platform.System.Integer RecurrenceDayOfMonth;
	public com.nawforce.platform.System.Integer RecurrenceDayOfWeekMask;
	public Date RecurrenceEndDateOnly;
	public com.nawforce.platform.System.String RecurrenceInstance;
	public com.nawforce.platform.System.Integer RecurrenceInterval;
	public com.nawforce.platform.System.String RecurrenceMonthOfYear;
	public Datetime RecurrenceStartDateTime;
	public com.nawforce.platform.System.String RecurrenceTimeZoneSidKey;
	public com.nawforce.platform.System.String RecurrenceType;
	public Datetime ReminderDateTime;
	public com.nawforce.platform.System.String ReplayId;
	public com.nawforce.platform.System.String ShowAs;
	public String Subject;
	public Id WhatId;
	public Account What;
	public Id WhoId;
	public Contact Who;
}
