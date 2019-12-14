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
import com.nawforce.platform.System.Boolean;
import com.nawforce.platform.System.Integer;
import com.nawforce.platform.System.String;
import com.nawforce.platform.System.*;


@SuppressWarnings("unused")
public class Event extends SObject {
	public static SObjectType$<Event> SObjectType;
	public static SObjectFields$<Event> Fields;

	public Id AccountId;
	public Account Account;
	public Date ActivityDate;
	public Datetime ActivityDateTime;
	public Id CreatedById;
	public User CreatedBy;
	public Datetime CreatedDate;
	public String CurrencyIsoCode;
	public String Description;
	public Integer DurationInMinutes;
	public Datetime EndDateTime;
	public String EventSubtype;
	public String GroupEventType;
	public Id Id;
	public Boolean IsAllDayEvent;
	public Boolean IsArchived;
	public Boolean IsChild;
	public Boolean IsDeleted;
	public Boolean IsGroupEvent;
	public Boolean IsPrivate;
	public Boolean IsRecurrence;
	public Boolean IsRecurrence2;
	public Boolean IsRecurrence2Exception;
	public Boolean IsRecurrence2Exclusion;
	public Boolean IsReminderSet;
	public Id LastModifiedById;
	public User LastModifiedBy;
	public Datetime LastModifiedDate;
	public String Location;
	public Id OwnerId;
	public User Owner;
	public Datetime Recurrence2PatternStartDate;
	public String Recurrence2PatternText;
	public String Recurrence2PatternTimeZone;
	public String Recurrence2PatternVersion;
	public Id RecurrenceActivityId;
	public Event RecurrenceActivity;
	public Integer RecurrenceDayOfMonth;
	public Integer RecurrenceDayOfWeekMask;
	public Date RecurrenceEndDateOnly;
	public String RecurrenceInstance;
	public Integer RecurrenceInterval;
	public String RecurrenceMonthOfYear;
	public Datetime RecurrenceStartDateTime;
	public String RecurrenceTimeZoneSidKey;
	public String RecurrenceType;
	public Datetime ReminderDateTime;
	public String ShowAs;
	public Datetime StartDateTime;
	public String Subject;
	public Datetime SystemModstamp;
	public Id WhatId;
	public Account What;
	public Id WhoId;
	public Contact Who;

	public AcceptedEventRelation[] AcceptedEventRelations;
	public AttachedContentDocument[] AttachedContentDocuments;
	public Attachment[] Attachments;
	public CombinedAttachment[] CombinedAttachments;
	public ContentDocumentLink[] ContentDocumentLinks;
	public DeclinedEventRelation[] DeclinedEventRelations;
	public EventRelation[] EventRelations;
	public EntitySubscription[] FeedSubscriptionsForEntity;
	public EventFeed[] Feeds;
	public Event[] RecurringEvents;
	public TopicAssignment[] TopicAssignments;
	public UndecidedEventRelation[] UndecidedEventRelations;
}
