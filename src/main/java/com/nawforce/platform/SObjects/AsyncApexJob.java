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
import com.nawforce.platform.System.Integer;
import com.nawforce.platform.System.String;
import com.nawforce.platform.System.*;


@SuppressWarnings("unused")
public class AsyncApexJob extends SObject {
	public static SObjectType$<AsyncApexJob> SObjectType;
	public static SObjectTypeFields$<AsyncApexJob> Fields;
	public static SObjectTypeFieldSets$<AsyncApexJob> FieldSets;
	public Id ApexClassId;
	public ApexClass ApexClass;
	public Datetime CompletedDate;
	public String ExtendedStatus;
	public com.nawforce.platform.System.Integer JobItemsProcessed;
	public String JobType;
	public String LastProcessed;
	public com.nawforce.platform.System.Integer LastProcessedOffset;
	public String MethodName;
	public com.nawforce.platform.System.Integer NumberOfErrors;
	public Id ParentJobId;
	public AsyncApexJob ParentJob;
	public String Status;
	public Integer TotalJobItems;

	public ApexTestRunResult[] AsyncApex;
}
