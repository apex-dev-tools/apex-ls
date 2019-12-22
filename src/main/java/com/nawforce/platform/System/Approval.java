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
package com.nawforce.platform.System;

import com.nawforce.platform.Approval.LockResult;
import com.nawforce.platform.Approval.ProcessRequest;
import com.nawforce.platform.Approval.ProcessResult;
import com.nawforce.platform.Approval.UnlockResult;

@SuppressWarnings("unused")
public class Approval {
	public static Boolean isLocked(Id id) {throw new java.lang.UnsupportedOperationException();}
	public static Map<Id, Boolean> isLocked(List<Object> objects) {throw new java.lang.UnsupportedOperationException();}
	public static Boolean isLocked(SObject sobject) {throw new java.lang.UnsupportedOperationException();}
	public static LockResult lock(Id id) {throw new java.lang.UnsupportedOperationException();}
	public static LockResult lock(Id id, Boolean allOrNothing) {throw new java.lang.UnsupportedOperationException();}
	public static List<LockResult> lock(List<Object> objects) {throw new java.lang.UnsupportedOperationException();}
	public static List<LockResult> lock(List<Object> objects, Boolean allOrNothing) {throw new java.lang.UnsupportedOperationException();}
	public static LockResult lock(SObject sobject) {throw new java.lang.UnsupportedOperationException();}
	public static LockResult lock(SObject sobject, Boolean allOrNothing) {throw new java.lang.UnsupportedOperationException();}
	public static List<ProcessResult> process(ProcessRequest approvalRequest) {throw new java.lang.UnsupportedOperationException();}
	public static List<ProcessResult> process(ProcessRequest approvalRequest, Boolean allOrNothing) {throw new java.lang.UnsupportedOperationException();}
	public static List<ProcessResult> process(List<ProcessRequest> approvalRequests) {throw new java.lang.UnsupportedOperationException();}
	public static List<ProcessResult> process(List<ProcessRequest> approvalRequests, Boolean allOrNothing) {throw new java.lang.UnsupportedOperationException();}
	public static UnlockResult unlock(Id id) {throw new java.lang.UnsupportedOperationException();}
	public static UnlockResult unlock(Id id, Boolean allOrNothing) {throw new java.lang.UnsupportedOperationException();}
	public static List<UnlockResult> unlock(List<Object> objects) {throw new java.lang.UnsupportedOperationException();}
	public static List<UnlockResult> unlock(List<Object> objects, Boolean allOrNothing) {throw new java.lang.UnsupportedOperationException();}
	public static UnlockResult unlock(SObject sobject) {throw new java.lang.UnsupportedOperationException();}
	public static UnlockResult unlock(SObject sobject, Boolean allOrNothing) {throw new java.lang.UnsupportedOperationException();}
}
