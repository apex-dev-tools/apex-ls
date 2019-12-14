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

import com.nawforce.platform.Schema.SObjectField;

@SuppressWarnings("unused")
public class Exception {
	public Exception getCause() {throw new java.lang.UnsupportedOperationException();}
	public Integer getLineNumber() {throw new java.lang.UnsupportedOperationException();}
	public String getMessage() {throw new java.lang.UnsupportedOperationException();}
	public String getStackTraceString() {throw new java.lang.UnsupportedOperationException();}
	public String getTypeName() {throw new java.lang.UnsupportedOperationException();}
	public void initCause(Exception cause) {throw new java.lang.UnsupportedOperationException();}
	public void setMessage(String message) {throw new java.lang.UnsupportedOperationException();}

	// These should be on DMLException & EmailException but you can access via Exception
	public List<String> getDmlFieldNames(Integer index) {throw new java.lang.UnsupportedOperationException();}
	public List<SObjectField> getDmlFields(Integer index) {throw new java.lang.UnsupportedOperationException();}
	public String getDmlId(Integer index) {throw new java.lang.UnsupportedOperationException();}
	public Integer getDmlIndex(Integer index) {throw new java.lang.UnsupportedOperationException();}
	public String getDmlMessage(Integer index) {throw new java.lang.UnsupportedOperationException();}
	public String getDmlStatusCode(Integer index) {throw new java.lang.UnsupportedOperationException();}
	public StatusCode getDmlType(Integer index) {throw new java.lang.UnsupportedOperationException();}
	public Integer getNumDml() {throw new java.lang.UnsupportedOperationException();}
}
