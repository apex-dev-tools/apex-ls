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
import com.nawforce.platform.System.Id;
import com.nawforce.platform.System.SObject;
import com.nawforce.platform.System.String;

@SuppressWarnings("unused")
public class AuthProvider extends SObject {
	public static SObjectType$<AuthProvider> SObjectType;
	public static SObjectTypeFields$<AuthProvider> Fields;
	public static SObjectTypeFieldSets$<AuthProvider> FieldSets;
	public String AuthorizeUrl;
	public String ConsumerKey;
	public String ConsumerSecret;
	public String CustomMetadataTypeRecord;
	public String DefaultScopes;
	public String DeveloperName;
	public String ErrorUrl;
	public Id ExecutionUserId;
	public User ExecutionUser;
	public String FriendlyName;
	public String IconUrl;
	public String IdTokenIssuer;
	public String LinkKickoffUrl;
	public String LogoutUrl;
	public String OauthKickoffUrl;
	public Boolean OptionsIncludeOrgIdInId;
	public Boolean OptionsSendAccessTokenInHeader;
	public Boolean OptionsSendClientCredentialsInHeader;
	public Id PluginId;
	public ApexClass Plugin;
	public String ProviderType;
	public Id RegistrationHandlerId;
	public ApexClass RegistrationHandler;
	public String SsoKickoffUrl;
	public String TokenUrl;
	public String UserInfoUrl;
}
