/*
 * Copyright (c) 2024 Robert von Burg <eitch@eitchnet.ch>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package li.strolch.privilege.ldap;

import li.strolch.privilege.base.AccessDeniedException;
import li.strolch.privilege.helper.RemoteGroupMappingModel;
import li.strolch.utils.dbc.DBC;

import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import static li.strolch.utils.LdapHelper.encodeForLDAP;
import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.StringHelper.isNotEmpty;

public class LinuxLdapQueryContext extends LdapQueryContext {

	public static final String LDAP_DEPARTMENT_NUMBER = "departmentNumber";
	public static final String LDAP_PREFERRED_LANGUAGE = "preferredLanguage";
	public static final String LDAP_ENTRY_UUID = "entryUUID";

	public LinuxLdapQueryContext(Map<String, String> parameterMap, RemoteGroupMappingModel groupMappingModel) {
		super(parameterMap, groupMappingModel);
	}

	@Override
	public LdapQuery getLdapQuery() {
		return new LinuxLdapQuery(this);
	}

	public String getLoginUsername(String username) {
		// escape the user provider username
		return encodeForLDAP(username, true);
	}

	@Override
	protected String validateLdapUsername(String username, Attributes attrs) throws NamingException {
		Attribute accountName = attrs.get(getUserAttributeIdentifier());
		if (accountName == null || !username.equalsIgnoreCase(accountName.get().toString()))
			throw new AccessDeniedException("Could not login with user: " + username + " on Ldap: Wrong LDAP Data");

		return accountName.get().toString();
	}

	@Override
	public String getObjectClassFilter() {
		if (isNotEmpty(this.overrideUserClass))
			return this.overrideUserClass;
		return "(objectClass=inetOrgPerson)";
	}

	@Override
	public String getUserAttributeIdentifier() {
		if (isNotEmpty(this.overrideUserIdentifier))
			return this.overrideUserIdentifier;
		return "uid";
	}

	@Override
	protected String getDepartment(Attributes attrs) throws NamingException {
		return getLdapString(attrs, LDAP_DEPARTMENT_NUMBER);
	}

	@Override
	public Set<String> getLdapGroups(Attributes attrs) throws NamingException {
		Attribute ou = attrs.get("ou");
		if (ou == null)
			return Set.of();
		Set<String> groups = new HashSet<>();
		NamingEnumeration<?> all = ou.getAll();
		while (all.hasMore()) {
			String group = all.next().toString();
			if (isEmpty(this.groupPrefixFilter) || group.startsWith(this.groupPrefixFilter))
				groups.add(group);
		}

		return groups;
	}

	@Override
	protected Locale getLocale(Attributes attrs) throws NamingException {
		String preferredLanguage = getLdapString(attrs, LDAP_PREFERRED_LANGUAGE);
		return isEmpty(preferredLanguage) ? this.defaultLocale : Locale.forLanguageTag(preferredLanguage);
	}

	@Override
	protected String getUserId(Attributes attrs) throws NamingException {
		String value = getLdapString(attrs, LDAP_ENTRY_UUID);
		DBC.PRE.assertNotEmpty("LDAP field " + LDAP_ENTRY_UUID + " is empty!", value);
		return value;
	}
}
