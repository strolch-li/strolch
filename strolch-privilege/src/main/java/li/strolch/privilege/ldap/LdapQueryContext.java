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
import li.strolch.privilege.helper.GroupsAndRoles;
import li.strolch.privilege.helper.RemoteGroupMappingModel;
import li.strolch.privilege.model.UserState;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.Context;
import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.SearchResult;
import java.util.Hashtable;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import static li.strolch.utils.LdapHelper.ldapAttributesToString;
import static li.strolch.utils.helper.StringHelper.*;

public abstract class LdapQueryContext {

	private static final Logger logger = LoggerFactory.getLogger(LdapQueryContext.class);

	public static final String PARAM_SVC_USER_BINDING = "svcUserBinding";
	public static final String PARAM_SVC_USER_PASSWORD = "svcUserPassword";
	public static final String PARAM_PROVIDER_URL = "providerUrl";
	public static final String PARAM_SEARCH_BASE = "searchBase";
	public static final String PARAM_ADDITIONAL_FILTER = "additionalFilter";
	public static final String PARAM_DEFAULT_LOCALE = "defaultLocale";
	public static final String PARAM_OVERRIDE_USER_IDENTIFIER = "overrideUserIdentifier";
	public static final String PARAM_OVERRIDE_USER_CLASS = "overrideUserClass";
	public static final String PARAM_GROUP_PREFIX_FILTER = "groupPrefixFilter";

	public static final String LDAP_SN = "sn";
	public static final String LDAP_GIVEN_NAME = "givenName";

	protected final String svcUserBinding;
	protected final String svcUserPassword;

	protected final String providerUrl;
	protected final String searchBase;
	protected final String additionalFilter;
	protected final Locale defaultLocale;
	protected final RemoteGroupMappingModel groupMappingModel;

	protected final String overrideUserIdentifier;
	protected final String overrideUserClass;
	protected final String groupPrefixFilter;

	public LdapQueryContext(Map<String, String> parameterMap, RemoteGroupMappingModel groupMappingModel) {
		this.providerUrl = trimOrEmpty(parameterMap.get(PARAM_PROVIDER_URL));
		this.searchBase = trimOrEmpty(parameterMap.get(PARAM_SEARCH_BASE));
		this.additionalFilter = trimOrEmpty(parameterMap.get(PARAM_ADDITIONAL_FILTER));
		this.defaultLocale = parameterMap.containsKey(PARAM_DEFAULT_LOCALE) ?
				Locale.forLanguageTag(parameterMap.get(PARAM_DEFAULT_LOCALE)) : Locale.getDefault();

		this.svcUserBinding = trimOrEmpty(parameterMap.getOrDefault(PARAM_SVC_USER_BINDING, ""));
		this.svcUserPassword = trimOrEmpty(parameterMap.getOrDefault(PARAM_SVC_USER_PASSWORD, ""));

		this.groupMappingModel = groupMappingModel;

		logger.info("providerUrl: {}", this.providerUrl);
		logger.info("searchBase: {}", this.searchBase);
		if (!this.additionalFilter.isEmpty())
			logger.info("additionalFilter: {}", this.additionalFilter);

		this.overrideUserIdentifier = trimOrEmpty(parameterMap.get(PARAM_OVERRIDE_USER_IDENTIFIER));
		this.overrideUserClass = trimOrEmpty(parameterMap.get(PARAM_OVERRIDE_USER_CLASS));
		this.groupPrefixFilter = trimOrEmpty(parameterMap.get(PARAM_GROUP_PREFIX_FILTER));
	}

	public String buildUserDn(String username) {
		String identifier = getUserAttributeIdentifier();
		String bindDn = getSearchBase();
		return String.format("%s=%s,%s", identifier, username, bindDn);
	}

	public String getProviderUrl() {
		return this.providerUrl;
	}

	public String getSearchBase() {
		return this.searchBase;
	}

	public String getAdditionalFilter() {
		return this.additionalFilter;
	}

	public Locale getDefaultLocale() {
		return this.defaultLocale;
	}

	public abstract LdapQuery getLdapQuery();

	protected abstract String getObjectClassFilter();

	protected abstract String getUserAttributeIdentifier();

	protected abstract String getDepartment(Attributes attrs) throws NamingException;

	public abstract Set<String> getLdapGroups(Attributes attrs) throws NamingException;

	protected abstract Locale getLocale(Attributes attrs) throws NamingException;

	protected String getFirstName(String username, Attributes attrs) throws NamingException {
		String value = getLdapString(attrs, LDAP_GIVEN_NAME);
		return isEmpty(value) ? username : value;
	}

	protected String getLastName(String username, Attributes attrs) throws NamingException {
		String value = getLdapString(attrs, LDAP_SN);
		return isEmpty(value) ? username : value;
	}

	public abstract String getLoginUsername(String safeUsername);

	protected abstract String validateLdapUsername(String username, Attributes attrs) throws NamingException;

	protected String getLdapString(Attributes attrs, String key) throws NamingException {
		Attribute sn = attrs.get(key);
		return sn == null ? null : sn.get().toString();
	}

	public User buildUserFromSearchResult(String username, SearchResult searchResult) throws Exception {
		Attributes attrs = searchResult.getAttributes();

		username = validateLdapUsername(username, attrs);

		String firstName = getFirstName(username, attrs);
		String lastName = getLastName(username, attrs);
		Locale locale = getLocale(attrs);

		// evaluate groups and roles for this user
		Set<String> originalLdapGroups = getLdapGroups(attrs);
		if (originalLdapGroups.isEmpty())
			throw new AccessDeniedException(
					"User %s can not login, as no LDAP groups could be evaluated! Attributes:\n%s".formatted(username,
							ldapAttributesToString(attrs)));

		Set<String> ldapGroups = this.groupMappingModel.getUserGroupOverride(username, originalLdapGroups);
		GroupsAndRoles groupsAndRoles = this.groupMappingModel.mapRemoteGroupsToStrolch(ldapGroups);

		if (groupsAndRoles.isEmpty()) {
			logger.error("User {} can not login, as no group or role mappings were found.", username);
			logger.info("User {} is member of the following LDAP groups: ", username);
			ldapGroups.forEach(s -> logger.info("  {}", s));
			throw new AccessDeniedException(
					"User %s can not login, as no group or role mappings were found for ldap groups %s".formatted(
							username, ldapGroups));
		}

		// first see if we can find the primaryLocation from the department attribute:
		String department = getDepartment(attrs);
		Map<String, String> properties = this.groupMappingModel.buildProperties(department, ldapGroups);

		return new User(username, username, null, firstName, lastName, UserState.REMOTE, groupsAndRoles.groups(),
				groupsAndRoles.roles(), locale, properties, false, UserHistory.EMPTY);
	}

	public Hashtable<String, String> buildLdapEnv(char[] password, String userPrincipalName) {

		// Set up the environment for creating the initial context
		Hashtable<String, String> env = new Hashtable<>();

		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
		env.put(Context.PROVIDER_URL, this.providerUrl);

		// Authenticate
		env.put(Context.SECURITY_AUTHENTICATION, "simple");
		env.put(Context.SECURITY_PRINCIPAL, userPrincipalName);
		env.put(Context.SECURITY_CREDENTIALS, new String(password));
		env.put(Context.REFERRAL, "ignore");
		return env;
	}

	public boolean isServiceUserDefined() {
		return isNotEmpty(this.svcUserBinding) && isNotEmpty(this.svcUserPassword);
	}

	public Hashtable<String, String> buildServiceUserLdapEnv() {
		if (isEmpty(this.svcUserBinding) || isEmpty(this.svcUserPassword))
			throw new IllegalStateException("Service user binding and/or password not set!");

		// Set up the environment for creating the initial context
		Hashtable<String, String> env = new Hashtable<>();

		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
		env.put(Context.PROVIDER_URL, this.providerUrl);

		// Authenticate
		env.put(Context.SECURITY_AUTHENTICATION, "simple");
		env.put(Context.SECURITY_PRINCIPAL, this.svcUserBinding);
		env.put(Context.SECURITY_CREDENTIALS, this.svcUserPassword);
		env.put(Context.REFERRAL, "ignore");
		return env;
	}
}
