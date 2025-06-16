/*
 * Copyright (c) 2024-2025 Robert von Burg <eitch@eitchnet.ch>
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.ldap.LdapName;
import javax.naming.ldap.Rdn;
import java.nio.ByteBuffer;
import java.util.*;

import static li.strolch.utils.LdapHelper.encodeForLDAP;
import static li.strolch.utils.helper.StringHelper.*;

public class WindowsLdapQueryContext extends LdapQueryContext {

	private static final Logger logger = LoggerFactory.getLogger(WindowsLdapQueryContext.class);

	public static final String PARAM_DOMAIN = "domain";
	public static final String PARAM_USE_BASE_DN_FOR_DISTINGUISHED_NAME = "useBaseDnForDistinguishedName";

	public static final String LDAP_DEPARTMENT = "department";
	public static final String LDAP_MEMBER_OF = "memberOf";
	public static final String LDAP_CN = "CN";
	public static final String LDAP_SAM_ACCOUNT_NAME = "sAMAccountName";
	public static final String LDAP_USER_PRINCIPAL_NAME = "userPrincipalName";
	public static final String LDAP_OBJECT_GUID = "objectGUID";

	protected final String domain;
	protected final String domainPrefix;

	protected final boolean useBaseDnForDistinguishedName;

	public WindowsLdapQueryContext(Map<String, String> parameterMap, RemoteGroupMappingModel groupMappingModel) {
		super(parameterMap, groupMappingModel);

		String domain = trimOrEmpty(parameterMap.get(PARAM_DOMAIN));
		String domainPrefix = null;
		if (!domain.isEmpty()) {
			if (domain.startsWith("@")) {
				logger.warn(
						"Remove the @ symbol from the domain property! Will be added automatically where required.");
				domain = domain.substring(1);
			}

			domainPrefix = domain + '\\';

			logger.info("domain: {}", domain);
			logger.info("domain prefix: {}", domainPrefix);
		}
		this.domain = domain;
		this.domainPrefix = domainPrefix;
		this.useBaseDnForDistinguishedName = Boolean.parseBoolean(
				parameterMap.get(PARAM_USE_BASE_DN_FOR_DISTINGUISHED_NAME));
	}

	@Override
	public Hashtable<String, String> buildLdapEnv(char[] password, String userPrincipalName) {
		Hashtable<String, String> env = super.buildLdapEnv(password, userPrincipalName);
		env.put("java.naming.ldap.attributes.binary", LDAP_OBJECT_GUID);
		return env;
	}

	@Override
	public LdapQuery getLdapQuery() {
		return new WindowsLdapQuery(this);
	}

	public String getDomain() {
		return domain;
	}

	public String getDomainPrefix() {
		return domainPrefix;
	}

	@Override
	public String getObjectClassFilter() {
		if (isNotEmpty(this.overrideUserClass))
			return this.overrideUserClass;
		return "(objectCategory=person)(objectClass=user)";
	}

	@Override
	public String getUserAttributeIdentifier() {
		if (isNotEmpty(this.overrideUserIdentifier))
			return this.overrideUserIdentifier;
		return LDAP_SAM_ACCOUNT_NAME;
	}

	protected String getUserId(Attributes attrs) throws NamingException {
		Attribute attribute = attrs.get(LDAP_OBJECT_GUID);
		if (attribute == null)
			throw new IllegalStateException("LDAP field " + LDAP_OBJECT_GUID + " doesn't exist!");
		byte[] guidBytes = (byte[]) attribute.get();
		if (guidBytes == null)
			throw new IllegalStateException("The value for LDAP field " + LDAP_OBJECT_GUID + " is null!");
		return decodeObjectGuid(guidBytes).toString();
	}

	public static UUID decodeObjectGuid(byte[] bytes) {
		// Thanks to robotdan: https://gist.github.com/davidmc24/0588900f3200eba3ea80?permalink_comment_id=3464812#gistcomment-3464812
		// MSB bytes are the first 8 bytes in this order [3,2,1,0,5,4,7,6]
		long msb = ByteBuffer
				.allocate(8)
				.put(3, bytes[0])
				.put(2, bytes[1])
				.put(1, bytes[2])
				.put(0, bytes[3])
				.put(5, bytes[4])
				.put(4, bytes[5])
				.put(7, bytes[6])
				.put(6, bytes[7])
				.getLong();

		// LSB are just the last 8 bytes in the same order
		long lsb = ByteBuffer.wrap(bytes, 8, 8).getLong();

		return new UUID(msb, lsb);
	}

	public String getUserAttributeIdentifier1() {
		return LDAP_USER_PRINCIPAL_NAME;
	}

	@Override
	public String getLoginUsername(String safeUsername) {
		if (this.useBaseDnForDistinguishedName)
			return buildUserDn(safeUsername);

		if (this.domain.isEmpty())
			return safeUsername;

		if (!this.domainPrefix.isEmpty() && safeUsername.startsWith(this.domainPrefix)) {
			logger.warn("Trimming domain from given username, to first search in sAMAccountName");
			safeUsername = encodeForLDAP(safeUsername.substring(this.domainPrefix.length()), true);
		}

		return safeUsername + "@" + this.domain;
	}

	@Override
	protected String validateLdapUsername(String username, Attributes attrs) throws NamingException {
		Attribute accountName = attrs.get(getUserAttributeIdentifier());
		if (accountName == null || !username.equalsIgnoreCase(accountName.get().toString()))
			throw new AccessDeniedException(
					"Could not login with user: " + username + this.domain + " on Ldap: Wrong LDAP Data");

		return accountName.get().toString();
	}

	@Override
	public String getDepartment(Attributes attrs) throws NamingException {
		return getLdapString(attrs, LDAP_DEPARTMENT);
	}

	@Override
	public Locale getLocale(Attributes attrs) {
		return this.defaultLocale;
	}

	@Override
	public Set<String> getLdapGroups(Attributes attrs) throws NamingException {
		Set<String> ldapRoles = new HashSet<>();
		Attribute groupMembers = attrs.get(LDAP_MEMBER_OF);
		if (groupMembers == null)
			return ldapRoles;

		for (int i = 0; i < groupMembers.size(); i++) {
			String memberOfLdapString = attrs.get(LDAP_MEMBER_OF).get(i).toString();

			// extract group name from ldap string -> CN=groupname,OU=company,DC=domain,DC=country
			LdapName memberOfName = new LdapName(memberOfLdapString);
			for (Rdn rdn : memberOfName.getRdns()) {
				if (rdn.getType().equalsIgnoreCase(LDAP_CN)) {
					String groupName = rdn.getValue().toString();
					if (isEmpty(this.groupPrefixFilter) || groupName.startsWith(this.groupPrefixFilter))
						ldapRoles.add(groupName);
					break;
				}
			}
		}

		return ldapRoles;
	}
}
