/*
 * Copyright (c) 2013-2024 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.privilege.handler;

import li.strolch.privilege.base.PrivilegeConflictResolution;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.base.PrivilegeModelException;
import li.strolch.privilege.model.*;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.iso8601.ISO8601;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.ZonedDateTime;
import java.util.*;

import static java.text.MessageFormat.format;
import static java.util.stream.Collectors.toCollection;
import static li.strolch.privilege.base.PrivilegeConstants.*;
import static li.strolch.privilege.helper.ModelHelper.streamAllRolesForUser;

public class PrivilegeContextBuilder {
	protected static final Logger logger = LoggerFactory.getLogger(PrivilegeContextBuilder.class);

	protected final Map<String, Class<PrivilegePolicy>> policyMap;
	protected final DefaultPrivilegeHandler privilegeHandler;
	protected final PrivilegeConflictResolution conflictResolution;
	protected final PersistenceHandler persistenceHandler;

	protected Set<String> groups;
	protected Set<String> userDirectRoles;
	protected Set<String> rolesWithGroupRoles;
	protected Map<String, String> properties;

	public PrivilegeContextBuilder(DefaultPrivilegeHandler privilegeHandler) {
		this.privilegeHandler = privilegeHandler;
		this.persistenceHandler = privilegeHandler.persistenceHandler;
		this.policyMap = privilegeHandler.policyMap;
		this.conflictResolution = privilegeHandler.privilegeConflictResolution;
	}

	public PrivilegeContext buildPrivilegeContext(Usage usage, User user, String source, ZonedDateTime loginTime,
			boolean keepAlive) {
		String authToken = this.privilegeHandler.getEncryptionHandler().nextToken();
		String sessionId = UUID.randomUUID().toString();
		return buildPrivilegeContext(usage, user, authToken, sessionId, source, loginTime, keepAlive);
	}

	public PrivilegeContext buildPrivilegeContext(Usage usage, User user, String authToken, String sessionId,
			String source, ZonedDateTime loginTime, boolean keepAlive) {
		DBC.PRE.assertNotEmpty("source must not be empty!", source);

		keepAlive = keepAlive && this.privilegeHandler.allowSessionRefresh;

		prepare(user);

		Map<String, Privilege> privileges = new HashMap<>();
		Map<String, PrivilegePolicy> policies = new HashMap<>();

		// cache the privileges and policies for this user by role
		addPrivilegesForRoles(this.rolesWithGroupRoles, user.getUsername(), privileges, policies);

		Certificate certificate = new Certificate(usage, sessionId, user.getUsername(), user.getFirstname(),
				user.getLastname(), user.getUserState(), authToken, source, loginTime, keepAlive, user.getLocale(),
				this.groups, this.rolesWithGroupRoles, this.userDirectRoles, this.properties);

		return new PrivilegeContext(certificate, privileges, policies);
	}

	public UserPrivileges buildUserPrivilege(User user) {
		prepare(user);

		// cache the privileges and policies for this user by role
		Map<String, Privilege> privileges = new HashMap<>();
		addPrivilegesForRoles(this.rolesWithGroupRoles, user.getUsername(), privileges, new HashMap<>());

		return new UserPrivileges(user.asUserRep(), List.copyOf(privileges.values()));
	}

	public GroupPrivileges buildGroupPrivilege(Group group) {
		Set<String> groupRoles = group.roles().stream().sorted().collect(toCollection(TreeSet::new));

		// cache the privileges and policies for this group by role
		Map<String, Privilege> privileges = new HashMap<>();
		addPrivilegesForRoles(groupRoles, group.name(), privileges, new HashMap<>());

		return new GroupPrivileges(group, List.copyOf(privileges.values()));
	}

	protected void prepare(User user) {
		Set<Group> groups = user
				.getGroups()
				.stream()
				.sorted()
				.map(this::getGroup)
				.filter(Objects::nonNull)
				.filter(this::isGroupActive)
				.collect(toCollection(TreeSet::new));
		this.groups = groups.stream().map(Group::name).collect(toCollection(TreeSet::new));
		this.userDirectRoles = user.getRoles().stream().sorted().collect(toCollection(TreeSet::new));
		this.rolesWithGroupRoles = streamAllRolesForUser(this.persistenceHandler, user)
				.sorted()
				.collect(toCollection(TreeSet::new));
		this.properties = new HashMap<>(user.getProperties());

		// copy properties from groups to user properties
		copyGroupProperties(user, groups);
	}

	protected boolean isGroupActive(Group group) {
		ZonedDateTime now = ZonedDateTime.now();
		if (group.hasProperty(VALID_FROM) && now.isBefore(ISO8601.parseToZdt(group.getProperty(VALID_FROM))))
			return false;
		return !group.hasProperty(VALID_TO) || now.isBefore(ISO8601.parseToZdt(group.getProperty(VALID_TO)));
	}

	protected Group getGroup(String groupName) {
		Group group = this.persistenceHandler.getGroup(groupName);
		if (group != null)
			return group;
		logger.error("Group {} does not exist!", groupName);
		return null;
	}

	protected void copyGroupProperties(User user, Set<Group> groups) {
		for (Group group : groups) {
			Map<String, String> groupProperties = group.getProperties();
			for (String key : groupProperties.keySet()) {

				// we have special handling for certain duplicate group properties
				if (this.properties.containsKey(key)) {
					if (handleDuplicateGroupProperty(key, group))
						continue;
				}

				String value = groupProperties.get(key);
				String replaced = this.properties.put(key, value);
				if (replaced != null && !replaced.equals(value)) {
					logger.error("Duplicate property {} for user {} from group {} replaced: {} with: {}", key,
							user.getUsername(), group.name(), replaced, value);
				}
			}
		}
	}

	protected boolean handleDuplicateGroupProperty(String key, Group group) {
		if (!key.equals(LOCATION))
			return false;

		String currentValue = this.properties.get(key);
		String groupValue = group.getProperty(key);

		this.properties.put(key, currentValue + "," + groupValue);

		return true;
	}

	protected void addPrivilegesForRoles(Set<String> roles, String name, Map<String, Privilege> privileges,
			Map<String, PrivilegePolicy> policies) {

		for (String roleName : roles) {
			Role role = this.persistenceHandler.getRole(roleName);
			if (role == null) {
				logger.error("Role {} does not exist for user/group {}", roleName, name);
			} else {
				addPrivilegesForRole(name, role, privileges, policies);
			}
		}
	}

	protected void addPrivilegesForRole(String name, Role role, Map<String, Privilege> privileges,
			Map<String, PrivilegePolicy> policies) {

		for (Privilege privilege : role.privilegeMap().values()) {
			String privilegeName = privilege.name();

			if (!privileges.containsKey(privilegeName)) {
				privileges.put(privilegeName, privilege);
			} else {
				handleDuplicatePrivilege(name, role, privileges, privilege, privilegeName);
			}

			// cache the policy for the privilege
			addPolicyForPrivilege(policies, privilege, privilegeName);
		}
	}

	protected void addPolicyForPrivilege(Map<String, PrivilegePolicy> policies, Privilege privilege,
			String privilegeName) {
		String policyName = privilege.getPolicy();
		if (policies.containsKey(policyName))
			return;

		PrivilegePolicy policy = getPolicy(policyName);
		if (policy == null) {
			logger.error("The Policy {} does not exist for Privilege {}", policyName, privilegeName);
		} else {
			policies.put(policyName, policy);
		}
	}

	protected void handleDuplicatePrivilege(String name, Role role, Map<String, Privilege> privileges,
			Privilege additionalPrivilege, String privilegeName) {

		// for strict, we have to throw an exception
		if (this.conflictResolution.isStrict())
			throw new PrivilegeModelException(
					format("User/Group " + name + " has conflicts for privilege {0} with role {1}", privilegeName,
							role.name()));

		// merge privileges
		Privilege knownPrivilege = privileges.get(privilegeName);
		boolean allAllowed = knownPrivilege.isAllAllowed() || additionalPrivilege.isAllAllowed();
		Set<String> allowList;
		Set<String> denyList;
		if (allAllowed) {
			allowList = Set.of();
			denyList = Set.of();
		} else {
			allowList = new HashSet<>(knownPrivilege.getAllowList());
			allowList.addAll(additionalPrivilege.getAllowList());
			denyList = new HashSet<>(knownPrivilege.getDenyList());
			denyList.addAll(additionalPrivilege.getDenyList());
		}

		String policy = knownPrivilege.getPolicy();
		privileges.put(privilegeName, new Privilege(knownPrivilege.getName(), policy, allAllowed, denyList, allowList));
	}

	/**
	 * <p>
	 * This method instantiates a {@link PrivilegePolicy} object from the given policyName. The {@link PrivilegePolicy}
	 * is not stored in a database. The privilege name is a class name and is then used to instantiate a new
	 * {@link PrivilegePolicy} object
	 * </p>
	 *
	 * @param policyName the class name of the {@link PrivilegePolicy} object to return
	 *
	 * @return the {@link PrivilegePolicy} object
	 *
	 * @throws PrivilegeException if the {@link PrivilegePolicy} object for the given policy name could not be
	 *                            instantiated
	 */
	protected PrivilegePolicy getPolicy(String policyName) {

		// get the policies class
		Class<PrivilegePolicy> policyClazz = this.policyMap.get(policyName);
		if (policyClazz == null)
			return null;

		// instantiate the policy
		PrivilegePolicy policy;
		try {
			policy = policyClazz.getConstructor().newInstance();
		} catch (Exception e) {
			String msg = "The class for the policy with the name {0} does not exist!{1}";
			msg = format(msg, policyName, policyName);
			throw new PrivilegeModelException(msg, e);
		}

		policy.initialize(this.privilegeHandler);
		return policy;
	}
}
