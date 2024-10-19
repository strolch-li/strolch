package li.strolch.privilege.handler;

import li.strolch.privilege.base.PrivilegeConflictResolution;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.base.PrivilegeModelException;
import li.strolch.privilege.model.*;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.ZonedDateTime;
import java.util.*;

import static java.text.MessageFormat.format;
import static java.util.stream.Collectors.toCollection;
import static li.strolch.privilege.handler.DefaultPrivilegeHandler.streamAllRolesForUser;

public class PrivilegeContextBuilder {
	private static final Logger logger = LoggerFactory.getLogger(PrivilegeContextBuilder.class);

	private final Map<String, Class<PrivilegePolicy>> policyMap;
	private final DefaultPrivilegeHandler privilegeHandler;
	private final PrivilegeConflictResolution conflictResolution;
	private final PersistenceHandler persistenceHandler;

	private Set<String> groups;
	private Set<String> roles;
	private Map<String, String> properties;

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
		addPrivilegesForRoles(this.roles, user.getUsername(), privileges, policies);

		UserRep userRep = user.asUserRep();
		userRep.setRoles(this.roles);
		userRep.setGroups(this.groups);
		userRep.setProperties(this.properties);
		userRep.readOnly();

		Certificate certificate = new Certificate(usage, sessionId, user.getUsername(), user.getFirstname(),
				user.getLastname(), user.getUserState(), authToken, source, loginTime, keepAlive, user.getLocale(),
				this.groups, this.roles, this.properties);

		return new PrivilegeContext(userRep, certificate, privileges, policies);
	}

	public UserPrivileges buildUserPrivilege(UserRep userRep) {
		this.roles = streamAllRolesForUser(this.persistenceHandler, userRep)
				.sorted()
				.collect(toCollection(TreeSet::new));

		// cache the privileges and policies for this user by role
		Map<String, Privilege> privileges = new HashMap<>();
		addPrivilegesForRoles(this.roles, userRep.getUsername(), privileges, new HashMap<>());

		return new UserPrivileges(userRep, List.copyOf(privileges.values()));
	}

	public GroupPrivileges buildGroupPrivilege(Group group) {
		this.roles = group.roles().stream().sorted().collect(toCollection(TreeSet::new));

		// cache the privileges and policies for this group by role
		Map<String, Privilege> privileges = new HashMap<>();
		addPrivilegesForRoles(this.roles, group.name(), privileges, new HashMap<>());

		return new GroupPrivileges(group, List.copyOf(privileges.values()));
	}

	private void prepare(User user) {
		this.groups = user.getGroups().stream().sorted().collect(toCollection(TreeSet::new));
		this.roles = streamAllRolesForUser(this.persistenceHandler, user).sorted().collect(toCollection(TreeSet::new));
		this.properties = new HashMap<>(user.getProperties());

		// copy properties from groups to user properties
		for (String groupName : this.groups) {
			Group group = this.persistenceHandler.getGroup(groupName);
			if (group == null) {
				logger.error("Group {} does not exist!", groupName);
				continue;
			}
			Map<String, String> groupProperties = group.getProperties();
			for (String key : groupProperties.keySet()) {
				String value = groupProperties.get(key);
				String replaced = this.properties.put(key, value);
				if (replaced != null && !replaced.equals(value))
					logger.error("Duplicate property {} for user {} from group {}", key, user.getUsername(), groupName);
			}
		}
	}

	private void addPrivilegesForRoles(Set<String> roles, String name, Map<String, Privilege> privileges,
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

	private void addPrivilegesForRole(String name, Role role, Map<String, Privilege> privileges,
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

	private void addPolicyForPrivilege(Map<String, PrivilegePolicy> policies, Privilege privilege,
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

	private void handleDuplicatePrivilege(String name, Role role, Map<String, Privilege> privileges,
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
	private PrivilegePolicy getPolicy(String policyName) {

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

		return policy;
	}
}
