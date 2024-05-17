package li.strolch.privilege.handler;

import li.strolch.privilege.base.PrivilegeConflictResolution;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.base.PrivilegeModelException;
import li.strolch.privilege.model.*;
import li.strolch.privilege.model.internal.Group;
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

	private Set<String> userGroups;
	private Set<String> userRoles;
	private Map<String, String> userProperties;

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
		addPrivilegesForRoles(this.userRoles, user.getUsername(), privileges, policies);

		UserRep userRep = user.asUserRep();
		userRep.setRoles(this.userRoles);
		userRep.setGroups(this.userGroups);
		userRep.setProperties(this.userProperties);
		userRep.readOnly();

		Certificate certificate = new Certificate(usage, sessionId, user.getUsername(), user.getFirstname(),
				user.getLastname(), user.getUserState(), authToken, source, loginTime, keepAlive, user.getLocale(),
				this.userGroups, this.userRoles, this.userProperties);

		return new PrivilegeContext(userRep, certificate, privileges, policies);
	}

	private void prepare(User user) {
		this.userGroups = user.getGroups().stream().sorted().collect(toCollection(TreeSet::new));
		this.userRoles = streamAllRolesForUser(this.persistenceHandler, user).sorted()
				.collect(toCollection(TreeSet::new));
		this.userProperties = new HashMap<>(user.getProperties());

		// copy properties from groups to user properties
		for (String groupName : this.userGroups) {
			Group group = this.persistenceHandler.getGroup(groupName);
			if (group == null) {
				logger.error("Group {} does not exist!", groupName);
				continue;
			}
			Map<String, String> groupProperties = group.getProperties();
			for (String key : groupProperties.keySet()) {
				String replaced = this.userProperties.put(key, groupProperties.get(key));
				if (replaced != null)
					logger.error("Duplicate property {} for user {} from group {}", key, user.getUsername(), groupName);
			}
		}
	}

	private void addPrivilegesForRoles(Set<String> roles, String userName, Map<String, Privilege> privileges,
			Map<String, PrivilegePolicy> policies) {

		for (String roleName : roles) {
			Role role = this.persistenceHandler.getRole(roleName);
			if (role == null) {
				logger.error("Role {} does not exist for user {}", roleName, userName);
			} else {
				addPrivilegesForRole(userName, role, privileges, policies);
			}
		}
	}

	private void addPrivilegesForRole(String userName, Role role, Map<String, Privilege> privileges,
			Map<String, PrivilegePolicy> policies) {

		for (Privilege privilege : role.privilegeMap().values()) {
			String privilegeName = privilege.name();

			if (!privileges.containsKey(privilegeName)) {
				privileges.put(privilegeName, privilege);
			} else {
				handleDuplicatePrivilege(userName, role, privileges, privilege, privilegeName);
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
			logger.error(format("The Policy {0} does not exist for Privilege {1}", policyName, privilegeName));
		} else {
			policies.put(policyName, policy);
		}
	}

	private void handleDuplicatePrivilege(String userName, Role role, Map<String, Privilege> privileges,
			Privilege additionalPrivilege, String privilegeName) {

		// for strict, we have to throw an exception
		if (this.conflictResolution.isStrict())
			throw new PrivilegeModelException(
					format("User " + userName + " has conflicts for privilege {0} with role {1}", privilegeName,
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
		if (policyClazz == null) {
			return null;
		}

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
