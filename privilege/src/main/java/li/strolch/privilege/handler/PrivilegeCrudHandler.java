package li.strolch.privilege.handler;

import li.strolch.privilege.base.PrivilegeConflictResolution;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.base.PrivilegeModelException;
import li.strolch.privilege.model.*;
import li.strolch.privilege.model.internal.PasswordCrypt;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.utils.collections.Tuple;

import java.text.MessageFormat;
import java.time.ZonedDateTime;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.text.MessageFormat.format;
import static li.strolch.utils.helper.StringHelper.*;

public class PrivilegeCrudHandler {

	private final DefaultPrivilegeHandler privilegeHandler;
	private final PersistenceHandler persistenceHandler;
	private final Map<String, Class<PrivilegePolicy>> policyMap;
	private final PrivilegeConflictResolution privilegeConflictResolution;

	public PrivilegeCrudHandler(DefaultPrivilegeHandler privilegeHandler, Map<String, Class<PrivilegePolicy>> policyMap,
			PrivilegeConflictResolution privilegeConflictResolution) {
		this.privilegeHandler = privilegeHandler;
		this.persistenceHandler = privilegeHandler.persistenceHandler;
		this.policyMap = policyMap;
		this.privilegeConflictResolution = privilegeConflictResolution;
	}

	public RoleRep getRole(Certificate certificate, String roleName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_GET_ROLE);

		Role role = this.persistenceHandler.getRole(roleName);
		if (role == null)
			return null;

		prvCtx.validateAction(
				new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_GET_ROLE, new Tuple(null, role)));

		return role.asRoleRep();
	}

	public UserRep getUser(Certificate certificate, String username) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_GET_USER);

		User user = this.persistenceHandler.getUser(username);
		if (user == null)
			return null;

		prvCtx.validateAction(
				new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_GET_USER, new Tuple(null, user)));
		return user.asUserRep();
	}

	public Map<String, String> getPolicyDefs(Certificate certificate) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.validateAction(new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_ACTION,
				DefaultPrivilegeHandler.PRIVILEGE_ACTION_GET_POLICIES));

		Map<String, String> policyDef = new HashMap<>(this.policyMap.size());
		for (Map.Entry<String, Class<PrivilegePolicy>> entry : this.policyMap.entrySet()) {
			policyDef.put(entry.getKey(), entry.getValue().getName());
		}
		return policyDef;
	}

	public List<RoleRep> getRoles(Certificate certificate) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_GET_ROLE);

		Stream<Role> rolesStream = this.persistenceHandler.getAllRoles().stream();

		// validate access to each role
		rolesStream = rolesStream.filter(role -> prvCtx.hasPrivilege(
				new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_GET_ROLE, new Tuple(null, role))));

		return rolesStream.map(Role::asRoleRep).collect(Collectors.toList());
	}

	public List<UserRep> getUsers(Certificate certificate) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_GET_USER);

		Stream<User> usersStream = this.persistenceHandler.getAllUsers().stream();

		// validate access to each user
		usersStream = usersStream.filter(user -> prvCtx.hasPrivilege(
				new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_GET_USER, new Tuple(null, user))));

		return usersStream.map(User::asUserRep).collect(Collectors.toList());
	}

	public List<UserRep> queryUsers(Certificate certificate, UserRep selectorRep) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_GET_USER);

		String selUserId = selectorRep.getUserId();
		String selUsername = selectorRep.getUsername();
		String selFirstName = selectorRep.getFirstname();
		String selLastName = selectorRep.getLastname();
		UserState selUserState = selectorRep.getUserState();
		Locale selLocale = selectorRep.getLocale();
		Set<String> selGroups = selectorRep.getGroups();
		Set<String> selRoles = selectorRep.getRoles();
		Map<String, String> selPropertyMap = selectorRep.getProperties();

		List<UserRep> result = new ArrayList<>();
		List<User> allUsers = this.persistenceHandler.getAllUsers();
		for (User user : allUsers) {

			if (!prvCtx.hasPrivilege(
					new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_GET_USER, new Tuple(null, user))))
				continue;

			// selections
			boolean userIdSelected;
			boolean usernameSelected;
			boolean firstNameSelected;
			boolean lastNameSelected;
			boolean userStateSelected;
			boolean localeSelected;
			boolean roleSelected;
			boolean groupSelected;
			boolean propertySelected;

			// userId
			userIdSelected = isEmpty(selUserId) || selUserId.equals(user.getUserId());

			// username
			usernameSelected = isEmpty(selUsername) || selUsername.equals(user.getUsername());

			// firstname
			firstNameSelected = isEmpty(selFirstName) || selFirstName.equals(user.getFirstname());

			// lastname
			lastNameSelected = isEmpty(selLastName) || selLastName.equals(user.getLastname());

			// user state
			userStateSelected = selUserState == null || selUserState.equals(user.getUserState());

			// locale
			localeSelected = selLocale == null || selLocale.equals(user.getLocale());

			// groups
			groupSelected = isSelectedByGroup(selGroups, user.getGroups());

			// roles
			roleSelected = isSelectedByRole(selRoles, user.getRoles());

			// properties
			propertySelected = isSelectedByProperty(selPropertyMap, user.getProperties());

			boolean selected = userIdSelected && usernameSelected && firstNameSelected && lastNameSelected &&
					userStateSelected && localeSelected && groupSelected && roleSelected && propertySelected;

			if (selected)
				result.add(user.asUserRep());
		}

		result.sort(Comparator.comparing(UserRep::getUsername));
		return result;
	}

	/**
	 * Checks if the given properties contains values which are contained in the selectionMap. If the selectionMap is
	 * null or empty, then true is returned. If a key/value pair from the selectionMap is not in the properties, then
	 * false is returned
	 *
	 * @param selectionMap the map defining the expected properties
	 * @param properties   the properties which must be a sub set of selectionMap to have this method return true
	 *
	 * @return If the selectionMap is null or empty, then true is returned. If a key/value pair from the selectionMap is
	 * not in the properties, then false is returned
	 */
	boolean isSelectedByProperty(Map<String, String> selectionMap, Map<String, String> properties) {

		if (selectionMap == null)
			return true;

		if (selectionMap.isEmpty() && properties.isEmpty())
			return true;

		for (String selKey : selectionMap.keySet()) {

			String value = properties.get(selKey);
			if (value == null || !value.equals(selectionMap.get(selKey)))
				return false;
		}

		return true;
	}

	/**
	 * Checks if the given roles contains the given selectionRoles, if this is the case, or selectionRoles is null or
	 * empty, then true is returned, otherwise false
	 *
	 * @param selectionRoles the required roles
	 * @param roles          the roles to check if they contain the selectionRoles
	 *
	 * @return Checks if the given roles contains the given selectionRoles, if this is the case, or selectionRoles is
	 * null or empty, then true is returned, otherwise false
	 */
	boolean isSelectedByRole(Set<String> selectionRoles, Set<String> roles) {
		return selectionRoles == null || roles.containsAll(selectionRoles);
	}

	/**
	 * Checks if the given groups contains the given selectionGroups, if this is the case, or selectionGroups is null or
	 * empty, then true is returned, otherwise false
	 *
	 * @param selectionGroups the required groups
	 * @param groups          the groups to check if they contain the selectionGroups
	 *
	 * @return Checks if the given groups contains the given selectionGroups, if this is the case, or selectionGroups is
	 * null or empty, then true is returned, otherwise false
	 */
	boolean isSelectedByGroup(Set<String> selectionGroups, Set<String> groups) {
		return selectionGroups == null || groups.containsAll(selectionGroups);
	}

	public UserRep addUser(Certificate certificate, UserRep userRepParam, char[] password) {
		try {

			// validate user actually has this type of privilege
			PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
			prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_ADD_USER);

			// make sure userId is not set
			if (isNotEmpty(userRepParam.getUserId()))
				throw new PrivilegeModelException("UserId can not be set when adding a new user!");

			UserRep userRep = userRepParam.getCopy();

			// set userId
			userRep.setUserId(getUniqueId());

			// first validate user
			userRep.validate();

			validateGroupsExist(userRep);
			validateRolesExist(userRep);

			// validate user does not already exist
			if (this.persistenceHandler.getUser(userRep.getUsername()) != null) {
				String msg = "User {0} can not be added as it already exists!";
				throw new PrivilegeModelException(MessageFormat.format(msg, userRep.getUsername()));
			}

			UserHistory history = UserHistory.EMPTY;
			PasswordCrypt passwordCrypt = null;
			if (password != null) {

				// validate password meets basic requirements
				this.privilegeHandler.validatePassword(certificate.getLocale(), password);

				// get new salt for user
				byte[] salt = this.privilegeHandler.getEncryptionHandler().nextSalt();

				// hash password
				passwordCrypt = this.privilegeHandler.getEncryptionHandler().hashPassword(password, salt);

				history = history.withLastPasswordChange(ZonedDateTime.now());
			}

			// create new user
			User newUser = createUser(userRep, history, passwordCrypt, false);

			// detect privilege conflicts
			assertNoPrivilegeConflict(newUser);

			// validate this user may create such a user
			prvCtx.validateAction(
					new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_ADD_USER, new Tuple(null, newUser)));

			// delegate to persistence handler
			this.persistenceHandler.addUser(newUser);
			this.privilegeHandler.persistModelAsync();

			DefaultPrivilegeHandler.logger.info("Created new user " + newUser.getUsername());

			return newUser.asUserRep();

		} finally {
			clearPassword(password);
		}
	}

	public void addOrUpdateUsers(Certificate certificate, List<UserRep> userReps) throws PrivilegeException {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_ADD_USER);

		List<User> toCreate = new ArrayList<>();
		List<User> toUpdate = new ArrayList<>();

		for (UserRep e : userReps) {
			UserRep userRep = e.getCopy();

			User user;
			User existingUser = this.persistenceHandler.getUser(userRep.getUsername());

			if (existingUser == null) {

				// add user

				// make sure userId is not set
				if (isNotEmpty(userRep.getUserId()))
					throw new PrivilegeModelException("UserId can not be set when adding a new user!");

				// set userId
				userRep.setUserId(getUniqueId());

				// first validate user
				userRep.validate();

				validateGroupsExist(userRep);
				validateRolesExist(userRep);

				// create new user
				user = createUser(userRep, UserHistory.EMPTY, null, false);

				// detect privilege conflicts
				assertNoPrivilegeConflict(user);

				// validate this user may create such a user
				prvCtx.validateAction(
						new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_ADD_USER, new Tuple(null, user)));

				toCreate.add(user);
				DefaultPrivilegeHandler.logger.info("Creating new user " + user.getUsername());

			} else {

				// update user

				if (userRep.getUserId() == null)
					userRep.setUserId(existingUser.getUserId());

				user = createUser(userRep, existingUser.getHistory(), existingUser.getPasswordCrypt(),
						existingUser.isPasswordChangeRequested());

				// detect privilege conflicts
				assertNoPrivilegeConflict(user);

				// validate this user may modify this user
				prvCtx.validateAction(new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_MODIFY_USER,
						new Tuple(existingUser, user)));

				toUpdate.add(user);
				DefaultPrivilegeHandler.logger.info("Updating existing user " + user.getUsername());
			}
		}

		// delegate to persistence handler
		toCreate.forEach(this.persistenceHandler::addUser);
		toUpdate.forEach(this.persistenceHandler::replaceUser);
		this.privilegeHandler.persistModelAsync();

		DefaultPrivilegeHandler.logger.info("Created " + toCreate.size() + " users");
		DefaultPrivilegeHandler.logger.info("Updated " + toUpdate.size() + " users");
	}

	private void assertNoPrivilegeConflict(User user) {
		if (!this.privilegeConflictResolution.isStrict())
			return;
		Map<String, String> privilegeNames = new HashMap<>();
		List<String> conflicts = detectPrivilegeConflicts(privilegeNames, user);
		if (conflicts.isEmpty())
			return;
		String msg = String.join("\n", conflicts);
		throw new PrivilegeModelException(msg);
	}

	private void assertNoPrivilegeConflict(Role role) {
		if (!this.privilegeConflictResolution.isStrict())
			return;

		Map<String, String> privilegeNames = new HashMap<>();
		for (String privilegeName : role.getPrivilegeNames()) {
			privilegeNames.put(privilegeName, role.getName());
		}

		List<String> conflicts = new ArrayList<>();
		List<User> users = this.persistenceHandler.getAllUsers();
		for (User user : users) {
			if (user.hasRole(role.getName()))
				conflicts.addAll(detectPrivilegeConflicts(privilegeNames, user));
		}

		if (!conflicts.isEmpty()) {
			String msg = String.join("\n", conflicts);
			throw new PrivilegeModelException(msg);
		}
	}

	public List<String> detectPrivilegeConflicts(Map<String, String> privilegeNames, User user) {
		List<String> conflicts = new ArrayList<>();

		Set<String> userRoles = user.getRoles();
		for (String roleName : userRoles) {
			Role role = this.persistenceHandler.getRole(roleName);
			if (role == null)
				throw new IllegalStateException("Role " + roleName + " does not exist for user " + user.getUsername());
			for (String privilegeName : role.getPrivilegeNames()) {
				String roleOrigin = privilegeNames.get(privilegeName);
				if (roleOrigin == null) {
					privilegeNames.put(privilegeName, roleName);
				} else if (!roleOrigin.equals(roleName)) {
					String msg = "User {0} has conflicts for privilege {1} on roles {2} and {3}";
					msg = format(msg, user.getUsername(), privilegeName, roleOrigin, roleName);
					conflicts.add(msg);
				}
			}
		}

		return conflicts;
	}

	private void validateGroupsExist(UserRep userRep) {
		for (String group : userRep.getGroups()) {
			if (this.persistenceHandler.getGroup(group) == null) {
				String msg = "Can not add/update user {0} as group {1} does not exist!";
				msg = MessageFormat.format(msg, userRep.getUsername(), group);
				throw new PrivilegeModelException(msg);
			}
		}
	}

	private void validateRolesExist(UserRep userRep) {
		for (String role : userRep.getRoles()) {
			if (this.persistenceHandler.getRole(role) == null) {
				String msg = "Can not add/update user {0} as role {1} does not exist!";
				msg = MessageFormat.format(msg, userRep.getUsername(), role);
				throw new PrivilegeModelException(msg);
			}
		}
	}

	User createUser(UserRep userRep, UserHistory history, PasswordCrypt passwordCrypt,
			boolean passwordChangeRequested) {
		String userId = userRep.getUserId();
		String userName = userRep.getUsername();
		String firstName = userRep.getFirstname();
		String lastName = userRep.getLastname();
		UserState state = userRep.getUserState();
		Set<String> groups = userRep.getGroups();
		Set<String> roles = userRep.getRoles();
		Locale locale = userRep.getLocale();
		Map<String, String> properties = userRep.getProperties();
		return new User(userId, userName, passwordCrypt, firstName, lastName, state, groups, roles, locale, properties,
				passwordChangeRequested, history);
	}

	public UserRep updateUser(Certificate certificate, UserRep userRep, char[] password) throws PrivilegeException {
		try {

			// validate user actually has this type of privilege
			PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
			prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_MODIFY_USER);

			// first validate user
			userRep.validate();

			validateGroupsExist(userRep);
			validateRolesExist(userRep);

			// validate user exists
			User existingUser = this.persistenceHandler.getUser(userRep.getUsername());
			if (existingUser == null) {
				String msg = "User {0} does not exist!";
				throw new PrivilegeModelException(MessageFormat.format(msg, userRep.getUsername()));
			}

			// validate same userId
			if (!existingUser.getUserId().equals(userRep.getUserId())) {
				String msg = "UserId of existing user {0} does not match userRep {1}";
				msg = MessageFormat.format(msg, existingUser.getUserId(), userRep.getUserId());
				throw new PrivilegeModelException(MessageFormat.format(msg, userRep.getUsername()));
			}

			UserHistory history = existingUser.getHistory();
			PasswordCrypt passwordCrypt;
			if (password == null) {
				passwordCrypt = existingUser.getPasswordCrypt();
			} else {

				// validate password meets basic requirements
				this.privilegeHandler.validatePassword(certificate.getLocale(), password);

				// get new salt for user
				byte[] salt = this.privilegeHandler.getEncryptionHandler().nextSalt();

				// hash password
				passwordCrypt = this.privilegeHandler.getEncryptionHandler().hashPassword(password, salt);

				history = history.withLastPasswordChange(ZonedDateTime.now());
			}

			User newUser = createUser(userRep, history, passwordCrypt, existingUser.isPasswordChangeRequested());

			// detect privilege conflicts
			assertNoPrivilegeConflict(newUser);

			// validate this user may modify this user
			prvCtx.validateAction(new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_MODIFY_USER,
					new Tuple(existingUser, newUser)));

			// delegate to persistence handler
			this.persistenceHandler.replaceUser(newUser);
			this.privilegeHandler.persistModelAsync();

			DefaultPrivilegeHandler.logger.info("Replaced user " + newUser.getUsername());

			return newUser.asUserRep();

		} finally {
			clearPassword(password);
		}
	}

	public UserRep removeUser(Certificate certificate, String username) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_REMOVE_USER);

		// validate user exists
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null) {
			String msg = "Can not remove User {0} because user does not exist!";
			throw new PrivilegeModelException(MessageFormat.format(msg, username));
		}

		// validate this user may remove this user
		prvCtx.validateAction(
				new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_REMOVE_USER, new Tuple(null, existingUser)));

		// delegate user removal to persistence handler
		this.privilegeHandler.invalidSessionsFor(existingUser);
		this.persistenceHandler.removeUser(username);
		this.privilegeHandler.persistModelAsync();

		DefaultPrivilegeHandler.logger.info("Removed user " + username);

		return existingUser.asUserRep();
	}

	public UserRep setUserLocale(Certificate certificate, String username, Locale locale) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_SET_USER_LOCALE);

		// get User
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null)
			throw new PrivilegeModelException(MessageFormat.format("User {0} does not exist!", username));

		// create new user
		User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), existingUser.getPasswordCrypt(),
				existingUser.getFirstname(), existingUser.getLastname(), existingUser.getUserState(),
				existingUser.getGroups(), existingUser.getRoles(), locale, existingUser.getProperties(),
				existingUser.isPasswordChangeRequested(), existingUser.getHistory());

		// if the user is not setting their own locale, then make sure this user may set this user's locale
		if (!certificate.getUsername().equals(username)) {
			prvCtx.validateAction(new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_SET_USER_LOCALE,
					new Tuple(existingUser, newUser)));
		}

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceUser(newUser);
		this.privilegeHandler.persistModelAsync();

		DefaultPrivilegeHandler.logger.info("Set locale to " + locale + " for " + newUser.getUsername());

		return newUser.asUserRep();
	}

	public void requirePasswordChange(Certificate certificate, String username) throws PrivilegeException {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_REQUIRE_PASSWORD_CHANGE);

		// get User
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null)
			throw new PrivilegeModelException(MessageFormat.format("User {0} does not exist!", username));

		if (existingUser.getUserState().isRemote())
			throw new PrivilegeModelException(
					MessageFormat.format("User {0} is remote and can not set password!", username));

		// create new user
		User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), existingUser.getPasswordCrypt(),
				existingUser.getFirstname(), existingUser.getLastname(), existingUser.getUserState(),
				existingUser.getGroups(), existingUser.getRoles(), existingUser.getLocale(),
				existingUser.getProperties(), true, existingUser.getHistory());

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceUser(newUser);
		this.privilegeHandler.persistModelAsync();

		DefaultPrivilegeHandler.logger.info(
				"Requiring user " + newUser.getUsername() + " to change their password on next login.");
	}

	public void setUserPassword(Certificate certificate, String username, char[] password) {

		// we don't want the user to worry about whitespace
		username = trimOrEmpty(username);

		try {

			// validate user actually has this type of privilege
			PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
			prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_SET_USER_PASSWORD);

			// get User
			User existingUser = this.persistenceHandler.getUser(username);
			if (existingUser == null)
				throw new PrivilegeModelException(MessageFormat.format("User {0} does not exist!", username));

			UserHistory history = existingUser.getHistory();

			PasswordCrypt passwordCrypt = null;
			if (password != null) {

				// validate password meets basic requirements
				this.privilegeHandler.validatePassword(certificate.getLocale(), password);

				// get new salt for user
				byte[] salt = this.privilegeHandler.getEncryptionHandler().nextSalt();

				// hash password
				passwordCrypt = this.privilegeHandler.getEncryptionHandler().hashPassword(password, salt);

				history = history.withLastPasswordChange(ZonedDateTime.now());
			}

			// create new user
			User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), passwordCrypt,
					existingUser.getFirstname(), existingUser.getLastname(), existingUser.getUserState(),
					existingUser.getGroups(), existingUser.getRoles(), existingUser.getLocale(),
					existingUser.getProperties(), false, history);

			if (!certificate.getUsername().equals(username)) {
				// check that the user may change their own password
				Tuple value = new Tuple(existingUser, newUser);
				prvCtx.validateAction(
						new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_SET_USER_PASSWORD, value));
			}

			// delegate user replacement to persistence handler
			this.persistenceHandler.replaceUser(newUser);
			this.privilegeHandler.persistModelAsync();

			if (certificate.getUsage() == Usage.SET_PASSWORD)
				this.privilegeHandler.invalidate(certificate);

			if (password == null)
				DefaultPrivilegeHandler.logger.info("Cleared password for " + newUser.getUsername());
			else
				DefaultPrivilegeHandler.logger.info("Updated password for " + newUser.getUsername());

		} finally {
			clearPassword(password);
		}
	}

	public UserRep setUserState(Certificate certificate, String username, UserState state) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_SET_USER_STATE);

		// get User
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null)
			throw new PrivilegeModelException(MessageFormat.format("User {0} does not exist!", username));

		// create new user
		User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), existingUser.getPasswordCrypt(),
				existingUser.getFirstname(), existingUser.getLastname(), state, existingUser.getGroups(),
				existingUser.getRoles(), existingUser.getLocale(), existingUser.getProperties(),
				existingUser.isPasswordChangeRequested(), existingUser.getHistory());

		// validate that this user may modify this user's state
		prvCtx.validateAction(new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_SET_USER_STATE,
				new Tuple(existingUser, newUser)));

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceUser(newUser);
		this.privilegeHandler.persistModelAsync();

		DefaultPrivilegeHandler.logger.info("Set state of user " + newUser.getUsername() + " to " + state);

		return newUser.asUserRep();
	}

	public RoleRep addRole(Certificate certificate, RoleRep roleRep) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_ADD_ROLE);

		// first validate role
		roleRep.validate();

		// validate role does not exist
		if (this.persistenceHandler.getRole(roleRep.getName()) != null) {
			String msg = MessageFormat.format("Can not add role {0} as it already exists!", roleRep.getName());
			throw new PrivilegeModelException(msg);
		}

		// create new role from RoleRep
		Role newRole = Role.of(roleRep);

		// validate that this user may add this new role
		prvCtx.validateAction(
				new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_ADD_ROLE, new Tuple(null, newRole)));

		// validate policy if not null
		validatePolicies(newRole);

		// delegate to persistence handler
		this.persistenceHandler.addRole(newRole);
		this.privilegeHandler.persistModelAsync();

		DefaultPrivilegeHandler.logger.info("Added new role " + newRole.getName());

		return newRole.asRoleRep();
	}

	public RoleRep replaceRole(Certificate certificate, RoleRep roleRep) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_MODIFY_ROLE);

		// first validate role
		roleRep.validate();

		// validate role does exist
		Role existingRole = this.persistenceHandler.getRole(roleRep.getName());
		if (existingRole == null) {
			String msg = MessageFormat.format("Can not replace role {0} as it does not exist!", roleRep.getName());
			throw new PrivilegeModelException(msg);
		}

		// create new role from RoleRep
		Role newRole = Role.of(roleRep);

		// detect privilege conflicts
		assertNoPrivilegeConflict(newRole);

		// validate that this user may modify this role
		prvCtx.validateAction(new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_MODIFY_ROLE,
				new Tuple(existingRole, newRole)));

		// validate policy if not null
		validatePolicies(newRole);

		// delegate to persistence handler
		this.persistenceHandler.replaceRole(newRole);
		this.privilegeHandler.persistModelAsync();

		DefaultPrivilegeHandler.logger.info("Replaced role " + newRole.getName());

		// update any existing certificates with new role
		this.privilegeHandler.updateExistingSessionsWithNewRole(newRole);

		return newRole.asRoleRep();
	}

	public RoleRep removeRole(Certificate certificate, String roleName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = this.privilegeHandler.validate(certificate);
		prvCtx.assertHasPrivilege(DefaultPrivilegeHandler.PRIVILEGE_REMOVE_ROLE);

		// validate no user is using this role
		Set<String> roles = new HashSet<>(Collections.singletonList(roleName));
		UserRep selector = new UserRep(null, null, null, null, null, null, roles, null, null, null);
		List<UserRep> usersWithRole = this.privilegeHandler.queryUsers(certificate, selector);
		if (!usersWithRole.isEmpty()) {
			String usersS = usersWithRole.stream().map(UserRep::getUsername).collect(Collectors.joining(", "));
			String msg = "The role {0} can not be removed as the following {1} user have the role assigned: {2}";
			msg = MessageFormat.format(msg, roleName, usersWithRole.size(), usersS);
			throw new PrivilegeModelException(msg);
		}

		// validate role exists
		Role existingRole = this.persistenceHandler.getRole(roleName);
		if (existingRole == null) {
			String msg = "Can not remove Role {0} because role does not exist!";
			throw new PrivilegeModelException(MessageFormat.format(msg, roleName));
		}

		// validate that this user may remove this role
		prvCtx.validateAction(
				new SimpleRestrictable(DefaultPrivilegeHandler.PRIVILEGE_REMOVE_ROLE, new Tuple(null, existingRole)));

		// delegate role removal to persistence handler
		this.persistenceHandler.removeRole(roleName);
		this.privilegeHandler.persistModelAsync();

		DefaultPrivilegeHandler.logger.info("Removed role " + roleName);

		return existingRole.asRoleRep();
	}

	/**
	 * Validates that the policies which are not null on the privileges of the role exist
	 *
	 * @param role the role for which the policies are to be checked
	 */
	public void validatePolicies(Role role) {
		for (String privilegeName : role.getPrivilegeNames()) {
			Privilege privilege = role.getPrivilege(privilegeName);
			String policy = privilege.getPolicy();
			if (policy != null && !this.policyMap.containsKey(policy)) {
				String msg = "Policy {0} for Privilege {1} does not exist on role {2}";
				msg = format(msg, policy, privilege.getName(), role);
				throw new PrivilegeModelException(msg);
			}
		}
	}

	/**
	 * Passwords should not be kept as strings, as string are immutable, this method thus clears the char array so that
	 * the password is not in memory anymore
	 *
	 * @param password the char array containing the passwort which is to be set to zeroes
	 */
	static void clearPassword(char[] password) {
		if (password != null)
			Arrays.fill(password, (char) 0);
	}
}