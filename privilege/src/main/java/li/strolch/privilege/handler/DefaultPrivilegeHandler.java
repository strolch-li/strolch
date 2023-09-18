/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.privilege.base.*;
import li.strolch.privilege.model.*;
import li.strolch.privilege.model.internal.*;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.privilege.xml.CertificateStubsDomWriter;
import li.strolch.privilege.xml.CertificateStubsSaxReader;
import li.strolch.privilege.xml.CertificateStubsSaxReader.CertificateStub;
import li.strolch.utils.collections.Tuple;
import li.strolch.utils.concurrent.ElementLockingHandler;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.AesCryptoHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXParseException;

import javax.crypto.SecretKey;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.time.ZonedDateTime;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.text.MessageFormat.format;
import static li.strolch.utils.helper.ExceptionHelper.getRootCause;
import static li.strolch.utils.helper.StringHelper.*;

/**
 * <p>
 * This is default implementation of the {@link PrivilegeHandler}
 * </p>
 * <p>
 * The following list describes implementation details:
 * <ul>
 * <li>any methods which change the model are first validated by checking if the certificate has the appropriate
 * privilege</li>
 * <li>all model requests are delegated to the configured {@link PrivilegeHandler}, except for the session id to
 * {@link Certificate} map, no model data is kept in this implementation. This also means that to return the
 * representation objects, for every new model query, a new representation object is created</li>
 * <li>when creating new users, or editing users then a null password is understood as no password set</li>
 * <li>Password requirements are simple: Non null and non empty/length 0</li>
 * </ul>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultPrivilegeHandler implements PrivilegeHandler {

	protected static final Logger logger = LoggerFactory.getLogger(DefaultPrivilegeHandler.class);
	public static final String SOURCE_UNKNOWN = "unknown";

	/**
	 * Map keeping a reference to all active sessions
	 */
	protected Map<String, PrivilegeContext> privilegeContextMap;

	/**
	 * Map of {@link PrivilegePolicy} classes
	 */
	protected Map<String, Class<PrivilegePolicy>> policyMap;

	/**
	 * The persistence handler is used for getting objects and saving changes
	 */
	protected PersistenceHandler persistenceHandler;

	/**
	 * The encryption handler is used for generating hashes and tokens
	 */
	protected EncryptionHandler encryptionHandler;

	/**
	 * The password strength handler is used for validating the strength of a password when being set
	 */
	protected PasswordStrengthHandler passwordStrengthHandler;

	/**
	 * The Single Sign On Handler
	 */
	protected SingleSignOnHandler ssoHandler;

	/**
	 * The {@link UserChallengeHandler} is used to challenge a user which tries to authenticate and/or change their
	 * password
	 */
	protected UserChallengeHandler userChallengeHandler;

	/**
	 * flag to define if already initialized
	 */
	protected boolean initialized;

	/**
	 * flag to define if a persist should be performed after a user changes their own data
	 */
	protected boolean autoPersistOnUserChangesData;

	/**
	 * flag to define if sessions should be persisted
	 */
	protected boolean persistSessions;

	/**
	 * Path to sessions file for persistence
	 */
	protected File persistSessionsPath;

	/**
	 * Secret key
	 */
	protected SecretKey secretKey;

	/**
	 * flag if session refreshing is allowed
	 */
	protected boolean allowSessionRefresh;
	protected boolean disallowSourceChange;

	protected PrivilegeConflictResolution privilegeConflictResolution;
	private String identifier;

	private Map<String, String> parameterMap;

	private ElementLockingHandler<String> lockingHandler;
	private ScheduledExecutorService executorService;
	private Future<?> persistSessionsTask;
	private Future<?> persistModelTask;

	@Override
	public SingleSignOnHandler getSsoHandler() {
		return this.ssoHandler;
	}

	@Override
	public UserChallengeHandler getUserChallengeHandler() {
		return this.userChallengeHandler;
	}

	@Override
	public PersistenceHandler getPersistenceHandler() {
		return this.persistenceHandler;
	}

	@Override
	public Map<String, String> getParameterMap() {
		return this.parameterMap;
	}

	@Override
	public boolean isRefreshAllowed() {
		return this.allowSessionRefresh;
	}

	@Override
	public boolean isPersistOnUserDataChanged() {
		return this.autoPersistOnUserChangesData;
	}

	@Override
	public EncryptionHandler getEncryptionHandler() throws PrivilegeException {
		return this.encryptionHandler;
	}

	@Override
	public RoleRep getRole(Certificate certificate, String roleName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_GET_ROLE);

		Role role = this.persistenceHandler.getRole(roleName);
		if (role == null)
			return null;

		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_GET_ROLE, new Tuple(null, role)));

		return role.asRoleRep();
	}

	@Override
	public UserRep getUser(Certificate certificate, String username) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_GET_USER);

		User user = this.persistenceHandler.getUser(username);
		if (user == null)
			return null;

		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_GET_USER, new Tuple(null, user)));
		return user.asUserRep();
	}

	@Override
	public Map<String, String> getPolicyDefs(Certificate certificate) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_GET_POLICIES));

		Map<String, String> policyDef = new HashMap<>(this.policyMap.size());
		for (Entry<String, Class<PrivilegePolicy>> entry : this.policyMap.entrySet()) {
			policyDef.put(entry.getKey(), entry.getValue().getName());
		}
		return policyDef;
	}

	@Override
	public List<Certificate> getCertificates(Certificate certificate) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_GET_CERTIFICATES));

		return this.privilegeContextMap.values().stream().map(PrivilegeContext::getCertificate)
				.collect(Collectors.toList());
	}

	@Override
	public List<RoleRep> getRoles(Certificate certificate) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_GET_ROLE);

		Stream<Role> rolesStream = this.persistenceHandler.getAllRoles().stream();

		// validate access to each role
		rolesStream = rolesStream.filter(
				role -> prvCtx.hasPrivilege(new SimpleRestrictable(PRIVILEGE_GET_ROLE, new Tuple(null, role))));

		return rolesStream.map(Role::asRoleRep).collect(Collectors.toList());
	}

	@Override
	public List<UserRep> getUsers(Certificate certificate) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_GET_USER);

		Stream<User> usersStream = this.persistenceHandler.getAllUsers().stream();

		// validate access to each user
		usersStream = usersStream.filter(
				user -> prvCtx.hasPrivilege(new SimpleRestrictable(PRIVILEGE_GET_USER, new Tuple(null, user))));

		return usersStream.map(User::asUserRep).collect(Collectors.toList());
	}

	@Override
	public List<UserRep> queryUsers(Certificate certificate, UserRep selectorRep) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_GET_USER);

		String selUserId = selectorRep.getUserId();
		String selUsername = selectorRep.getUsername();
		String selFirstName = selectorRep.getFirstname();
		String selLastName = selectorRep.getLastname();
		UserState selUserState = selectorRep.getUserState();
		Locale selLocale = selectorRep.getLocale();
		Set<String> selRoles = selectorRep.getRoles();
		Map<String, String> selPropertyMap = selectorRep.getProperties();

		List<UserRep> result = new ArrayList<>();
		List<User> allUsers = this.persistenceHandler.getAllUsers();
		for (User user : allUsers) {

			if (!prvCtx.hasPrivilege(new SimpleRestrictable(PRIVILEGE_GET_USER, new Tuple(null, user))))
				continue;

			// selections
			boolean userIdSelected;
			boolean usernameSelected;
			boolean firstNameSelected;
			boolean lastNameSelected;
			boolean userStateSelected;
			boolean localeSelected;
			boolean roleSelected;
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

			// roles
			roleSelected = isSelectedByRole(selRoles, user.getRoles());

			// properties
			propertySelected = isSelectedByProperty(selPropertyMap, user.getProperties());

			boolean selected = userIdSelected && usernameSelected && firstNameSelected && lastNameSelected &&
					userStateSelected && localeSelected && roleSelected && propertySelected;

			if (selected)
				result.add(user.asUserRep());
		}

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
	private boolean isSelectedByProperty(Map<String, String> selectionMap, Map<String, String> properties) {

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
	private boolean isSelectedByRole(Set<String> selectionRoles, Set<String> roles) {
		return selectionRoles == null || roles.containsAll(selectionRoles);
	}

	@Override
	public UserRep addUser(Certificate certificate, UserRep userRepParam, char[] password) {
		return this.lockingHandler.lockedExecuteWithResult(userRepParam.getUsername(),
				() -> internalAddUser(certificate, userRepParam, password));
	}

	public UserRep internalAddUser(Certificate certificate, UserRep userRepParam, char[] password) {
		try {

			// validate user actually has this type of privilege
			PrivilegeContext prvCtx = validate(certificate);
			prvCtx.assertHasPrivilege(PRIVILEGE_ADD_USER);

			// make sure userId is not set
			if (isNotEmpty(userRepParam.getUserId()))
				throw new PrivilegeModelException("UserId can not be set when adding a new user!");

			UserRep userRep = userRepParam.getCopy();

			// set userId
			userRep.setUserId(getUniqueId());

			// first validate user
			userRep.validate();

			validateRolesExist(userRep);

			// validate user does not already exist
			if (this.persistenceHandler.getUser(userRep.getUsername()) != null) {
				String msg = "User {0} can not be added as it already exists!";
				throw new PrivilegeModelException(format(msg, userRep.getUsername()));
			}

			UserHistory history = new UserHistory();
			PasswordCrypt passwordCrypt = null;
			if (password != null) {

				// validate password meets basic requirements
				validatePassword(certificate.getLocale(), password);

				// get new salt for user
				byte[] salt = this.encryptionHandler.nextSalt();

				// hash password
				passwordCrypt = this.encryptionHandler.hashPassword(password, salt);

				history.setLastPasswordChange(ZonedDateTime.now());
			}

			// create new user
			User newUser = createUser(userRep, history, passwordCrypt, false);

			// detect privilege conflicts
			assertNoPrivilegeConflict(newUser);

			// validate this user may create such a user
			prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ADD_USER, new Tuple(null, newUser)));

			// delegate to persistence handler
			this.persistenceHandler.addUser(newUser);
			persistModelAsync();

			logger.info("Created new user " + newUser.getUsername());

			return newUser.asUserRep();

		} finally {
			clearPassword(password);
		}
	}

	@Override
	public void addOrUpdateUsers(Certificate certificate, List<UserRep> userReps) throws PrivilegeException {
		this.lockingHandler.lockedExecute(PrivilegeHandler.class.getSimpleName(),
				() -> internalAddOrUpdateUsers(certificate, userReps));
	}

	private void internalAddOrUpdateUsers(Certificate certificate, List<UserRep> userReps) throws PrivilegeException {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_ADD_USER);

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

				validateRolesExist(userRep);

				// create new user
				user = createUser(userRep, new UserHistory(), null, false);

				// detect privilege conflicts
				assertNoPrivilegeConflict(user);

				// validate this user may create such a user
				prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ADD_USER, new Tuple(null, user)));

				toCreate.add(user);
				logger.info("Creating new user " + user.getUsername());

			} else {

				// update user

				if (userRep.getUserId() == null)
					userRep.setUserId(existingUser.getUserId());

				UserHistory history = existingUser.getHistory().getClone();
				user = createUser(userRep, history, existingUser.getPasswordCrypt(),
						existingUser.isPasswordChangeRequested());

				// detect privilege conflicts
				assertNoPrivilegeConflict(user);

				// validate this user may modify this user
				prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_MODIFY_USER, new Tuple(existingUser, user)));

				toUpdate.add(user);
				logger.info("Updating existing user " + user.getUsername());
			}
		}

		// delegate to persistence handler
		toCreate.forEach(user -> this.persistenceHandler.addUser(user));
		toUpdate.forEach(user -> this.persistenceHandler.replaceUser(user));
		persistModelAsync();

		logger.info("Created " + toCreate.size() + " users");
		logger.info("Updated " + toUpdate.size() + " users");
	}

	@Override
	public UserRep replaceUser(Certificate certificate, UserRep userRep, char[] password) throws PrivilegeException {
		return this.lockingHandler.lockedExecuteWithResult(userRep.getUsername(),
				() -> internalReplaceUser(certificate, userRep, password));
	}

	private UserRep internalReplaceUser(Certificate certificate, UserRep userRep, char[] password) {
		try {

			// validate user actually has this type of privilege
			PrivilegeContext prvCtx = validate(certificate);
			prvCtx.assertHasPrivilege(PRIVILEGE_MODIFY_USER);

			// first validate user
			userRep.validate();

			validateRolesExist(userRep);

			// validate user exists
			User existingUser = this.persistenceHandler.getUser(userRep.getUsername());
			if (existingUser == null) {
				String msg = "User {0} can not be replaced as it does not exist!";
				throw new PrivilegeModelException(format(msg, userRep.getUsername()));
			}

			// validate same userId
			if (!existingUser.getUserId().equals(userRep.getUserId())) {
				String msg = "UserId of existing user {0} does not match userRep {1}";
				msg = format(msg, existingUser.getUserId(), userRep.getUserId());
				throw new PrivilegeModelException(format(msg, userRep.getUsername()));
			}

			UserHistory history = existingUser.getHistory().getClone();
			PasswordCrypt passwordCrypt = null;
			if (password != null) {

				// validate password meets basic requirements
				validatePassword(certificate.getLocale(), password);

				// get new salt for user
				byte[] salt = this.encryptionHandler.nextSalt();

				// hash password
				passwordCrypt = this.encryptionHandler.hashPassword(password, salt);

				history.setLastPasswordChange(ZonedDateTime.now());
			}

			User newUser = createUser(userRep, history, passwordCrypt, existingUser.isPasswordChangeRequested());

			// detect privilege conflicts
			assertNoPrivilegeConflict(newUser);

			// validate this user may modify this user
			prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_MODIFY_USER, new Tuple(existingUser, newUser)));

			// delegate to persistence handler
			this.persistenceHandler.replaceUser(newUser);
			persistModelAsync();

			logger.info("Replaced user " + newUser.getUsername());

			return newUser.asUserRep();

		} finally {
			clearPassword(password);
		}
	}

	private void validateRolesExist(UserRep userRep) {
		// validate all roles exist
		for (String role : userRep.getRoles()) {
			if (this.persistenceHandler.getRole(role) == null) {
				String msg = "Can not add user {0} as role {1} does not exist!";
				msg = format(msg, userRep.getUsername(), role);
				throw new PrivilegeModelException(msg);
			}
		}
	}

	private User createUser(UserRep userRep, UserHistory history, PasswordCrypt passwordCrypt,
			boolean passwordChangeRequested) {
		String userId = userRep.getUserId();
		String userName = userRep.getUsername();
		String firstName = userRep.getFirstname();
		String lastName = userRep.getLastname();
		UserState state = userRep.getUserState();
		Set<String> roles = userRep.getRoles();
		Locale locale = userRep.getLocale();
		Map<String, String> properties = userRep.getProperties();
		return new User(userId, userName, passwordCrypt, firstName, lastName, state, roles, locale, properties,
				passwordChangeRequested, history);
	}

	@Override
	public UserRep updateUser(Certificate certificate, UserRep userRep) throws PrivilegeException {
		return this.lockingHandler.lockedExecuteWithResult(userRep.getUsername(),
				() -> internalUpdateUser(certificate, userRep));
	}

	public UserRep internalUpdateUser(Certificate certificate, UserRep userRep) throws PrivilegeException {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_MODIFY_USER);

		// get existing user
		User existingUser = this.persistenceHandler.getUser(userRep.getUsername());
		if (existingUser == null)
			throw new PrivilegeModelException(format("User {0} does not exist!", userRep.getUsername()));

		// if nothing to do, then stop
		if (isEmpty(userRep.getFirstname()) && isEmpty(userRep.getLastname()) && userRep.getLocale() == null &&
				(userRep.getProperties() == null || userRep.getProperties().isEmpty())) {
			throw new PrivilegeModelException(
					format("All updateable fields are empty for update of user {0}", userRep.getUsername()));
		}

		String userId = existingUser.getUserId();
		String username = existingUser.getUsername();
		PasswordCrypt passwordCrypt = existingUser.getPasswordCrypt();
		String firstName = existingUser.getFirstname();
		String lastName = existingUser.getLastname();
		UserState userState = existingUser.getUserState();
		Set<String> roles = existingUser.getRoles();
		Locale locale = existingUser.getLocale();
		Map<String, String> propertyMap = existingUser.getProperties();

		// get updated fields
		if (isNotEmpty(userRep.getFirstname()))
			firstName = userRep.getFirstname();
		if (isNotEmpty(userRep.getLastname()))
			lastName = userRep.getLastname();
		if (userRep.getProperties() != null && !userRep.getProperties().isEmpty())
			propertyMap = userRep.getProperties();

		if (userRep.getLocale() != null)
			locale = userRep.getLocale();
		if (userRep.getUserState() != null)
			userState = userRep.getUserState();
		if (userRep.getRoles() != null && !userRep.getRoles().equals(roles))
			roles = userRep.getRoles();

		// create new user
		User newUser = new User(userId, username, passwordCrypt, firstName, lastName, userState, roles, locale,
				propertyMap, existingUser.isPasswordChangeRequested(), existingUser.getHistory().getClone());

		// detect privilege conflicts
		assertNoPrivilegeConflict(newUser);

		// validate this user may modify this user
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_MODIFY_USER, new Tuple(existingUser, newUser)));

		// delegate to persistence handler
		this.persistenceHandler.replaceUser(newUser);
		persistModelAsync();

		logger.info("Updated user " + newUser.getUsername());

		// update any existing sessions for this user
		updateExistingSessionsForUser(newUser);

		return newUser.asUserRep();
	}

	@Override
	public UserRep removeUser(Certificate certificate, String username) {
		return this.lockingHandler.lockedExecuteWithResult(username, () -> internalRemoveUser(certificate, username));
	}

	private UserRep internalRemoveUser(Certificate certificate, String username) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_REMOVE_USER);

		// validate user exists
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null) {
			String msg = "Can not remove User {0} because user does not exist!";
			throw new PrivilegeModelException(format(msg, username));
		}

		// validate this user may remove this user
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_REMOVE_USER, new Tuple(null, existingUser)));

		// delegate user removal to persistence handler
		invalidSessionsFor(existingUser);
		this.persistenceHandler.removeUser(username);
		persistModelAsync();

		logger.info("Removed user " + username);

		return existingUser.asUserRep();
	}

	@Override
	public UserRep addRoleToUser(Certificate certificate, String username, String roleName) {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> internalAddRoleToUser(certificate, username, roleName));
	}

	private UserRep internalAddRoleToUser(Certificate certificate, String username, String roleName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_ADD_ROLE_TO_USER);

		// get user
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null)
			throw new PrivilegeModelException(format("User {0} does not exist!", username));

		// validate that this user may add this role to this user
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ADD_ROLE_TO_USER, new Tuple(existingUser, roleName)));

		// check that user not already has role
		Set<String> currentRoles = existingUser.getRoles();
		if (currentRoles.contains(roleName)) {
			String msg = format("User {0} already has role {1}", username, roleName);
			throw new PrivilegeModelException(msg);
		}

		// validate that the role exists
		if (this.persistenceHandler.getRole(roleName) == null) {
			String msg = format("Role {0} does not exist!", roleName);
			throw new PrivilegeModelException(msg);
		}

		// create new user
		Set<String> newRoles = new HashSet<>(currentRoles);
		newRoles.add(roleName);

		User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), existingUser.getPasswordCrypt(),
				existingUser.getFirstname(), existingUser.getLastname(), existingUser.getUserState(), newRoles,
				existingUser.getLocale(), existingUser.getProperties(), existingUser.isPasswordChangeRequested(),
				existingUser.getHistory().getClone());

		// detect privilege conflicts
		assertNoPrivilegeConflict(newUser);

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceUser(newUser);
		persistModelAsync();

		logger.info("Added role " + roleName + " to " + newUser.getUsername());

		// update any existing sessions for this user
		updateExistingSessionsForUser(newUser);

		return newUser.asUserRep();
	}

	@Override
	public UserRep removeRoleFromUser(Certificate certificate, String username, String roleName) {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> internalRemoveRoleFromUser(certificate, username, roleName));
	}

	private UserRep internalRemoveRoleFromUser(Certificate certificate, String username, String roleName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_REMOVE_ROLE_FROM_USER);

		// get User
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null)
			throw new PrivilegeModelException(format("User {0} does not exist!", username));

		// validate that this user may remove this role from this user
		prvCtx.validateAction(
				new SimpleRestrictable(PRIVILEGE_REMOVE_ROLE_FROM_USER, new Tuple(existingUser, roleName)));

		// ignore if user does not have role
		Set<String> currentRoles = existingUser.getRoles();
		if (!currentRoles.contains(roleName)) {
			String msg = format("User {0} does not have role {1}", existingUser.getUsername(), roleName);
			throw new PrivilegeModelException(msg);
		}

		// create new user
		Set<String> newRoles = new HashSet<>(currentRoles);
		newRoles.remove(roleName);
		User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), existingUser.getPasswordCrypt(),
				existingUser.getFirstname(), existingUser.getLastname(), existingUser.getUserState(), newRoles,
				existingUser.getLocale(), existingUser.getProperties(), existingUser.isPasswordChangeRequested(),
				existingUser.getHistory().getClone());

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceUser(newUser);
		persistModelAsync();

		logger.info("Removed role " + roleName + " from " + newUser.getUsername());

		// update any existing sessions for this user
		updateExistingSessionsForUser(newUser);

		return newUser.asUserRep();
	}

	@Override
	public UserRep setUserLocale(Certificate certificate, String username, Locale locale) {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> internalSetUserLocale(certificate, username, locale));
	}

	private UserRep internalSetUserLocale(Certificate certificate, String username, Locale locale) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_SET_USER_LOCALE);

		// get User
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null)
			throw new PrivilegeModelException(format("User {0} does not exist!", username));

		// create new user
		User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), existingUser.getPasswordCrypt(),
				existingUser.getFirstname(), existingUser.getLastname(), existingUser.getUserState(),
				existingUser.getRoles(), locale, existingUser.getProperties(), existingUser.isPasswordChangeRequested(),
				existingUser.getHistory().getClone());

		// if the user is not setting their own locale, then make sure this user may set this user's locale
		if (!certificate.getUsername().equals(username)) {
			prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_SET_USER_LOCALE, new Tuple(existingUser, newUser)));
		}

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceUser(newUser);
		persistModelAsync();

		logger.info("Set locale to " + locale + " for " + newUser.getUsername());

		return newUser.asUserRep();
	}

	@Override
	public void requirePasswordChange(Certificate certificate, String username) throws PrivilegeException {
		this.lockingHandler.lockedExecute(username, () -> internalRequirePasswordChange(certificate, username));
	}

	private void internalRequirePasswordChange(Certificate certificate, String username) throws PrivilegeException {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_REQUIRE_PASSWORD_CHANGE);

		// get User
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null)
			throw new PrivilegeModelException(format("User {0} does not exist!", username));

		if (existingUser.getUserState().isRemote())
			throw new PrivilegeModelException(format("User {0} is remote and can not set password!", username));

		// create new user
		User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), existingUser.getPasswordCrypt(),
				existingUser.getFirstname(), existingUser.getLastname(), existingUser.getUserState(),
				existingUser.getRoles(), existingUser.getLocale(), existingUser.getProperties(), true,
				existingUser.getHistory().getClone());

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceUser(newUser);
		persistModelAsync();

		logger.info("Requiring user " + newUser.getUsername() + " to change their password on next login.");
	}

	@Override
	public void setUserPassword(Certificate certificate, String username, char[] password) {
		this.lockingHandler.lockedExecute(username, () -> internalSetUserPassword(certificate, username, password));
	}

	private void internalSetUserPassword(Certificate certificate, String username, char[] password) {

		// we don't want the user to worry about whitespace
		username = trimOrEmpty(username);

		try {

			// validate user actually has this type of privilege
			PrivilegeContext prvCtx = validate(certificate);
			prvCtx.assertHasPrivilege(PRIVILEGE_SET_USER_PASSWORD);

			// get User
			User existingUser = this.persistenceHandler.getUser(username);
			if (existingUser == null)
				throw new PrivilegeModelException(format("User {0} does not exist!", username));

			UserHistory history = existingUser.getHistory().getClone();

			PasswordCrypt passwordCrypt = null;
			if (password != null) {

				// validate password meets basic requirements
				validatePassword(certificate.getLocale(), password);

				// get new salt for user
				byte[] salt = this.encryptionHandler.nextSalt();

				// hash password
				passwordCrypt = this.encryptionHandler.hashPassword(password, salt);

				history.setLastPasswordChange(ZonedDateTime.now());
			}

			// create new user
			User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), passwordCrypt,
					existingUser.getFirstname(), existingUser.getLastname(), existingUser.getUserState(),
					existingUser.getRoles(), existingUser.getLocale(), existingUser.getProperties(), false, history);

			if (!certificate.getUsername().equals(username)) {
				// check that the user may change their own password
				Tuple value = new Tuple(existingUser, newUser);
				prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_SET_USER_PASSWORD, value));
			}

			// delegate user replacement to persistence handler
			this.persistenceHandler.replaceUser(newUser);
			persistModelAsync();

			if (certificate.getUsage() == Usage.SET_PASSWORD)
				invalidate(certificate);

			if (password == null)
				logger.info("Cleared password for " + newUser.getUsername());
			else
				logger.info("Updated password for " + newUser.getUsername());

		} finally {
			clearPassword(password);
		}
	}

	@Override
	public UserRep setUserState(Certificate certificate, String username, UserState state) {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> internalSetUserState(certificate, username, state));
	}

	private UserRep internalSetUserState(Certificate certificate, String username, UserState state) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_SET_USER_STATE);

		// get User
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null)
			throw new PrivilegeModelException(format("User {0} does not exist!", username));

		// create new user
		User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), existingUser.getPasswordCrypt(),
				existingUser.getFirstname(), existingUser.getLastname(), state, existingUser.getRoles(),
				existingUser.getLocale(), existingUser.getProperties(), existingUser.isPasswordChangeRequested(),
				existingUser.getHistory().getClone());

		// validate that this user may modify this user's state
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_SET_USER_STATE, new Tuple(existingUser, newUser)));

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceUser(newUser);
		persistModelAsync();

		logger.info("Set state of user " + newUser.getUsername() + " to " + state);

		return newUser.asUserRep();
	}

	@Override
	public RoleRep addRole(Certificate certificate, RoleRep roleRep) {
		return this.lockingHandler.lockedExecuteWithResult(roleRep.getName(),
				() -> internalAddRole(certificate, roleRep));
	}

	private RoleRep internalAddRole(Certificate certificate, RoleRep roleRep) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_ADD_ROLE);

		// first validate role
		roleRep.validate();

		// validate role does not exist
		if (this.persistenceHandler.getRole(roleRep.getName()) != null) {
			String msg = format("Can not add role {0} as it already exists!", roleRep.getName());
			throw new PrivilegeModelException(msg);
		}

		// create new role from RoleRep
		Role newRole = new Role(roleRep);

		// validate that this user may add this new role
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ADD_ROLE, new Tuple(null, newRole)));

		// validate policy if not null
		validatePolicies(newRole);

		// delegate to persistence handler
		this.persistenceHandler.addRole(newRole);
		persistModelAsync();

		logger.info("Added new role " + newRole.getName());

		return newRole.asRoleRep();
	}

	@Override
	public RoleRep replaceRole(Certificate certificate, RoleRep roleRep) {
		return this.lockingHandler.lockedExecuteWithResult(roleRep.getName(),
				() -> internalReplaceRole(certificate, roleRep));
	}

	private RoleRep internalReplaceRole(Certificate certificate, RoleRep roleRep) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_MODIFY_ROLE);

		// first validate role
		roleRep.validate();

		// validate role does exist
		Role existingRole = this.persistenceHandler.getRole(roleRep.getName());
		if (existingRole == null) {
			String msg = format("Can not replace role {0} as it does not exist!", roleRep.getName());
			throw new PrivilegeModelException(msg);
		}

		// create new role from RoleRep
		Role newRole = new Role(roleRep);

		// detect privilege conflicts
		assertNoPrivilegeConflict(newRole);

		// validate that this user may modify this role
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_MODIFY_ROLE, new Tuple(existingRole, newRole)));

		// validate policy if not null
		validatePolicies(newRole);

		// delegate to persistence handler
		this.persistenceHandler.replaceRole(newRole);
		persistModelAsync();

		logger.info("Replaced role " + newRole.getName());

		// update any existing certificates with new role
		updateExistingSessionsWithNewRole(newRole);

		return newRole.asRoleRep();
	}

	@Override
	public RoleRep removeRole(Certificate certificate, String roleName) {
		return this.lockingHandler.lockedExecuteWithResult(roleName, () -> internalRemoveRole(certificate, roleName));
	}

	private RoleRep internalRemoveRole(Certificate certificate, String roleName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_REMOVE_ROLE);

		// validate no user is using this role
		Set<String> roles = new HashSet<>(Collections.singletonList(roleName));
		UserRep selector = new UserRep(null, null, null, null, null, roles, null, null, null);
		List<UserRep> usersWithRole = queryUsers(certificate, selector);
		if (!usersWithRole.isEmpty()) {
			String usersS = usersWithRole.stream().map(UserRep::getUsername).collect(Collectors.joining(", "));
			String msg = "The role {0} can not be removed as the following {1} user have the role assigned: {2}";
			msg = format(msg, roleName, usersWithRole.size(), usersS);
			throw new PrivilegeModelException(msg);
		}

		// validate role exists
		Role existingRole = this.persistenceHandler.getRole(roleName);
		if (existingRole == null) {
			String msg = "Can not remove Role {0} because role does not exist!";
			throw new PrivilegeModelException(format(msg, roleName));
		}

		// validate that this user may remove this role
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_REMOVE_ROLE, new Tuple(null, existingRole)));

		// delegate role removal to persistence handler
		this.persistenceHandler.removeRole(roleName);
		persistModelAsync();

		logger.info("Removed role " + roleName);

		return existingRole.asRoleRep();
	}

	@Override
	public RoleRep addOrReplacePrivilegeOnRole(Certificate certificate, String roleName, PrivilegeRep privilegeRep) {
		return this.lockingHandler.lockedExecuteWithResult(roleName,
				() -> internalAddOrReplacePrivilegeOnRole(certificate, roleName, privilegeRep));
	}

	private RoleRep internalAddOrReplacePrivilegeOnRole(Certificate certificate, String roleName,
			PrivilegeRep privilegeRep) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_MODIFY_ROLE);

		// validate PrivilegeRep
		privilegeRep.validate();

		// get role
		Role existingRole = this.persistenceHandler.getRole(roleName);
		if (existingRole == null) {
			String msg = format("Role {0} does not exist!", roleName);
			throw new PrivilegeModelException(msg);
		}

		// validate that policy exists if needed
		String policy = privilegeRep.getPolicy();
		if (policy != null && !this.policyMap.containsKey(policy)) {
			String msg = "Policy {0} for Privilege {1} does not exist";
			msg = format(msg, policy, privilegeRep.getName());
			throw new PrivilegeModelException(msg);
		}

		// create new role with the additional privilege
		IPrivilege newPrivilege = new PrivilegeImpl(privilegeRep);

		// copy existing privileges
		Set<String> existingPrivilegeNames = existingRole.getPrivilegeNames();
		Map<String, IPrivilege> privilegeMap = new HashMap<>(existingPrivilegeNames.size() + 1);
		for (String name : existingPrivilegeNames) {
			IPrivilege privilege = existingRole.getPrivilege(name);
			privilegeMap.put(name, privilege);
		}

		// add new one
		privilegeMap.put(newPrivilege.getName(), newPrivilege);

		// create new role
		Role newRole = new Role(existingRole.getName(), privilegeMap);

		// detect privilege conflicts
		assertNoPrivilegeConflict(newRole);

		// validate that this user may modify this role
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_MODIFY_ROLE, new Tuple(existingRole, newRole)));

		// delegate role replacement to persistence handler
		this.persistenceHandler.replaceRole(newRole);
		persistModelAsync();

		logger.info("Added/replaced privilege " + privilegeRep.getName() + " to " + roleName);

		// update any existing certificates with new role
		updateExistingSessionsWithNewRole(newRole);

		return newRole.asRoleRep();
	}

	@Override
	public RoleRep removePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName) {
		return this.lockingHandler.lockedExecuteWithResult(roleName,
				() -> internalRemovePrivilegeFromRole(certificate, roleName, privilegeName));
	}

	private RoleRep internalRemovePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_MODIFY_ROLE);

		// get role
		Role existingRole = this.persistenceHandler.getRole(roleName);
		if (existingRole == null) {
			throw new PrivilegeModelException(format("Role {0} does not exist!", roleName));
		}

		// ignore if role does not have privilege
		if (!existingRole.hasPrivilege(privilegeName)) {
			String msg = format("Role {0} does not have Privilege {1}", roleName, privilegeName);
			throw new PrivilegeModelException(msg);
		}

		// create new set of privileges with out the to removed privilege
		Set<String> privilegeNames = existingRole.getPrivilegeNames();
		Map<String, IPrivilege> newPrivileges = new HashMap<>(privilegeNames.size() - 1);
		for (String name : privilegeNames) {
			IPrivilege privilege = existingRole.getPrivilege(name);
			if (!privilege.getName().equals(privilegeName))
				newPrivileges.put(privilege.getName(), privilege);
		}

		// create new role
		Role newRole = new Role(existingRole.getName(), newPrivileges);

		// validate that this user may modify this role
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_MODIFY_ROLE, new Tuple(existingRole, newRole)));

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceRole(newRole);
		persistModelAsync();

		logger.info("Removed privilege " + privilegeName + " from " + roleName);

		// update any existing certificates with new role
		updateExistingSessionsWithNewRole(newRole);

		return newRole.asRoleRep();
	}

	/**
	 * Replaces any existing {@link PrivilegeContext} for the given user by updating with the new user object
	 *
	 * @param newUser the new user to update with
	 */
	private void updateExistingSessionsForUser(User newUser) {
		List<PrivilegeContext> contexts = new ArrayList<>(this.privilegeContextMap.values());
		for (PrivilegeContext ctx : contexts) {
			if (ctx.getUserRep().getUsername().equals(newUser.getUsername())) {
				Certificate cert = ctx.getCertificate();
				cert = buildCertificate(cert.getUsage(), newUser, cert.getAuthToken(), cert.getSessionId(),
						cert.getSource(), cert.getLoginTime(), cert.isKeepAlive());
				PrivilegeContext privilegeContext = buildPrivilegeContext(cert, newUser);
				this.privilegeContextMap.put(cert.getSessionId(), privilegeContext);
			}
		}

		persistSessionsAsync();
	}

	/**
	 * Replaces any existing {@link PrivilegeContext} for users with the given role
	 *
	 * @param role the role to update with
	 */
	private void updateExistingSessionsWithNewRole(Role role) {
		List<PrivilegeContext> contexts = new ArrayList<>(this.privilegeContextMap.values());
		for (PrivilegeContext ctx : contexts) {
			if (ctx.getUserRep().hasRole(role.getName())) {
				User user = this.persistenceHandler.getUser(ctx.getUsername());
				if (user == null)
					continue;

				Certificate cert = ctx.getCertificate();
				cert = buildCertificate(cert.getUsage(), user, cert.getAuthToken(), cert.getSessionId(),
						cert.getSource(), cert.getLoginTime(), cert.isKeepAlive());
				PrivilegeContext privilegeContext = buildPrivilegeContext(cert, user);
				this.privilegeContextMap.put(cert.getSessionId(), privilegeContext);
			}
		}

		persistSessionsAsync();
	}

	private void invalidSessionsFor(User user) {
		List<PrivilegeContext> contexts = new ArrayList<>(this.privilegeContextMap.values());
		for (PrivilegeContext ctx : contexts) {
			if (ctx.getUserRep().getUsername().equals(user.getUsername()))
				invalidate(ctx.getCertificate());
		}
	}

	@Override
	public void initiateChallengeFor(Usage usage, String username) {
		initiateChallengeFor(usage, username, SOURCE_UNKNOWN);
	}

	@Override
	public void initiateChallengeFor(Usage usage, String username, String source) {
		this.lockingHandler.lockedExecute(username, () -> internalInitiateChallengeFor(usage, username, source));
	}

	private void internalInitiateChallengeFor(Usage usage, String username, String source) {
		DBC.PRE.assertNotEmpty("source must not be empty!", source);

		// get User
		User user = this.persistenceHandler.getUser(username);
		if (user == null) {
			throw new PrivilegeModelException(format("User {0} does not exist!", username));
		}

		// initiate the challenge
		this.userChallengeHandler.initiateChallengeFor(usage, user, source);

		logger.info(format("Initiated Challenge for {0} with usage {1}", username, usage));
	}

	@Override
	public Certificate validateChallenge(String username, String challenge) throws PrivilegeException {
		return validateChallenge(username, challenge, "unknown");
	}

	@Override
	public Certificate validateChallenge(String username, String challenge, String source) throws PrivilegeException {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> internalValidateChallenge(username, challenge, source));
	}

	private Certificate internalValidateChallenge(String username, String challenge, String source)
			throws PrivilegeException {
		DBC.PRE.assertNotEmpty("source must not be empty!", source);

		// get User
		User user = this.persistenceHandler.getUser(username);
		if (user == null) {
			throw new PrivilegeModelException(format("User {0} does not exist!", username));
		}

		// validate the response
		UserChallenge userChallenge = this.userChallengeHandler.validateResponse(user, challenge);
		String authToken = this.encryptionHandler.nextToken();
		String sessionId = UUID.randomUUID().toString();

		// create a new certificate, with details of the user
		Usage usage = userChallenge.getUsage();
		Certificate certificate = buildCertificate(usage, user, authToken, sessionId, userChallenge.getSource(),
				ZonedDateTime.now(), false);

		PrivilegeContext privilegeContext = buildPrivilegeContext(certificate, user);
		this.privilegeContextMap.put(sessionId, privilegeContext);

		if (!source.equals("unknown") && !source.equals(userChallenge.getSource())) {
			logger.warn("Challenge request and response source's are different: request: " + userChallenge.getSource() +
					" to " + source);
		}

		persistSessionsAsync();

		logger.info(format("Challenge validated for user {0} with usage {1}", username, usage));
		return certificate;
	}

	@Override
	public Certificate authenticate(String username, char[] password, boolean keepAlive) {
		return authenticate(username, password, "unknown", Usage.ANY, keepAlive);
	}

	@Override
	public Certificate authenticate(String username, char[] password, String source, Usage usage, boolean keepAlive) {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> internalAuthenticate(username, password, source, usage, keepAlive));
	}

	private Certificate internalAuthenticate(String username, char[] password, String source, Usage usage,
			boolean keepAlive) {
		DBC.PRE.assertNotEmpty("source must not be empty!", source);

		// we don't want the user to worry about whitespace
		username = trimOrEmpty(username);

		try {
			// username must be at least 2 characters in length
			if (username.length() < 2) {
				String msg = format("The given username ''{0}'' is shorter than 2 characters", username);
				throw new InvalidCredentialsException(msg);
			}

			// check the password
			User user = checkCredentialsAndUserState(username, password);

			// validate user has at least one role
			Set<String> userRoles = user.getRoles();
			if (userRoles.isEmpty())
				throw new InvalidCredentialsException(format("User {0} does not have any roles defined!", username));

			if (user.isPasswordChangeRequested()) {
				if (usage == Usage.SINGLE)
					throw new IllegalStateException("Password change requested!");
				usage = Usage.SET_PASSWORD;
			}

			// get 2 auth tokens
			String authToken = this.encryptionHandler.nextToken();

			// get next session id
			String sessionId = UUID.randomUUID().toString();

			// create a new certificate, with details of the user
			Certificate certificate = buildCertificate(usage, user, authToken, sessionId, source, ZonedDateTime.now(),
					keepAlive);

			PrivilegeContext privilegeContext = buildPrivilegeContext(certificate, user);
			this.privilegeContextMap.put(sessionId, privilegeContext);

			persistSessionsAsync();

			// save last login
			if (user.getHistory().isFirstLoginEmpty())
				user.getHistory().setFirstLogin(ZonedDateTime.now());
			user.getHistory().setLastLogin(ZonedDateTime.now());
			this.persistenceHandler.replaceUser(user);
			persistModelAsync();

			// log
			logger.info(format("User {0} authenticated: {1}", username, certificate));

			// return the certificate
			return certificate;

		} catch (PrivilegeException e) {
			throw e;
		} catch (RuntimeException e) {
			logger.error(e.getMessage(), e);
			String msg = "User {0} failed to authenticate: {1}";
			msg = format(msg, username, e.getMessage());
			throw new PrivilegeException(msg, e);
		} finally {
			clearPassword(password);
		}
	}

	@Override
	public Certificate authenticateSingleSignOn(Object data, boolean keepAlive) throws PrivilegeException {
		return authenticateSingleSignOn(data, "unknown", keepAlive);
	}

	@Override
	public Certificate authenticateSingleSignOn(Object data, String source, boolean keepAlive) {
		DBC.PRE.assertNotEmpty("source must not be empty!", source);
		if (this.ssoHandler == null)
			throw new IllegalStateException("The SSO Handler is not configured!");

		User user = this.ssoHandler.authenticateSingleSignOn(data);

		return this.lockingHandler.lockedExecuteWithResult(user.getUsername(),
				() -> internalAuthenticateSingleSignOn(user, source, keepAlive));
	}

	private Certificate internalAuthenticateSingleSignOn(User user, String source, boolean keepAlive)
			throws PrivilegeException {

		DBC.PRE.assertEquals("SSO Users must have UserState.REMOTE!", UserState.REMOTE, user.getUserState());
		user.getHistory().setLastLogin(ZonedDateTime.now());

		// persist this user
		User internalUser = this.persistenceHandler.getUser(user.getUsername());
		if (internalUser == null) {
			user.getHistory().setFirstLogin(ZonedDateTime.now());
			this.persistenceHandler.addUser(user);
		} else {
			user.getHistory().setFirstLogin(internalUser.getHistory().getFirstLogin());
			this.persistenceHandler.replaceUser(user);
		}
		persistModelAsync();

		// get 2 auth tokens
		String authToken = this.encryptionHandler.nextToken();

		// get next session id
		String sessionId = UUID.randomUUID().toString();

		// create a new certificate, with details of the user
		Certificate certificate = buildCertificate(Usage.ANY, user, authToken, sessionId, source, ZonedDateTime.now(),
				keepAlive);

		PrivilegeContext privilegeContext = buildPrivilegeContext(certificate, user);
		this.privilegeContextMap.put(sessionId, privilegeContext);

		persistSessionsAsync();

		// log
		logger.info(format("User {0} authenticated: {1}", user.getUsername(), certificate));

		return certificate;
	}

	@Override
	public Certificate refresh(Certificate certificate, String source) throws AccessDeniedException {
		return this.lockingHandler.lockedExecuteWithResult(certificate.getUsername(),
				() -> internalRefresh(certificate, source));
	}

	private Certificate internalRefresh(Certificate certificate, String source) throws AccessDeniedException {
		DBC.PRE.assertNotNull("certificate must not be null!", certificate);

		try {
			// username must be at least 2 characters in length
			if (!this.allowSessionRefresh)
				throw new AccessDeniedException("Refreshing of sessions not allowed!");

			validate(certificate);

			if (!certificate.isKeepAlive())
				throw new AccessDeniedException("Refreshing of session not allowed!");

			if (!certificate.getSource().equals(source)) {
				logger.error("Source of existing session {} is not the same as the refresh request's source {}",
						certificate.getSource(), source);
			}

			// check the password
			User user = this.persistenceHandler.getUser(certificate.getUsername());

			// get 2 auth tokens
			String authToken = this.encryptionHandler.nextToken();

			// get next session id
			String sessionId = UUID.randomUUID().toString();

			// create a new certificate, with details of the user
			Certificate refreshedCert = buildCertificate(certificate.getUsage(), user, authToken, sessionId, source,
					ZonedDateTime.now(), true);

			PrivilegeContext privilegeContext = buildPrivilegeContext(refreshedCert, user);
			this.privilegeContextMap.put(sessionId, privilegeContext);

			// invalidate the previous session
			invalidate(certificate);

			// log
			logger.info(format("User {0} refreshed session: {1}", user.getUsername(), refreshedCert));

			// return the certificate
			return refreshedCert;

		} catch (PrivilegeException e) {
			throw e;
		} catch (RuntimeException e) {
			logger.error(e.getMessage(), e);
			String msg = "User {0} failed to refresh session: {1}";
			msg = format(msg, certificate.getUsername(), e.getMessage());
			throw new PrivilegeException(msg, e);
		}
	}

	private Certificate buildCertificate(Usage usage, User user, String authToken, String sessionId, String source,
			ZonedDateTime loginTime, boolean keepAlive) {
		DBC.PRE.assertNotEmpty("source must not be empty!", source);
		Set<String> userRoles = user.getRoles();
		return new Certificate(usage, sessionId, user.getUsername(), user.getFirstname(), user.getLastname(),
				user.getUserState(), authToken, source, loginTime, keepAlive && this.allowSessionRefresh,
				user.getLocale(), userRoles, new HashMap<>(user.getProperties()));
	}

	private synchronized void persistModelAsync() {
		if (!this.autoPersistOnUserChangesData)
			return;

		// async execution, max. once per second
		if (this.persistModelTask != null)
			this.persistModelTask.cancel(true);
		this.persistModelTask = this.executorService.schedule(
				() -> this.lockingHandler.lockedExecute("persist-model", () -> {
					try {
						this.persistenceHandler.persist();
					} catch (Exception e) {
						logger.error("Failed to persist model!", e);
					}
				}), 1, TimeUnit.SECONDS);
	}

	private synchronized boolean persistSessionsAsync() {
		if (!this.persistSessions)
			return false;

		// async execution, max. once per second
		if (this.persistSessionsTask != null)
			this.persistSessionsTask.cancel(true);
		this.persistSessionsTask = this.executorService.schedule(
				() -> this.lockingHandler.lockedExecute("persist-sessions", () -> {

					List<Certificate> sessions = new ArrayList<>(this.privilegeContextMap.values()).stream()
							.map(PrivilegeContext::getCertificate).filter(c -> !c.getUserState().isSystem())
							.collect(Collectors.toList());

					try (OutputStream out = Files.newOutputStream(this.persistSessionsPath.toPath());
						 OutputStream outputStream = AesCryptoHelper.wrapEncrypt(this.secretKey, out)) {

						CertificateStubsDomWriter writer = new CertificateStubsDomWriter(sessions, outputStream);
						writer.write();
						outputStream.flush();

					} catch (Exception e) {
						logger.error("Failed to persist sessions!", e);
						if (this.persistSessionsPath.exists() && !this.persistSessionsPath.delete()) {
							logger.error("Failed to delete sessions file after failing to write to it, at " +
									this.persistSessionsPath.getAbsolutePath());
						}
					}
				}), 1, TimeUnit.SECONDS);

		return true;
	}

	private void loadSessions() {
		if (!this.persistSessions) {
			logger.info("Persisting of sessions not enabled, so not loading!.");
			return;
		}

		if (!this.persistSessionsPath.exists()) {
			logger.info("Sessions file does not exist");
			return;
		}

		if (!this.persistSessionsPath.isFile())
			throw new PrivilegeModelException(
					"Sessions data file is not a file but exists at " + this.persistSessionsPath.getAbsolutePath());

		List<CertificateStub> certificateStubs;
		try (InputStream fin = Files.newInputStream(this.persistSessionsPath.toPath());
			 InputStream inputStream = AesCryptoHelper.wrapDecrypt(this.secretKey, fin)) {

			CertificateStubsSaxReader reader = new CertificateStubsSaxReader(inputStream);
			certificateStubs = reader.read();

		} catch (Exception e) {
			if (getRootCause(e) instanceof SAXParseException)
				logger.error("Failed to load sessions: " + getRootCause(e).getMessage());
			else
				logger.error("Failed to load sessions!", e);
			if (!this.persistSessionsPath.delete())
				logger.error("Failed to delete session file at " + this.persistSessionsPath.getAbsolutePath());
			return;
		}

		if (certificateStubs.isEmpty()) {
			logger.info("No persisted sessions exist to be loaded.");
			return;
		}

		for (CertificateStub stub : certificateStubs) {
			Usage usage = stub.getUsage();
			String username = stub.getUsername();
			String sessionId = stub.getSessionId();
			String authToken = stub.getAuthToken();
			String source = stub.getSource();
			User user = this.persistenceHandler.getUser(username);
			if (user == null) {
				logger.error("Ignoring session data for missing user " + username);
				continue;
			}

			if (user.getUserState() == UserState.DISABLED || user.getUserState() == UserState.EXPIRED) {
				logger.error("Ignoring session data for disabled/expired user " + username);
				continue;
			}

			Set<String> userRoles = user.getRoles();
			if (userRoles.isEmpty()) {
				logger.error("Ignoring session data for user " + username + " which has not roles defined!");
				continue;
			}

			// create a new certificate, with details of the user
			Certificate certificate = buildCertificate(usage, user, authToken, sessionId, source, stub.getLoginTime(),
					stub.isKeepAlive());
			certificate.setLocale(stub.getLocale());
			certificate.setLastAccess(stub.getLastAccess());

			PrivilegeContext privilegeContext = buildPrivilegeContext(certificate, user);
			this.privilegeContextMap.put(sessionId, privilegeContext);
		}

		logger.info("Loaded " + this.privilegeContextMap.size() + " sessions.");
	}

	/**
	 * Checks the credentials and validates that the user may log in.
	 *
	 * @param username the username of the {@link User} to check against
	 * @param password the password of this user
	 *
	 * @return the {@link User} if the credentials are valid and the user may login
	 *
	 * @throws AccessDeniedException       if anything is wrong with the credentials or the user state
	 * @throws InvalidCredentialsException if the given credentials are invalid, the user does not exist, or has no
	 *                                     password set
	 */
	protected User checkCredentialsAndUserState(String username, char[] password)
			throws InvalidCredentialsException, AccessDeniedException {

		// and validate the password
		if (password == null || password.length < 3)
			throw new InvalidCredentialsException("Password is invalid!");

		// get user object
		User user = this.persistenceHandler.getUser(username);
		// no user means no authentication
		if (user == null) {
			String msg = format("There is no user defined with the username {0}", username);
			throw new InvalidCredentialsException(msg);
		}

		// make sure not a system user - they may not login
		if (user.getUserState() == UserState.SYSTEM) {
			String msg = "User {0} is a system user and may not login!";
			msg = format(msg, username);
			throw new InvalidCredentialsException(msg);
		}

		// validate if user is allowed to login
		// this also capture the trying to login of SYSTEM user
		if (user.getUserState() != UserState.ENABLED) {
			String msg = "User {0} does not have state {1} and can not login!";
			msg = format(msg, username, UserState.ENABLED);
			throw new AccessDeniedException(msg);
		}

		PasswordCrypt userPasswordCrypt = user.getPasswordCrypt();
		if (userPasswordCrypt == null || userPasswordCrypt.getPassword() == null)
			throw new InvalidCredentialsException(format("User {0} has no password and may not login!", username));

		// we only work with hashed passwords
		PasswordCrypt requestPasswordCrypt;
		if (userPasswordCrypt.getSalt() == null) {
			requestPasswordCrypt = this.encryptionHandler.hashPasswordWithoutSalt(password);
		} else if (userPasswordCrypt.getHashAlgorithm() == null || userPasswordCrypt.getHashIterations() == -1 ||
				userPasswordCrypt.getHashKeyLength() == -1) {
			requestPasswordCrypt = this.encryptionHandler.hashPassword(password, userPasswordCrypt.getSalt());
		} else {
			requestPasswordCrypt = this.encryptionHandler.hashPassword(password, userPasswordCrypt.getSalt(),
					userPasswordCrypt.getHashAlgorithm(), userPasswordCrypt.getHashIterations(),
					userPasswordCrypt.getHashKeyLength());
		}

		// validate password
		if (!Arrays.equals(requestPasswordCrypt.getPassword(), userPasswordCrypt.getPassword()))
			throw new InvalidCredentialsException(format("Password is incorrect for {0}", username));

		// see if we need to update the hash
		if (this.encryptionHandler.isPasswordCryptOutdated(userPasswordCrypt)) {

			logger.warn("Updating user " + username + " due to change in hashing algorithm properties ");

			// get new salt for user
			byte[] salt = this.encryptionHandler.nextSalt();

			// hash password
			PasswordCrypt newPasswordCrypt = this.encryptionHandler.hashPassword(password, salt);

			// create new user
			user = new User(user.getUserId(), user.getUsername(), newPasswordCrypt, user.getFirstname(),
					user.getLastname(), user.getUserState(), user.getRoles(), user.getLocale(), user.getProperties(),
					user.isPasswordChangeRequested(), user.getHistory().getClone());

			// delegate user replacement to persistence handler
			this.persistenceHandler.replaceUser(user);
			persistModelAsync();

			logger.info("Updated password for " + user.getUsername());
		}

		return user;
	}

	/**
	 * Builds a {@link PrivilegeContext} for the given {@link User} and its {@link Certificate}
	 *
	 * @param certificate the certificate for which to build the {@link PrivilegeContext}
	 * @param user        the user for which to build the {@link PrivilegeContext}
	 *
	 * @return the {@link PrivilegeContext}
	 */
	private PrivilegeContext buildPrivilegeContext(Certificate certificate, User user) {

		Set<String> userRoles = user.getRoles();
		Map<String, IPrivilege> privileges = new HashMap<>();
		Map<String, PrivilegePolicy> policies = new HashMap<>();

		// get a cache of the privileges and policies for this user
		for (String roleName : userRoles) {
			Role role = this.persistenceHandler.getRole(roleName);
			if (role == null) {
				logger.error("Role " + roleName + " does not exist for user " + user.getUsername());
				continue;
			}

			Set<String> privilegeNames = role.getPrivilegeNames();
			for (String privilegeName : privilegeNames) {

				IPrivilege privilege = role.getPrivilege(privilegeName);
				if (privilege == null) {
					logger.error(format("The Privilege {0} does not exist for role {1}", privilegeName, roleName));
					continue;
				}

				// cache the privilege
				if (privileges.containsKey(privilegeName)) {
					if (this.privilegeConflictResolution.isStrict()) {
						throw new PrivilegeModelException(
								format("User has conflicts for privilege {0} with role {1}", privilegeName, roleName));
					}

					IPrivilege priv = privileges.get(privilegeName);
					boolean allAllowed = priv.isAllAllowed() || privilege.isAllAllowed();
					Set<String> allowList;
					Set<String> denyList;
					if (allAllowed) {
						allowList = Collections.emptySet();
						denyList = Collections.emptySet();
					} else {
						allowList = new HashSet<>(priv.getAllowList());
						allowList.addAll(privilege.getAllowList());
						denyList = new HashSet<>(priv.getDenyList());
						denyList.addAll(privilege.getDenyList());
					}
					priv = new PrivilegeImpl(priv.getName(), priv.getPolicy(), allAllowed, denyList, allowList);

					privileges.put(privilegeName, priv);
					continue;
				}

				privileges.put(privilegeName, privilege);

				// cache the policy for the privilege
				String policyName = privilege.getPolicy();
				if (policies.containsKey(policyName))
					continue;

				PrivilegePolicy policy = getPolicy(policyName);
				if (policy == null) {
					logger.error(format("The Policy {0} does not exist for Privilege {1}", policyName, privilegeName));
					continue;
				}

				policies.put(policyName, policy);
			}
		}

		UserRep userRep = user.asUserRep();
		return new PrivilegeContext(userRep, certificate, privileges, policies);
	}

	@Override
	public boolean invalidate(Certificate certificate) {

		// remove registration
		PrivilegeContext privilegeContext = this.privilegeContextMap.remove(certificate.getSessionId());

		// persist sessions
		if (privilegeContext != null)
			persistSessionsAsync();

		// return true if object was really removed
		boolean loggedOut = privilegeContext != null;
		if (loggedOut)
			logger.info(format("User {0} logged out.", certificate.getUsername()));
		else
			logger.warn("User already logged out!");

		return loggedOut;
	}

	@Override
	public PrivilegeContext validate(Certificate certificate) throws PrivilegeException {
		return validate(certificate, "unknown");
	}

	@Override
	public void validateSystemSession(PrivilegeContext ctx) throws PrivilegeException {
		// ctx  must not be null
		if (ctx == null)
			throw new PrivilegeException("PrivilegeContext may not be null!");

		// validate user state is system
		if (ctx.getUserRep().getUserState() != UserState.SYSTEM) {
			String msg = "The PrivilegeContext's user {0} does not have expected user state {1}";
			msg = format(msg, ctx.getUserRep().getUsername(), UserState.SYSTEM);
			throw new PrivilegeException(msg);
		}

		// see if a session exists for this certificate
		Certificate certificate = ctx.getCertificate();
		PrivilegeContext privilegeContext = this.privilegeContextMap.get(certificate.getSessionId());
		if (privilegeContext == null) {
			String msg = format("There is no session information for {0}", certificate);
			throw new NotAuthenticatedException(msg);
		}

		// validate same privilege contexts
		if (ctx != privilegeContext) {
			String msg = format("The given PrivilegeContext {0} is not the same as registered under the sessionId {1}",
					ctx.getCertificate().getSessionId(), privilegeContext.getCertificate().getSessionId());
			throw new PrivilegeException(msg);
		}

		// validate certificate has not been tampered with
		Certificate sessionCertificate = privilegeContext.getCertificate();
		if (!sessionCertificate.equals(certificate)) {
			String msg = "Received illegal certificate for session id {0}";
			msg = format(msg, certificate.getSessionId());
			throw new PrivilegeException(msg);
		}

		certificate.setLastAccess(ZonedDateTime.now());

		if (!certificate.getSource().equals(this.identifier))
			throw new IllegalStateException(
					"Source has changed for certificate " + certificate + " to " + certificate.getSource());
	}

	@Override
	public PrivilegeContext validate(Certificate certificate, String source) throws PrivilegeException {
		DBC.PRE.assertNotEmpty("source must not be empty!", source);

		// certificate  must not be null
		if (certificate == null)
			throw new PrivilegeException("Certificate may not be null!");

		// first see if a session exists for this certificate
		PrivilegeContext privilegeContext = this.privilegeContextMap.get(certificate.getSessionId());
		if (privilegeContext == null) {
			String msg = format("There is no session information for {0}", certificate);
			throw new NotAuthenticatedException(msg);
		}

		// validate certificate has not been tampered with
		Certificate sessionCertificate = privilegeContext.getCertificate();
		if (!sessionCertificate.equals(certificate)) {
			String msg = "Received illegal certificate for session id {0}";
			msg = format(msg, certificate.getSessionId());
			throw new PrivilegeException(msg);
		}

		// validate that challenge certificate is not expired (1 hour only) 
		if (sessionCertificate.getUsage() != Usage.ANY) {
			ZonedDateTime dateTime = sessionCertificate.getLoginTime();
			if (dateTime.plusHours(1).isBefore(ZonedDateTime.now())) {
				invalidate(sessionCertificate);
				throw new NotAuthenticatedException("Certificate has already expired!");
			}
		}

		certificate.setLastAccess(ZonedDateTime.now());

		// assert source did not change
		if (this.disallowSourceChange && !source.equals(SOURCE_UNKNOWN) && !certificate.getSource().equals(source)) {
			invalidate(certificate);
			throw new NotAuthenticatedException("Source has changed for certificate " + certificate + " to " + source);
		}

		return privilegeContext;
	}

	@Override
	public void validatePassword(Locale locale, char[] password) throws PasswordStrengthException {
		if (!this.passwordStrengthHandler.validateStrength(password))
			throw new PasswordStrengthException(this.passwordStrengthHandler.getDescription(locale));
	}

	@Override
	public boolean persist(Certificate certificate) {

		// validate who is doing this
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_PERSIST));

		// persist non async
		return this.persistenceHandler.persist();
	}

	@Override
	public boolean persistSessions(Certificate certificate, String source) {

		// validate who is doing this
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_PERSIST_SESSIONS));

		return persistSessionsAsync();
	}

	@Override
	public void start() {
		this.lockingHandler.start();
	}

	@Override
	public void stop() {
		this.lockingHandler.stop();
	}

	@Override
	public boolean reload(Certificate certificate, String source) {

		// validate who is doing this
		PrivilegeContext prvCtx = validate(certificate, source);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_RELOAD));

		return this.persistenceHandler.reload();
	}

	/**
	 * Initializes the concrete {@link PrivilegeHandler}. The passed parameter map contains any configuration this
	 * {@link PrivilegeHandler} might need. This method may only be called once and this must be enforced by the
	 * concrete implementation
	 *
	 * @param parameterMap            a map containing configuration properties
	 * @param encryptionHandler       the {@link EncryptionHandler} instance for this {@link PrivilegeHandler}
	 * @param passwordStrengthHandler the {@link PasswordStrengthHandler} instance for this {@link PrivilegeHandler}
	 * @param persistenceHandler      the {@link PersistenceHandler} instance for this {@link PrivilegeHandler}
	 * @param userChallengeHandler    the handler to challenge a user's actions e.g. password change or authentication
	 * @param ssoHandler              the {@link SingleSignOnHandler}
	 * @param policyMap               map of {@link PrivilegePolicy} classes
	 *
	 * @throws PrivilegeException if this method is called multiple times or an initialization exception occurs
	 */
	public void initialize(ScheduledExecutorService executorService, Map<String, String> parameterMap,
			EncryptionHandler encryptionHandler, PasswordStrengthHandler passwordStrengthHandler,
			PersistenceHandler persistenceHandler, UserChallengeHandler userChallengeHandler,
			SingleSignOnHandler ssoHandler, Map<String, Class<PrivilegePolicy>> policyMap) {

		if (this.initialized)
			throw new PrivilegeModelException("Already initialized!");

		this.executorService = executorService;
		this.lockingHandler = new ElementLockingHandler<>(executorService, TimeUnit.SECONDS, 10L);
		this.policyMap = policyMap;
		this.encryptionHandler = encryptionHandler;
		this.passwordStrengthHandler = passwordStrengthHandler;
		this.persistenceHandler = persistenceHandler;
		this.userChallengeHandler = userChallengeHandler;
		this.ssoHandler = ssoHandler;

		handleAutoPersistOnUserDataChange(parameterMap);
		handlePersistSessionsParam(parameterMap);
		handleConflictResolutionParam(parameterMap);
		handleSecretParams(parameterMap);

		this.allowSessionRefresh = Boolean.parseBoolean(parameterMap.get(PARAM_ALLOW_SESSION_REFRESH));
		this.disallowSourceChange = Boolean.parseBoolean(parameterMap.get(PARAM_DISALLOW_SOURCE_CHANGE));

		// validate policies on privileges of Roles
		for (Role role : persistenceHandler.getAllRoles()) {
			validatePolicies(role);
		}

		// validate privilege conflicts
		validatePrivilegeConflicts();

		this.privilegeContextMap = new ConcurrentHashMap<>();

		loadSessions();

		this.parameterMap = parameterMap;
		this.initialized = true;
	}

	private void handleAutoPersistOnUserDataChange(Map<String, String> parameterMap) {
		String autoPersistS = parameterMap.get(PARAM_AUTO_PERSIST_ON_USER_CHANGES_DATA);
		if (isEmpty(autoPersistS) || autoPersistS.equals(Boolean.FALSE.toString())) {
			this.autoPersistOnUserChangesData = false;
		} else if (autoPersistS.equals(Boolean.TRUE.toString())) {
			this.autoPersistOnUserChangesData = true;
			logger.info("Enabling automatic persistence when user changes their data.");
		} else {
			String msg = "Parameter {0} has illegal value {1}. Overriding with {2}";
			msg = format(msg, PARAM_AUTO_PERSIST_ON_USER_CHANGES_DATA, autoPersistS, Boolean.FALSE);
			logger.error(msg);
			this.autoPersistOnUserChangesData = false;
		}
	}

	private void handlePersistSessionsParam(Map<String, String> parameterMap) {
		String persistSessionsS = parameterMap.get(PARAM_PERSIST_SESSIONS);
		if (isEmpty(persistSessionsS) || persistSessionsS.equals(Boolean.FALSE.toString())) {
			this.persistSessions = false;
		} else if (persistSessionsS.equals(Boolean.TRUE.toString())) {
			this.persistSessions = true;

			String persistSessionsPathS = parameterMap.get(PARAM_PERSIST_SESSIONS_PATH);
			if (isEmpty(persistSessionsPathS)) {
				String msg = "Parameter {0} has illegal value {1}.";
				msg = format(msg, PARAM_PERSIST_SESSIONS_PATH, persistSessionsPathS);
				throw new PrivilegeModelException(msg);
			}

			this.persistSessionsPath = getPersistSessionFile(persistSessionsPathS);
			logger.info(format("Enabling persistence of sessions to {0}", this.persistSessionsPath.getAbsolutePath()));
		} else {
			String msg = "Parameter {0} has illegal value {1}. Overriding with {2}";
			msg = format(msg, PARAM_PERSIST_SESSIONS, persistSessionsS, Boolean.FALSE);
			logger.error(msg);
			this.persistSessions = false;
		}
	}

	private static File getPersistSessionFile(String persistSessionsPathS) {
		File persistSessionsPath = new File(persistSessionsPathS);
		if (!persistSessionsPath.getParentFile().isDirectory()) {
			String msg = "Path for param {0} is invalid as parent does not exist or is not a directory. Value: {1}";
			msg = format(msg, PARAM_PERSIST_SESSIONS_PATH, persistSessionsPath.getAbsolutePath());
			throw new PrivilegeModelException(msg);
		}

		if (persistSessionsPath.exists() && (!persistSessionsPath.isFile() || !persistSessionsPath.canWrite())) {
			String msg = "Path for param {0} is invalid as file exists but is not a file or not writeable. Value: {1}";
			msg = format(msg, PARAM_PERSIST_SESSIONS_PATH, persistSessionsPath.getAbsolutePath());
			throw new PrivilegeModelException(msg);
		}
		return persistSessionsPath;
	}

	private void handleConflictResolutionParam(Map<String, String> parameterMap) {
		String privilegeConflictResolutionS = parameterMap.get(PARAM_PRIVILEGE_CONFLICT_RESOLUTION);
		if (privilegeConflictResolutionS == null) {
			this.privilegeConflictResolution = PrivilegeConflictResolution.MERGE;
			String msg = "No {0} parameter defined. Using {1}";
			msg = format(msg, PARAM_PRIVILEGE_CONFLICT_RESOLUTION, this.privilegeConflictResolution);
			logger.info(msg);
		} else {
			try {
				this.privilegeConflictResolution = PrivilegeConflictResolution.valueOf(privilegeConflictResolutionS);
			} catch (Exception e) {
				String msg = "Parameter {0} has illegal value {1}.";
				msg = format(msg, PARAM_PRIVILEGE_CONFLICT_RESOLUTION, privilegeConflictResolutionS);
				throw new PrivilegeModelException(msg);
			}
		}
		logger.info("Privilege conflict resolution set to " + this.privilegeConflictResolution);
	}

	private void handleSecretParams(Map<String, String> parameterMap) {

		String secretKeyS = parameterMap.get(PARAM_SECRET_KEY);
		if (isEmpty(secretKeyS)) {
			String msg = "Parameter {0} may not be empty";
			msg = format(msg, PARAM_SECRET_KEY);
			throw new PrivilegeModelException(msg);
		}

		String secretSaltS = parameterMap.get(PARAM_SECRET_SALT);
		if (isEmpty(secretSaltS)) {
			String msg = "Parameter {0} may not be empty";
			msg = format(msg, PARAM_SECRET_SALT);
			throw new PrivilegeModelException(msg);
		}

		this.secretKey = AesCryptoHelper.buildSecret(secretKeyS.toCharArray(), secretSaltS.getBytes());

		// build our identifier
		byte[] encrypt = AesCryptoHelper.encrypt(this.secretKey, "PrivilegeHandler".getBytes());
		this.identifier = Base64.getEncoder().encodeToString(encrypt);

		// remove secrets
		parameterMap.remove(PARAM_SECRET_KEY);
		parameterMap.remove(PARAM_SECRET_SALT);
	}

	private void validatePrivilegeConflicts() {
		if (!this.privilegeConflictResolution.isStrict()) {
			return;
		}

		List<String> conflicts = new ArrayList<>();
		List<User> users = this.persistenceHandler.getAllUsers();
		for (User user : users) {
			Map<String, String> privilegeNames = new HashMap<>();
			conflicts.addAll(detectPrivilegeConflicts(privilegeNames, user));
		}

		if (!conflicts.isEmpty()) {
			for (String conflict : conflicts) {
				logger.error(conflict);
			}
			throw new PrivilegeModelException("There are " + conflicts.size() + " privilege conflicts!");
		}
	}

	private void assertNoPrivilegeConflict(User user) {
		if (this.privilegeConflictResolution.isStrict()) {
			Map<String, String> privilegeNames = new HashMap<>();
			List<String> conflicts = detectPrivilegeConflicts(privilegeNames, user);
			if (!conflicts.isEmpty()) {
				String msg = String.join("\n", conflicts);
				throw new PrivilegeModelException(msg);
			}
		}
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

	private List<String> detectPrivilegeConflicts(Map<String, String> privilegeNames, User user) {
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

	/**
	 * Validates that the policies which are not null on the privileges of the role exist
	 *
	 * @param role the role for which the policies are to be checked
	 */
	private void validatePolicies(Role role) {
		for (String privilegeName : role.getPrivilegeNames()) {
			IPrivilege privilege = role.getPrivilege(privilegeName);
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
	private void clearPassword(char[] password) {
		if (password != null)
			Arrays.fill(password, (char) 0);
	}

	@Override
	public void runAs(String username, SystemAction action) throws Exception {

		PrivilegeContext systemUserPrivilegeContext = initiateSystemPrivilege(username, action);

		String sessionId = systemUserPrivilegeContext.getCertificate().getSessionId();
		try {
			// perform the action
			action.execute(systemUserPrivilegeContext);
		} finally {
			this.privilegeContextMap.remove(sessionId);
		}
	}

	@Override
	public <T> T runWithResult(String username, SystemActionWithResult<T> action) throws Exception {

		PrivilegeContext systemUserPrivilegeContext = initiateSystemPrivilege(username, action);

		String sessionId = systemUserPrivilegeContext.getCertificate().getSessionId();
		try {
			// perform the action
			return action.execute(systemUserPrivilegeContext);
		} finally {
			this.privilegeContextMap.remove(sessionId);
		}
	}

	@Override
	public PrivilegeContext openSystemUserContext(String username) throws PrivilegeException {

		// get privilegeContext for this system user
		PrivilegeContext systemUserPrivilegeContext = getSystemUserPrivilegeContext(username);

		String sessionId = systemUserPrivilegeContext.getCertificate().getSessionId();
		this.privilegeContextMap.put(sessionId, systemUserPrivilegeContext);

		return systemUserPrivilegeContext;
	}

	private PrivilegeContext initiateSystemPrivilege(String username, Restrictable restrictable) {
		if (username == null)
			throw new PrivilegeException("systemUsername may not be null!");
		if (restrictable == null)
			throw new PrivilegeException("action may not be null!");

		// get privilegeContext for this system user
		PrivilegeContext systemUserPrivilegeContext = getSystemUserPrivilegeContext(username);

		// validate this system user may perform the given action
		systemUserPrivilegeContext.validateAction(restrictable);

		String sessionId = systemUserPrivilegeContext.getCertificate().getSessionId();
		this.privilegeContextMap.put(sessionId, systemUserPrivilegeContext);

		return systemUserPrivilegeContext;
	}

	/**
	 * Returns the {@link Certificate} for the given system username. If it does not yet exist, then it is created by
	 * authenticating the system user
	 *
	 * @param systemUsername the name of the system user
	 *
	 * @return the {@link Certificate} for this system user
	 */
	private PrivilegeContext getSystemUserPrivilegeContext(String systemUsername) {

		// get user object
		User user = this.persistenceHandler.getUser(systemUsername);

		// no user means no authentication
		if (user == null) {
			String msg = format("The system user with username {0} does not exist!", systemUsername);
			throw new AccessDeniedException(msg);
		}

		// validate password
		if (user.getPasswordCrypt() != null) {
			String msg = format("System users must not have a password: {0}", user.getUsername());
			throw new AccessDeniedException(msg);
		}

		// validate user state is system
		if (user.getUserState() != UserState.SYSTEM) {
			String msg = "The system {0} user does not have expected user state {1}";
			msg = format(msg, user.getUsername(), UserState.SYSTEM);
			throw new PrivilegeException(msg);
		}

		// validate user has at least one role
		if (user.getRoles().isEmpty()) {
			String msg = format("The system user {0} does not have any roles defined!", user.getUsername());
			throw new PrivilegeException(msg);
		}

		// get 2 auth tokens
		String authToken = this.encryptionHandler.nextToken();

		// get next session id
		String sessionId = UUID.randomUUID().toString();

		// create a new certificate, with details of the user
		Certificate systemUserCertificate = buildCertificate(Usage.ANY, user, authToken, sessionId, this.identifier,
				ZonedDateTime.now(), false);

		// create and save a new privilege context
		PrivilegeContext privilegeContext = buildPrivilegeContext(systemUserCertificate, user);

		// log
		if (logger.isDebugEnabled()) {
			String msg = "The system user ''{0}'' is logged in with session {1}";
			msg = format(msg, user.getUsername(), systemUserCertificate.getSessionId());
			logger.info(msg);
		}

		return privilegeContext;
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
