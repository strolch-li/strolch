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
package ch.eitchnet.privilege.handler;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.crypto.SecretKey;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.InvalidCredentialsException;
import ch.eitchnet.privilege.base.PrivilegeConflictResolution;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.RoleRep;
import ch.eitchnet.privilege.model.SimpleRestrictable;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.PrivilegeImpl;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.privilege.policy.PrivilegePolicy;
import ch.eitchnet.privilege.xml.CertificateStubsDomWriter;
import ch.eitchnet.privilege.xml.CertificateStubsSaxReader;
import ch.eitchnet.privilege.xml.CertificateStubsSaxReader.CertificateStub;
import ch.eitchnet.utils.collections.Tuple;
import ch.eitchnet.utils.helper.AesCryptoHelper;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * <p>
 * This is default implementation of the {@link PrivilegeHandler}
 * </p>
 * 
 * The following list describes implementation details:
 * <ul>
 * <li>any methods which change the model are first validated by checking if the certificate is for an admin user by
 * calling {@link #assertIsPrivilegeAdmin(Certificate)}</li>
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

	/**
	 * slf4j logger
	 */
	protected static final Logger logger = LoggerFactory.getLogger(DefaultPrivilegeHandler.class);

	/**
	 * Map keeping a reference to all active sessions
	 */
	private Map<String, PrivilegeContext> privilegeContextMap;

	/**
	 * Map of {@link PrivilegePolicy} classes
	 */
	private Map<String, Class<PrivilegePolicy>> policyMap;

	/**
	 * The persistence handler is used for getting objects and saving changes
	 */
	private PersistenceHandler persistenceHandler;

	/**
	 * The encryption handler is used for generating hashes and tokens
	 */
	private EncryptionHandler encryptionHandler;

	/**
	 * flag to define if already initialized
	 */
	private boolean initialized;

	/**
	 * flag to define if a persist should be performed after a user changes their own data
	 */
	private boolean autoPersistOnUserChangesData;

	/**
	 * flag to define if sessions should be persisted
	 */
	private boolean persistSessions;

	/**
	 * Path to sessions file for persistence
	 */
	private File persistSessionsPath;

	/**
	 * Secret key
	 */
	private SecretKey secretKey;

	private PrivilegeConflictResolution privilegeConflictResolution;

	@Override
	public EncryptionHandler getEncryptionHandler() throws PrivilegeException {
		return this.encryptionHandler;
	}

	@Override
	public RoleRep getRole(Certificate certificate, String roleName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
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
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
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
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
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
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_GET_CERTIFICATES));

		return this.privilegeContextMap.values().stream().map(p -> p.getCertificate()).collect(Collectors.toList());
	}

	@Override
	public List<RoleRep> getRoles(Certificate certificate) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_GET_ROLE);

		Stream<Role> rolesStream = this.persistenceHandler.getAllRoles().stream();

		// validate access to each role
		// TODO throwing and catching exception ain't cool
		rolesStream = rolesStream.filter(role -> {
			try {
				prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_GET_ROLE, new Tuple(null, role)));
				return true;
			} catch (AccessDeniedException e) {
				return false;
			}
		});

		List<RoleRep> roles = rolesStream.map(r -> r.asRoleRep()).collect(Collectors.toList());
		return roles;
	}

	@Override
	public List<UserRep> getUsers(Certificate certificate) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_GET_USER);

		Stream<User> usersStream = this.persistenceHandler.getAllUsers().stream();

		// validate access to each user
		// TODO throwing and catching exception ain't cool
		usersStream = usersStream.filter(user -> {
			try {
				prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_GET_USER, new Tuple(null, user)));
				return true;
			} catch (AccessDeniedException e) {
				return false;
			}
		});

		List<UserRep> users = usersStream.map(u -> u.asUserRep()).collect(Collectors.toList());
		return users;
	}

	@Override
	public List<UserRep> queryUsers(Certificate certificate, UserRep selectorRep) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_GET_USER);

		String selUserId = selectorRep.getUserId();
		String selUsername = selectorRep.getUsername();
		String selFirstname = selectorRep.getFirstname();
		String selLastname = selectorRep.getLastname();
		UserState selUserState = selectorRep.getUserState();
		Locale selLocale = selectorRep.getLocale();
		Set<String> selRoles = selectorRep.getRoles();
		Map<String, String> selPropertyMap = selectorRep.getPropertyMap();

		List<UserRep> result = new ArrayList<>();
		List<User> allUsers = this.persistenceHandler.getAllUsers();
		for (User user : allUsers) {

			// TODO throwing and catching exception ain't cool
			try {
				prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_GET_USER, new Tuple(null, user)));
			} catch (AccessDeniedException e) {
				continue;
			}

			// selections
			boolean userIdSelected;
			boolean usernameSelected;
			boolean firstnameSelected;
			boolean lastnameSelected;
			boolean userStateSelected;
			boolean localeSelected;
			boolean roleSelected;
			boolean propertySelected;

			// userId
			if (selUserId == null)
				userIdSelected = true;
			else if (selUserId.equals(user.getUserId()))
				userIdSelected = true;
			else
				userIdSelected = false;

			// username
			if (selUsername == null)
				usernameSelected = true;
			else if (selUsername.equals(user.getUsername()))
				usernameSelected = true;
			else
				usernameSelected = false;

			// firstname
			if (selFirstname == null)
				firstnameSelected = true;
			else if (selFirstname.equals(user.getFirstname()))
				firstnameSelected = true;
			else
				firstnameSelected = false;

			// lastname
			if (selLastname == null)
				lastnameSelected = true;
			else if (selLastname.equals(user.getLastname()))
				lastnameSelected = true;
			else
				lastnameSelected = false;

			// user state
			if (selUserState == null)
				userStateSelected = true;
			else if (selUserState.equals(user.getUserState()))
				userStateSelected = true;
			else
				userStateSelected = false;

			// locale
			if (selLocale == null)
				localeSelected = true;
			else if (selLocale.equals(user.getLocale()))
				localeSelected = true;
			else
				localeSelected = false;

			// roles
			roleSelected = isSelectedByRole(selRoles, user.getRoles());

			// properties
			propertySelected = isSelectedByProperty(selPropertyMap, user.getProperties());

			boolean selected = userIdSelected && usernameSelected && firstnameSelected && lastnameSelected
					&& userStateSelected && localeSelected && roleSelected && propertySelected;

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
	 * @param selectionMap
	 *            the map defining the expected properties
	 * @param properties
	 *            the properties which must be a sub set of selectionMap to have this method return true
	 * 
	 * @return If the selectionMap is null or empty, then true is returned. If a key/value pair from the selectionMap is
	 *         not in the properties, then false is returned
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
	 * @param selectionRoles
	 *            the required roles
	 * @param roles
	 *            the roles to check if they contain the selectionRoles
	 * 
	 * @return Checks if the given roles contains the given selectionRoles, if this is the case, or selectionRoles is
	 *         null or empty, then true is returned, otherwise false
	 */
	private boolean isSelectedByRole(Set<String> selectionRoles, Set<String> roles) {

		if (selectionRoles == null)
			return true;

		return roles.containsAll(selectionRoles);
	}

	@Override
	public UserRep addUser(Certificate certificate, UserRep userRep, byte[] password) {
		try {

			// validate user actually has this type of privilege
			PrivilegeContext prvCtx = getPrivilegeContext(certificate);
			prvCtx.assertHasPrivilege(PRIVILEGE_ADD_USER);

			// make sure userId is not set
			if (StringHelper.isNotEmpty(userRep.getUserId())) {
				String msg = "UserId can not be set when adding a new user!";
				throw new PrivilegeException(MessageFormat.format(msg, userRep.getUsername()));
			}

			// set userId
			userRep.setUserId(StringHelper.getUniqueId());

			// first validate user
			userRep.validate();

			validateRolesExist(userRep);

			// validate user does not already exist
			if (this.persistenceHandler.getUser(userRep.getUsername()) != null) {
				String msg = "User {0} can not be added as it already exists!";
				throw new PrivilegeException(MessageFormat.format(msg, userRep.getUsername()));
			}

			String passwordHash = null;
			if (password != null) {

				// validate password meets basic requirements
				validatePassword(password);

				// hash password
				passwordHash = this.encryptionHandler.convertToHash(password);
			}

			// create new user
			User newUser = createUser(userRep, passwordHash);

			// detect privilege conflicts
			assertNoPrivilegeConflict(newUser);

			// validate this user may create such a user
			prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ADD_USER, new Tuple(null, newUser)));

			// delegate to persistence handler
			this.persistenceHandler.addUser(newUser);

			return newUser.asUserRep();

		} finally {
			clearPassword(password);
		}
	}

	@Override
	public UserRep replaceUser(Certificate certificate, UserRep userRep, byte[] password) {
		try {

			// validate user actually has this type of privilege
			PrivilegeContext prvCtx = getPrivilegeContext(certificate);
			prvCtx.assertHasPrivilege(PRIVILEGE_MODIFY_USER);

			// first validate user
			userRep.validate();

			validateRolesExist(userRep);

			// validate user exists
			User existingUser = this.persistenceHandler.getUser(userRep.getUsername());
			if (existingUser == null) {
				String msg = "User {0} can not be replaced as it does not exist!";
				throw new PrivilegeException(MessageFormat.format(msg, userRep.getUsername()));
			}

			// validate same userId
			if (!existingUser.getUserId().equals(userRep.getUserId())) {
				String msg = "UserId of existing user {0} does not match userRep {1}";
				msg = MessageFormat.format(msg, existingUser.getUserId(), userRep.getUserId());
				throw new PrivilegeException(MessageFormat.format(msg, userRep.getUsername()));
			}

			String passwordHash = null;
			if (password != null) {

				// validate password meets basic requirements
				validatePassword(password);

				// hash password
				passwordHash = this.encryptionHandler.convertToHash(password);
			}

			User newUser = createUser(userRep, passwordHash);

			// detect privilege conflicts
			assertNoPrivilegeConflict(newUser);

			// validate this user may modify this user
			prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_MODIFY_USER, new Tuple(existingUser, newUser)));

			// delegate to persistence handler
			this.persistenceHandler.replaceUser(newUser);

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
				msg = MessageFormat.format(msg, userRep.getUsername(), role);
				throw new PrivilegeException(msg);
			}
		}
	}

	private User createUser(UserRep userRep, String passwordHash) {
		User user = new User(userRep.getUserId(), userRep.getUsername(), passwordHash, userRep.getFirstname(),
				userRep.getLastname(), userRep.getUserState(), userRep.getRoles(), userRep.getLocale(),
				userRep.getPropertyMap());
		return user;
	}

	@Override
	public UserRep updateUser(Certificate certificate, UserRep userRep)
			throws AccessDeniedException, PrivilegeException {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_MODIFY_USER);

		// get existing user
		User existingUser = this.persistenceHandler.getUser(userRep.getUsername());
		if (existingUser == null) {
			throw new PrivilegeException(MessageFormat.format("User {0} does not exist!", userRep.getUsername())); //$NON-NLS-1$
		}

		// if nothing to do, then stop
		if (StringHelper.isEmpty(userRep.getFirstname()) && StringHelper.isEmpty(userRep.getLastname())
				&& userRep.getLocale() == null
				&& (userRep.getProperties() == null || userRep.getProperties().isEmpty())) {
			throw new PrivilegeException(MessageFormat.format("All updateable fields are empty for update of user {0}", //$NON-NLS-1$
					userRep.getUsername()));
		}

		String userId = existingUser.getUserId();
		String username = existingUser.getUsername();
		String password = existingUser.getPassword();
		String firstname = existingUser.getFirstname();
		String lastname = existingUser.getLastname();
		UserState userState = existingUser.getUserState();
		Set<String> roles = existingUser.getRoles();
		Locale locale = existingUser.getLocale();
		Map<String, String> propertyMap = existingUser.getProperties();

		// get updated fields
		if (StringHelper.isNotEmpty(userRep.getFirstname()))
			firstname = userRep.getFirstname();
		if (StringHelper.isNotEmpty(userRep.getLastname()))
			lastname = userRep.getLastname();
		if (userRep.getLocale() != null)
			locale = userRep.getLocale();
		if (userRep.getProperties() != null && !userRep.getProperties().isEmpty())
			propertyMap = userRep.getPropertyMap();

		// create new user
		User newUser = new User(userId, username, password, firstname, lastname, userState, roles, locale, propertyMap);

		// detect privilege conflicts
		assertNoPrivilegeConflict(newUser);

		// validate this user may modify this user
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_MODIFY_USER, new Tuple(existingUser, newUser)));

		// delegate to persistence handler
		this.persistenceHandler.replaceUser(newUser);

		return newUser.asUserRep();
	}

	@Override
	public UserRep removeUser(Certificate certificate, String username) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_REMOVE_USER);

		// validate user exists
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null) {
			String msg = "Can not remove User {0} because user does not exist!";
			throw new PrivilegeException(MessageFormat.format(msg, username));
		}

		// validate this user may remove this user
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_REMOVE_USER, new Tuple(null, existingUser)));

		// delegate user removal to persistence handler
		this.persistenceHandler.removeUser(username);

		return existingUser.asUserRep();
	}

	@Override
	public UserRep addRoleToUser(Certificate certificate, String username, String roleName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_ADD_ROLE_TO_USER);

		// get user
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null) {
			throw new PrivilegeException(MessageFormat.format("User {0} does not exist!", username)); //$NON-NLS-1$
		}

		// validate that this user may add this role to this user
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ADD_ROLE_TO_USER, new Tuple(existingUser, roleName)));

		// check that user not already has role
		Set<String> currentRoles = existingUser.getRoles();
		if (currentRoles.contains(roleName)) {
			String msg = MessageFormat.format("User {0} already has role {1}", username, roleName); //$NON-NLS-1$
			throw new PrivilegeException(msg);
		}

		// validate that the role exists
		if (this.persistenceHandler.getRole(roleName) == null) {
			String msg = MessageFormat.format("Role {0} does not exist!", roleName); //$NON-NLS-1$
			throw new PrivilegeException(msg);
		}

		// create new user
		Set<String> newRoles = new HashSet<>(currentRoles);
		newRoles.add(roleName);

		User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), existingUser.getPassword(),
				existingUser.getFirstname(), existingUser.getLastname(), existingUser.getUserState(), newRoles,
				existingUser.getLocale(), existingUser.getProperties());

		// detect privilege conflicts
		assertNoPrivilegeConflict(newUser);

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceUser(newUser);

		return newUser.asUserRep();
	}

	@Override
	public UserRep removeRoleFromUser(Certificate certificate, String username, String roleName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_REMOVE_ROLE_FROM_USER);

		// get User
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null) {
			throw new PrivilegeException(MessageFormat.format("User {0} does not exist!", username)); //$NON-NLS-1$
		}

		// validate that this user may remove this role from this user
		prvCtx.validateAction(
				new SimpleRestrictable(PRIVILEGE_REMOVE_ROLE_FROM_USER, new Tuple(existingUser, roleName)));

		// ignore if user does not have role
		Set<String> currentRoles = existingUser.getRoles();
		if (!currentRoles.contains(roleName)) {
			String msg = MessageFormat.format("User {0} does not have role {1}", existingUser.getUsername(), roleName); //$NON-NLS-1$
			throw new PrivilegeException(msg);
		}

		// create new user
		Set<String> newRoles = new HashSet<>(currentRoles);
		newRoles.remove(roleName);
		User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), existingUser.getPassword(),
				existingUser.getFirstname(), existingUser.getLastname(), existingUser.getUserState(), newRoles,
				existingUser.getLocale(), existingUser.getProperties());

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceUser(newUser);

		return newUser.asUserRep();
	}

	@Override
	public UserRep setUserLocale(Certificate certificate, String username, Locale locale) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_SET_USER_LOCALE);

		// get User
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null) {
			throw new PrivilegeException(MessageFormat.format("User {0} does not exist!", username)); //$NON-NLS-1$
		}

		// create new user
		User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), existingUser.getPassword(),
				existingUser.getFirstname(), existingUser.getLastname(), existingUser.getUserState(),
				existingUser.getRoles(), locale, existingUser.getProperties());

		// if the user is not setting their own locale, then make sure this user may set this user's locale
		if (!certificate.getUsername().equals(username)) {
			prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_SET_USER_LOCALE, new Tuple(existingUser, newUser)));
		}

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceUser(newUser);

		// perform automatic persisting, if enabled
		if (this.autoPersistOnUserChangesData) {
			this.persistenceHandler.persist();
		}

		return newUser.asUserRep();
	}

	@Override
	public void setUserPassword(Certificate certificate, String username, byte[] password) {
		try {

			// validate user actually has this type of privilege
			PrivilegeContext prvCtx = getPrivilegeContext(certificate);
			prvCtx.assertHasPrivilege(PRIVILEGE_SET_USER_PASSWORD);

			// get User
			User existingUser = this.persistenceHandler.getUser(username);
			if (existingUser == null) {
				throw new PrivilegeException(MessageFormat.format("User {0} does not exist!", username)); //$NON-NLS-1$
			}

			String passwordHash = null;
			if (password != null) {

				// validate password meets basic requirements
				validatePassword(password);

				// hash password
				passwordHash = this.encryptionHandler.convertToHash(password);
			}

			// create new user
			User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), passwordHash,
					existingUser.getFirstname(), existingUser.getLastname(), existingUser.getUserState(),
					existingUser.getRoles(), existingUser.getLocale(), existingUser.getProperties());

			// if the user is not setting their own password, then make sure this user may set this user's password
			if (!certificate.getUsername().equals(username)) {
				prvCtx.validateAction(
						new SimpleRestrictable(PRIVILEGE_SET_USER_PASSWORD, new Tuple(existingUser, newUser)));
			}

			// delegate user replacement to persistence handler
			this.persistenceHandler.replaceUser(newUser);

			// perform automatic persisting, if enabled
			if (this.autoPersistOnUserChangesData) {
				this.persistenceHandler.persist();
			}

		} finally {
			clearPassword(password);
		}
	}

	@Override
	public UserRep setUserState(Certificate certificate, String username, UserState state) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_SET_USER_STATE);

		// get User
		User existingUser = this.persistenceHandler.getUser(username);
		if (existingUser == null) {
			throw new PrivilegeException(MessageFormat.format("User {0} does not exist!", username)); //$NON-NLS-1$
		}

		// create new user
		User newUser = new User(existingUser.getUserId(), existingUser.getUsername(), existingUser.getPassword(),
				existingUser.getFirstname(), existingUser.getLastname(), state, existingUser.getRoles(),
				existingUser.getLocale(), existingUser.getProperties());

		// validate that this user may modify this user's state
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_SET_USER_STATE, new Tuple(existingUser, newUser)));

		// delegate user replacement to persistence handler
		this.persistenceHandler.replaceUser(newUser);

		return newUser.asUserRep();
	}

	@Override
	public RoleRep addRole(Certificate certificate, RoleRep roleRep) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_ADD_ROLE);

		// first validate role
		roleRep.validate();

		// validate role does not exist
		if (this.persistenceHandler.getRole(roleRep.getName()) != null) {
			String msg = MessageFormat.format("Can not add role {0} as it already exists!", roleRep.getName());
			throw new PrivilegeException(msg);
		}

		// create new role from RoleRep
		Role newRole = new Role(roleRep);

		// validate that this user may add this new role
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ADD_ROLE, new Tuple(null, newRole)));

		// validate policy if not null
		validatePolicies(newRole);

		// delegate to persistence handler
		this.persistenceHandler.addRole(newRole);

		return newRole.asRoleRep();
	}

	@Override
	public RoleRep replaceRole(Certificate certificate, RoleRep roleRep) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_MODIFY_ROLE);

		// first validate role
		roleRep.validate();

		// validate role does exist
		Role existingRole = this.persistenceHandler.getRole(roleRep.getName());
		if (existingRole == null) {
			String msg = MessageFormat.format("Can not replace role {0} as it does not exist!", roleRep.getName());
			throw new PrivilegeException(msg);
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

		return newRole.asRoleRep();
	}

	@Override
	public RoleRep removeRole(Certificate certificate, String roleName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_REMOVE_ROLE);

		// validate no user is using this role
		Set<String> roles = new HashSet<>(Arrays.asList(roleName));
		UserRep selector = new UserRep(null, null, null, null, null, roles, null, null);
		List<UserRep> usersWithRole = queryUsers(certificate, selector);
		if (!usersWithRole.isEmpty()) {
			String usersS = usersWithRole.stream().map(UserRep::getUsername).collect(Collectors.joining(", "));
			String msg = "The role {0} can not be removed as the following {1} user have the role assigned: {2}";
			msg = MessageFormat.format(msg, roleName, usersWithRole.size(), usersS);
			throw new PrivilegeException(msg);
		}

		// validate role exists
		Role existingRole = this.persistenceHandler.getRole(roleName);
		if (existingRole == null) {
			String msg = "Can not remove Role {0} because role does not exist!";
			throw new PrivilegeException(MessageFormat.format(msg, roleName));
		}

		// validate that this user may remove this role
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_REMOVE_ROLE, new Tuple(null, existingRole)));

		// delegate role removal to persistence handler
		this.persistenceHandler.removeRole(roleName);

		return existingRole.asRoleRep();
	}

	@Override
	public RoleRep addOrReplacePrivilegeOnRole(Certificate certificate, String roleName, PrivilegeRep privilegeRep) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_MODIFY_ROLE);

		// validate PrivilegeRep
		privilegeRep.validate();

		// get role
		Role existingRole = this.persistenceHandler.getRole(roleName);
		if (existingRole == null) {
			String msg = MessageFormat.format("Role {0} does not exist!", roleName); //$NON-NLS-1$
			throw new PrivilegeException(msg);
		}

		// validate that policy exists if needed
		String policy = privilegeRep.getPolicy();
		if (policy != null && !this.policyMap.containsKey(policy)) {
			String msg = "Policy {0} for Privilege {1} does not exist"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, policy, privilegeRep.getName());
			throw new PrivilegeException(msg);
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

		return newRole.asRoleRep();
	}

	@Override
	public RoleRep removePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_MODIFY_ROLE);

		// get role
		Role existingRole = this.persistenceHandler.getRole(roleName);
		if (existingRole == null) {
			throw new PrivilegeException(MessageFormat.format("Role {0} does not exist!", roleName)); //$NON-NLS-1$
		}

		// ignore if role does not have privilege
		if (!existingRole.hasPrivilege(privilegeName)) {
			String msg = MessageFormat.format("Role {0} does not have Privilege {1}", roleName, privilegeName); //$NON-NLS-1$
			throw new PrivilegeException(msg);
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

		return newRole.asRoleRep();
	}

	@Override
	public Certificate authenticate(String username, byte[] password) {

		try {
			// username must be at least 2 characters in length
			if (username == null || username.length() < 2) {
				String msg = MessageFormat.format("The given username ''{0}'' is shorter than 2 characters", username); //$NON-NLS-1$
				throw new PrivilegeException(msg);
			}

			// check the password
			User user = checkCredentialsAndUserState(username, password);

			// validate user has at least one role
			Set<String> userRoles = user.getRoles();
			if (userRoles.isEmpty()) {
				throw new PrivilegeException(
						MessageFormat.format("User {0} does not have any roles defined!", username)); //$NON-NLS-1$
			}

			// get 2 auth tokens
			String authToken = this.encryptionHandler.convertToHash(this.encryptionHandler.nextToken());

			// get next session id
			String sessionId = UUID.randomUUID().toString();

			// create a new certificate, with details of the user
			Certificate certificate = new Certificate(sessionId, username, user.getFirstname(), user.getLastname(),
					user.getUserState(), authToken, new Date(), user.getLocale(), userRoles,
					new HashMap<>(user.getProperties()));
			certificate.setLastAccess(new Date());

			PrivilegeContext privilegeContext = buildPrivilegeContext(certificate, user);
			this.privilegeContextMap.put(sessionId, privilegeContext);

			persistSessions();

			// log
			DefaultPrivilegeHandler.logger
					.info(MessageFormat.format("User {0} authenticated: {1}", username, certificate)); //$NON-NLS-1$

			// return the certificate
			return certificate;

		} catch (RuntimeException e) {
			String msg = "User {0} Failed to authenticate: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, username, e.getMessage());
			DefaultPrivilegeHandler.logger.error(msg);
			throw e;
		} finally {
			clearPassword(password);
		}
	}

	private boolean persistSessions() {
		if (!this.persistSessions)
			return false;

		List<Certificate> sessions = this.privilegeContextMap.values().stream().map(p -> p.getCertificate())
				.filter(c -> !c.getUserState().isSystem()).collect(Collectors.toList());

		try (FileOutputStream fout = new FileOutputStream(this.persistSessionsPath);
				OutputStream outputStream = AesCryptoHelper.wrapEncrypt(this.secretKey, fout)) {

			CertificateStubsDomWriter writer = new CertificateStubsDomWriter(sessions, outputStream);
			writer.write();
			outputStream.flush();

		} catch (Exception e) {
			throw new PrivilegeException("Failed to persist sessions!", e);
		}

		return true;
	}

	private boolean loadSessions() {
		if (!this.persistSessions) {
			logger.info("Persisteding of sessions not enabled, so not loading!.");
			return false;
		}

		if (!this.persistSessionsPath.exists()) {
			logger.info("No persisted sessions exist to be loaded.");
			return false;
		}

		if (!this.persistSessionsPath.isFile())
			throw new PrivilegeException(
					"Sessions data file is not a file but exists at " + this.persistSessionsPath.getAbsolutePath());

		List<CertificateStub> certificateStubs;
		try (FileInputStream fin = new FileInputStream(this.persistSessionsPath);
				InputStream inputStream = AesCryptoHelper.wrapDecrypt(this.secretKey, fin)) {

			CertificateStubsSaxReader reader = new CertificateStubsSaxReader(inputStream);
			certificateStubs = reader.read();

		} catch (Exception e) {
			logger.error("Failed to load sessions!", e);
			this.persistSessionsPath.delete();
			return false;
		}

		if (certificateStubs.isEmpty()) {
			logger.info("No persisted sessions exist to be loaded.");
			return false;
		}

		for (CertificateStub certificateStub : certificateStubs) {
			String username = certificateStub.getUsername();
			String sessionId = certificateStub.getSessionId();
			String authToken = certificateStub.getAuthToken();
			User user = this.persistenceHandler.getUser(username);
			if (user == null) {
				logger.error("Ignoring session data for missing user " + username);
				continue;
			}

			Set<String> userRoles = user.getRoles();
			if (userRoles.isEmpty()) {
				logger.error("Ignoring session data for user " + username + " which has not roles defined!");
				continue;
			}

			// create a new certificate, with details of the user
			Certificate certificate = new Certificate(sessionId, username, user.getFirstname(), user.getLastname(),
					user.getUserState(), authToken, certificateStub.getLoginTime(), certificateStub.getLocale(),
					userRoles, new HashMap<>(user.getProperties()));
			certificate.setLastAccess(certificateStub.getLastAccess());

			PrivilegeContext privilegeContext = buildPrivilegeContext(certificate, user);
			this.privilegeContextMap.put(sessionId, privilegeContext);
		}

		logger.info("Loaded " + this.privilegeContextMap.size() + " sessions.");
		return true;
	}

	/**
	 * Checks the credentials and validates that the user may log in.
	 * 
	 * @param username
	 *            the username of the {@link User} to check against
	 * @param password
	 *            the password of this user
	 * 
	 * @return the {@link User} if the credentials are valid and the user may login
	 * 
	 * @throws AccessDeniedException
	 *             if anything is wrong with the credentials or the user state
	 * @throws InvalidCredentialsException
	 *             if the given credentials are invalid, the user does not exist, or has no password set
	 */
	private User checkCredentialsAndUserState(String username, byte[] password)
			throws InvalidCredentialsException, AccessDeniedException {

		// and validate the password
		validatePassword(password);

		// we only work with hashed passwords
		String passwordHash = this.encryptionHandler.convertToHash(password);

		// get user object
		User user = this.persistenceHandler.getUser(username);
		// no user means no authentication
		if (user == null) {
			String msg = MessageFormat.format("There is no user defined with the username {0}", username); //$NON-NLS-1$
			throw new InvalidCredentialsException(msg);
		}

		// make sure not a system user - they may not login in
		if (user.getUserState() == UserState.SYSTEM) {
			String msg = "User {0} is a system user and may not login!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, username);
			throw new InvalidCredentialsException(msg);
		}

		// validate password
		String pwHash = user.getPassword();
		if (pwHash == null)
			throw new AccessDeniedException(
					MessageFormat.format("User {0} has no password and may not login!", username)); //$NON-NLS-1$
		if (!pwHash.equals(passwordHash))
			throw new InvalidCredentialsException(MessageFormat.format("Password is incorrect for {0}", username)); //$NON-NLS-1$

		// validate if user is allowed to login
		// this also capture the trying to login of SYSTEM user
		if (user.getUserState() != UserState.ENABLED) {
			String msg = "User {0} does not have state {1} and can not login!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, username, UserState.ENABLED);
			throw new AccessDeniedException(msg);
		}

		return user;
	}

	/**
	 * Builds a {@link PrivilegeContext} for the given {@link User} and its {@link Certificate}
	 * 
	 * @param certificate
	 * @param user
	 * 
	 * @return
	 */
	private PrivilegeContext buildPrivilegeContext(Certificate certificate, User user) {

		Set<String> userRoles = user.getRoles();
		Map<String, IPrivilege> privileges = new HashMap<>();
		Map<String, PrivilegePolicy> policies = new HashMap<>();

		// get a cache of the privileges and policies for this user
		for (String roleName : userRoles) {
			Role role = this.persistenceHandler.getRole(roleName);
			Set<String> privilegeNames = role.getPrivilegeNames();
			for (String privilegeName : privilegeNames) {

				IPrivilege privilege = role.getPrivilege(privilegeName);
				if (privilege == null) {
					String msg = "The Privilege {0} does not exist for role {1}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, privilegeName, roleName);
					throw new PrivilegeException(msg);
				}

				// cache the privilege
				if (privileges.containsKey(privilegeName)) {
					if (this.privilegeConflictResolution.isStrict()) {
						String msg = "User has conflicts for privilege {0} with role {1}";
						msg = MessageFormat.format(msg, privilegeName, roleName);
						throw new PrivilegeException(msg);
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
					String msg = "The Policy {0} does not exist for Privilege {1}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, policyName, privilegeName);
					throw new PrivilegeException(msg);
				}
				policies.put(policyName, policy);
			}
		}

		UserRep userRep = user.asUserRep();
		PrivilegeContext privilegeContext = new PrivilegeContext(userRep, certificate, privileges, policies);
		return privilegeContext;
	}

	@Override
	public boolean invalidateSession(Certificate certificate) {

		// first validate certificate
		isCertificateValid(certificate);

		// remove registration
		PrivilegeContext privilegeContext = this.privilegeContextMap.remove(certificate.getSessionId());

		// persist sessions
		persistSessions();

		// return true if object was really removed
		boolean loggedOut = privilegeContext != null;
		if (loggedOut)
			DefaultPrivilegeHandler.logger
					.info(MessageFormat.format("User {0} logged out.", certificate.getUsername())); //$NON-NLS-1$
		else
			DefaultPrivilegeHandler.logger.warn("User already logged out!"); //$NON-NLS-1$
		return loggedOut;
	}

	@Override
	public void isCertificateValid(Certificate certificate) {

		// certificate  must not be null
		if (certificate == null)
			throw new PrivilegeException("Certificate may not be null!"); //$NON-NLS-1$

		// first see if a session exists for this certificate
		PrivilegeContext privilegeContext = this.privilegeContextMap.get(certificate.getSessionId());
		if (privilegeContext == null) {
			String msg = MessageFormat.format("There is no session information for {0}", certificate); //$NON-NLS-1$
			throw new AccessDeniedException(msg);
		}

		// validate certificate has not been tampered with
		Certificate sessionCertificate = privilegeContext.getCertificate();
		if (!sessionCertificate.equals(certificate)) {
			String msg = "Received illegal certificate for session id {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, certificate.getSessionId());
			throw new PrivilegeException(msg);
		}

		// get user object
		User user = this.persistenceHandler.getUser(privilegeContext.getUsername());

		// if user exists, then certificate is valid
		if (user == null) {
			String msg = "Oh boy, how did this happen: No User in user map although the certificate is valid!"; //$NON-NLS-1$
			throw new PrivilegeException(msg);
		}

		// everything is ok
	}

	@Override
	public void checkPassword(Certificate certificate, byte[] password) throws PrivilegeException {
		try {
			isCertificateValid(certificate);
			checkCredentialsAndUserState(certificate.getUsername(), password);
		} finally {
			clearPassword(password);
		}
	}

	@Override
	public PrivilegeContext getPrivilegeContext(Certificate certificate) throws PrivilegeException {

		// first validate certificate
		isCertificateValid(certificate);

		return this.privilegeContextMap.get(certificate.getSessionId());
	}

	/**
	 * This simple implementation validates that the password is not null, and that the password string is not empty
	 * 
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#validatePassword(byte[])
	 */
	@Override
	public void validatePassword(byte[] password) throws PrivilegeException {

		if (password == null || password.length == 0) {
			throw new PrivilegeException("A password may not be empty!"); //$NON-NLS-1$
		}

		if (password.length < 3) {
			throw new PrivilegeException("The given password is shorter than 3 characters"); //$NON-NLS-1$
		}
	}

	@Override
	public boolean persist(Certificate certificate) {

		// validate who is doing this
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_PERSIST));

		return this.persistenceHandler.persist();
	}

	@Override
	public boolean persistSessions(Certificate certificate) {

		// validate who is doing this
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_PERSIST_SESSIONS));

		return persistSessions();
	}

	@Override
	public boolean reload(Certificate certificate) {

		// validate who is doing this
		PrivilegeContext prvCtx = getPrivilegeContext(certificate);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_RELOAD));

		return this.persistenceHandler.reload();
	}

	/**
	 * Initializes the concrete {@link EncryptionHandler}. The passed parameter map contains any configuration this
	 * {@link PrivilegeHandler} might need. This method may only be called once and this must be enforced by the
	 * concrete implementation
	 * 
	 * @param parameterMap
	 *            a map containing configuration properties
	 * @param encryptionHandler
	 *            the {@link EncryptionHandler} instance for this {@link PrivilegeHandler}
	 * @param persistenceHandler
	 *            the {@link PersistenceHandler} instance for this {@link PrivilegeHandler}
	 * @param policyMap
	 *            map of {@link PrivilegePolicy} classes
	 * 
	 * @throws PrivilegeException
	 *             if the this method is called multiple times or an initialization exception occurs
	 */
	public synchronized void initialize(Map<String, String> parameterMap, EncryptionHandler encryptionHandler,
			PersistenceHandler persistenceHandler, Map<String, Class<PrivilegePolicy>> policyMap) {

		if (this.initialized)
			throw new PrivilegeException("Already initialized!"); //$NON-NLS-1$

		this.policyMap = policyMap;
		this.encryptionHandler = encryptionHandler;
		this.persistenceHandler = persistenceHandler;

		handleAutoPersistOnUserDataChange(parameterMap);
		handlePersistSessionsParam(parameterMap);
		handleConflictResolutionParam(parameterMap);
		handleSecretParams(parameterMap);

		// validate policies on privileges of Roles
		for (Role role : persistenceHandler.getAllRoles()) {
			validatePolicies(role);
		}

		// validate privilege conflicts
		validatePrivilegeConflicts();

		this.privilegeContextMap = Collections.synchronizedMap(new HashMap<String, PrivilegeContext>());

		loadSessions();

		this.initialized = true;
	}

	private void handleAutoPersistOnUserDataChange(Map<String, String> parameterMap) {
		String autoPersistS = parameterMap.get(PARAM_AUTO_PERSIST_ON_USER_CHANGES_DATA);
		if (StringHelper.isEmpty(autoPersistS) || autoPersistS.equals(Boolean.FALSE.toString())) {
			this.autoPersistOnUserChangesData = false;
		} else if (autoPersistS.equals(Boolean.TRUE.toString())) {
			this.autoPersistOnUserChangesData = true;
			logger.info("Enabling automatic persistence when user changes their data."); //$NON-NLS-1$
		} else {
			String msg = "Parameter {0} has illegal value {1}. Overriding with {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PARAM_AUTO_PERSIST_ON_USER_CHANGES_DATA, autoPersistS, Boolean.FALSE);
			logger.error(msg);
			this.autoPersistOnUserChangesData = false;
		}
	}

	private void handlePersistSessionsParam(Map<String, String> parameterMap) {
		String persistSessionsS = parameterMap.get(PARAM_PERSIST_SESSIONS);
		if (StringHelper.isEmpty(persistSessionsS) || persistSessionsS.equals(Boolean.FALSE.toString())) {
			this.persistSessions = false;
		} else if (persistSessionsS.equals(Boolean.TRUE.toString())) {
			this.persistSessions = true;

			String persistSessionsPathS = parameterMap.get(PARAM_PERSIST_SESSIONS_PATH);
			if (StringHelper.isEmpty(persistSessionsPathS)) {
				String msg = "Parameter {0} has illegal value {1}."; //$NON-NLS-1$
				msg = MessageFormat.format(msg, PARAM_PERSIST_SESSIONS_PATH, persistSessionsPathS);
				throw new PrivilegeException(msg);
			}

			File persistSessionsPath = new File(persistSessionsPathS);
			if (!persistSessionsPath.getParentFile().isDirectory()) {
				String msg = "Path for param {0} is invalid as parent does not exist or is not a directory. Value: {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, PARAM_PERSIST_SESSIONS_PATH, persistSessionsPath.getAbsolutePath());
				throw new PrivilegeException(msg);
			}

			if (persistSessionsPath.exists() && (!persistSessionsPath.isFile() || !persistSessionsPath.canWrite())) {
				String msg = "Path for param {0} is invalid as file exists but is not a file or not writeable. Value: {1}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, PARAM_PERSIST_SESSIONS_PATH, persistSessionsPath.getAbsolutePath());
				throw new PrivilegeException(msg);
			}

			this.persistSessionsPath = persistSessionsPath;
			logger.info("Enabling persistence of sessions."); //$NON-NLS-1$
		} else {
			String msg = "Parameter {0} has illegal value {1}. Overriding with {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PARAM_PERSIST_SESSIONS, persistSessionsS, Boolean.FALSE);
			logger.error(msg);
			this.persistSessions = false;
		}
	}

	private void handleConflictResolutionParam(Map<String, String> parameterMap) {
		String privilegeConflictResolutionS = parameterMap.get(PARAM_PRIVILEGE_CONFLICT_RESOLUTION);
		if (privilegeConflictResolutionS == null) {
			this.privilegeConflictResolution = PrivilegeConflictResolution.STRICT;
			String msg = "No {0} parameter defined. Using {1}";
			msg = MessageFormat.format(msg, PARAM_PRIVILEGE_CONFLICT_RESOLUTION, this.privilegeConflictResolution);
			logger.info(msg);
		} else {
			try {
				this.privilegeConflictResolution = PrivilegeConflictResolution.valueOf(privilegeConflictResolutionS);
			} catch (Exception e) {
				String msg = "Parameter {0} has illegal value {1}."; //$NON-NLS-1$
				msg = MessageFormat.format(msg, PARAM_PRIVILEGE_CONFLICT_RESOLUTION, privilegeConflictResolutionS);
				throw new PrivilegeException(msg);
			}
		}
		logger.info("Privilege conflict resolution set to " + this.privilegeConflictResolution); //$NON-NLS-1$
	}

	private void handleSecretParams(Map<String, String> parameterMap) {

		if (!this.persistSessions)
			return;

		String secretKeyS = parameterMap.get(PARAM_SECRET_KEY);
		if (StringHelper.isEmpty(secretKeyS)) {
			String msg = "Parameter {0} may not be empty if parameter {1} is enabled."; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PARAM_SECRET_KEY, PARAM_PRIVILEGE_CONFLICT_RESOLUTION);
			throw new PrivilegeException(msg);
		}

		String secretSaltS = parameterMap.get(PARAM_SECRET_SALT);
		if (StringHelper.isEmpty(secretSaltS)) {
			String msg = "Parameter {0} may not be empty if parameter {1} is enabled."; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PARAM_SECRET_SALT, PARAM_PRIVILEGE_CONFLICT_RESOLUTION);
			throw new PrivilegeException(msg);
		}

		this.secretKey = AesCryptoHelper.buildSecret(secretKeyS.toCharArray(), secretSaltS.getBytes());
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
			throw new PrivilegeException("There are " + conflicts.size() + " privilege conflicts!");
		}
	}

	private void assertNoPrivilegeConflict(User user) {
		if (this.privilegeConflictResolution.isStrict()) {
			Map<String, String> privilegeNames = new HashMap<>();
			List<String> conflicts = detectPrivilegeConflicts(privilegeNames, user);
			if (!conflicts.isEmpty()) {
				String msg = conflicts.stream().collect(Collectors.joining("\n"));
				throw new PrivilegeException(msg);
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
			String msg = conflicts.stream().collect(Collectors.joining("\n"));
			throw new PrivilegeException(msg);
		}
	}

	private List<String> detectPrivilegeConflicts(Map<String, String> privilegeNames, User user) {
		List<String> conflicts = new ArrayList<>();

		Set<String> userRoles = user.getRoles();
		for (String roleName : userRoles) {
			Role role = this.persistenceHandler.getRole(roleName);
			for (String privilegeName : role.getPrivilegeNames()) {
				if (!privilegeNames.containsKey(privilegeName)) {
					privilegeNames.put(privilegeName, roleName);
				} else {
					String roleOrigin = privilegeNames.get(privilegeName);
					String msg = "User {0} has conflicts for privilege {1} on roles {2} and {3}";
					msg = MessageFormat.format(msg, user.getUsername(), privilegeName, roleOrigin, roleName);
					conflicts.add(msg);
				}
			}
		}

		return conflicts;
	}

	/**
	 * Validates that the policies which are not null on the privileges of the role exist
	 * 
	 * @param role
	 *            the role for which the policies are to be checked
	 */
	private void validatePolicies(Role role) {
		for (String privilegeName : role.getPrivilegeNames()) {
			IPrivilege privilege = role.getPrivilege(privilegeName);
			String policy = privilege.getPolicy();
			if (policy != null && !this.policyMap.containsKey(policy)) {
				String msg = "Policy {0} for Privilege {1} does not exist on role {2}"; //$NON-NLS-1$
				msg = MessageFormat.format(msg, policy, privilege.getName(), role);
				throw new PrivilegeException(msg);
			}
		}
	}

	/**
	 * Passwords should not be kept as strings, as string are immutable, this method thus clears the byte array so that
	 * the password is not in memory anymore
	 * 
	 * @param password
	 *            the byte array containing the passwort which is to be set to zeroes
	 */
	private void clearPassword(byte[] password) {
		if (password != null) {
			for (int i = 0; i < password.length; i++) {
				password[i] = 0;
			}
		}
	}

	@Override
	public <T extends SystemUserAction> T runAsSystem(String systemUsername, T action) throws PrivilegeException {

		if (systemUsername == null)
			throw new PrivilegeException("systemUsername may not be null!"); //$NON-NLS-1$
		if (action == null)
			throw new PrivilegeException("action may not be null!"); //$NON-NLS-1$

		// get the system user
		User systemUser = this.persistenceHandler.getUser(systemUsername);
		if (systemUser == null)
			throw new PrivilegeException(MessageFormat.format("System user {0} does not exist!", systemUsername)); //$NON-NLS-1$

		// validate this is a system user
		if (systemUser.getUserState() != UserState.SYSTEM)
			throw new PrivilegeException(MessageFormat.format("User {0} is not a System user!", systemUsername)); //$NON-NLS-1$

		// get privilegeContext for this system user
		PrivilegeContext systemUserPrivilegeContext = getSystemUserPrivilegeContext(systemUsername);

		// validate this system user may perform the given action
		systemUserPrivilegeContext.validateAction(action);

		String sessionId = systemUserPrivilegeContext.getCertificate().getSessionId();
		this.privilegeContextMap.put(sessionId, systemUserPrivilegeContext);
		try {
			// perform the action
			action.execute(systemUserPrivilegeContext);
		} finally {
			this.privilegeContextMap.remove(sessionId);
		}

		return action;
	}

	/**
	 * Returns the {@link Certificate} for the given system username. If it does not yet exist, then it is created by
	 * authenticating the system user
	 * 
	 * @param systemUsername
	 *            the name of the system user
	 * 
	 * @return the {@link Certificate} for this system user
	 */
	private PrivilegeContext getSystemUserPrivilegeContext(String systemUsername) {

		// get user object
		User user = this.persistenceHandler.getUser(systemUsername);
		// no user means no authentication
		if (user == null) {
			String msg = MessageFormat.format("The system user with username {0} does not exist!", systemUsername); //$NON-NLS-1$
			throw new AccessDeniedException(msg);
		}

		// validate password
		String pwHash = user.getPassword();
		if (pwHash != null) {
			String msg = MessageFormat.format("System users must not have a password: {0}", systemUsername); //$NON-NLS-1$
			throw new AccessDeniedException(msg);
		}

		// validate user state is system
		if (user.getUserState() != UserState.SYSTEM) {
			String msg = "The system {0} user does not have expected user state {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, systemUsername, UserState.SYSTEM);
			throw new PrivilegeException(msg);
		}

		// validate user has at least one role
		if (user.getRoles().isEmpty()) {
			String msg = MessageFormat.format("The system user {0} does not have any roles defined!", systemUsername); //$NON-NLS-1$
			throw new PrivilegeException(msg);
		}

		// get 2 auth tokens
		String authToken = this.encryptionHandler.nextToken();

		// get next session id
		String sessionId = UUID.randomUUID().toString();

		// create a new certificate, with details of the user
		Certificate systemUserCertificate = new Certificate(sessionId, systemUsername, user.getFirstname(),
				user.getLastname(), user.getUserState(), authToken, new Date(), user.getLocale(), user.getRoles(),
				new HashMap<>(user.getProperties()));
		systemUserCertificate.setLastAccess(new Date());

		// create and save a new privilege context
		PrivilegeContext privilegeContext = buildPrivilegeContext(systemUserCertificate, user);

		// log
		String msg = "The system user ''{0}'' is logged in with session {1}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, systemUsername, systemUserCertificate.getSessionId());
		DefaultPrivilegeHandler.logger.info(msg);

		return privilegeContext;
	}

	/**
	 * <p>
	 * This method instantiates a {@link PrivilegePolicy} object from the given policyName. The {@link PrivilegePolicy}
	 * is not stored in a database. The privilege name is a class name and is then used to instantiate a new
	 * {@link PrivilegePolicy} object
	 * </p>
	 * 
	 * @param policyName
	 *            the class name of the {@link PrivilegePolicy} object to return
	 * 
	 * @return the {@link PrivilegePolicy} object
	 * 
	 * @throws PrivilegeException
	 *             if the {@link PrivilegePolicy} object for the given policy name could not be instantiated
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

			policy = policyClazz.newInstance();
		} catch (Exception e) {
			String msg = "The class for the policy with the name {0} does not exist!{1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, policyName, policyName);
			throw new PrivilegeException(msg, e);
		}

		return policy;
	}
}
