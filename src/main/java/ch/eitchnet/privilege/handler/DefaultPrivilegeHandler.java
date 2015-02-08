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

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.RoleRep;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.PrivilegeImpl;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

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
	 * configuration parameter to define automatic persisting on password change
	 */
	private static final String PARAM_AUTO_PERSIST_ON_PASSWORD_CHANGE = "autoPersistOnPasswordChange"; //$NON-NLS-1$

	/**
	 * slf4j logger
	 */
	protected static final Logger logger = LoggerFactory.getLogger(DefaultPrivilegeHandler.class);

	/**
	 * last assigned id for the {@link Certificate}s
	 */
	private long lastSessionId;

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
	 * flag to define if a persist should be performed after a user changes their password
	 */
	private boolean autoPersistOnPasswordChange;

	@Override
	public RoleRep getRole(String roleName) {
		Role role = this.persistenceHandler.getRole(roleName);
		if (role == null)
			return null;
		return role.asRoleRep();
	}

	@Override
	public UserRep getUser(String username) {
		User user = this.persistenceHandler.getUser(username);
		if (user == null)
			return null;
		return user.asUserRep();
	}

	@Override
	public List<UserRep> queryUsers(UserRep selectorRep) {

		String selUserId = selectorRep.getUserId();
		String selUsername = selectorRep.getUsername();
		String selFirstname = selectorRep.getFirstname();
		String selLastname = selectorRep.getLastname();
		UserState selUserState = selectorRep.getUserState();
		Locale selLocale = selectorRep.getLocale();
		Set<String> selRoles = selectorRep.getRoles();
		Map<String, String> selPropertyMap = selectorRep.getProperties();

		List<UserRep> result = new ArrayList<>();
		List<User> allUsers = this.persistenceHandler.getAllUsers();
		for (User user : allUsers) {

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
	public void addOrReplaceRole(Certificate certificate, RoleRep roleRep) {

		// validate who is doing this
		assertIsPrivilegeAdmin(certificate);

		// create new role from RoleRep
		Role role = new Role(roleRep);

		// validate policy if not null
		validatePolicies(role);

		// delegate to persistence handler
		this.persistenceHandler.addOrReplaceRole(role);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#addOrReplaceUser(ch.eitchnet.privilege.model.Certificate,
	 *      ch.eitchnet.privilege.model.UserRep, byte[])
	 */
	@Override
	public void addOrReplaceUser(Certificate certificate, UserRep userRep, byte[] password) {
		try {

			// validate who is doing this
			assertIsPrivilegeAdmin(certificate);

			String passwordHash = null;
			if (password != null) {

				// validate password meets basic requirements
				validatePassword(password);

				// hash password
				passwordHash = this.encryptionHandler.convertToHash(password);
			}

			// create new user
			User user = new User(userRep.getUserId(), userRep.getUsername(), passwordHash, userRep.getFirstname(),
					userRep.getLastname(), userRep.getUserState(), userRep.getRoles(), userRep.getLocale(),
					userRep.getProperties());

			// delegate to persistence handler
			this.persistenceHandler.addOrReplaceUser(user);

		} finally {
			clearPassword(password);
		}
	}

	@Override
	public void addOrReplacePrivilegeOnRole(Certificate certificate, String roleName, PrivilegeRep privilegeRep) {

		// validate who is doing this
		assertIsPrivilegeAdmin(certificate);

		// validate PrivilegeRep
		privilegeRep.validate();

		// get role
		Role role = this.persistenceHandler.getRole(roleName);
		if (role == null) {
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
		Set<String> existingPrivilegeNames = role.getPrivilegeNames();
		Map<String, IPrivilege> privilegeMap = new HashMap<>(existingPrivilegeNames.size() + 1);
		for (String name : existingPrivilegeNames) {
			IPrivilege privilege = role.getPrivilege(name);
			privilegeMap.put(name, privilege);
		}
		// add new one
		privilegeMap.put(newPrivilege.getName(), newPrivilege);

		Role newRole = new Role(role.getName(), privilegeMap);

		// delegate role replacement to persistence handler
		this.persistenceHandler.addOrReplaceRole(newRole);
	}

	@Override
	public void addRoleToUser(Certificate certificate, String username, String roleName) {

		// validate who is doing this
		assertIsPrivilegeAdmin(certificate);

		// get user
		User user = this.persistenceHandler.getUser(username);
		if (user == null) {
			throw new PrivilegeException(MessageFormat.format("User {0} does not exist!", username)); //$NON-NLS-1$
		}

		// ignore if user already has role
		Set<String> currentRoles = user.getRoles();
		if (currentRoles.contains(roleName)) {
			String msg = MessageFormat.format("User {0} already has role {1}", username, roleName); //$NON-NLS-1$
			DefaultPrivilegeHandler.logger.error(msg);
			return;
		}

		// validate that role exists
		if (getRole(roleName) == null) {
			String msg = MessageFormat.format("Role {0} does not exist!", roleName); //$NON-NLS-1$
			throw new PrivilegeException(msg);
		}

		// create new user
		Set<String> newRoles = new HashSet<>(currentRoles);
		newRoles.add(roleName);

		User newUser = new User(user.getUserId(), user.getUsername(), user.getPassword(), user.getFirstname(),
				user.getLastname(), user.getUserState(), newRoles, user.getLocale(), user.getProperties());

		// delegate user replacement to persistence handler
		this.persistenceHandler.addOrReplaceUser(newUser);
	}

	@Override
	public void removePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName) {

		// validate who is doing this
		assertIsPrivilegeAdmin(certificate);

		// get role
		Role role = this.persistenceHandler.getRole(roleName);
		if (role == null) {
			throw new PrivilegeException(MessageFormat.format("Role {0} does not exist!", roleName)); //$NON-NLS-1$
		}

		// ignore if role does not have privilege
		if (!role.hasPrivilege(privilegeName)) {
			String msg = MessageFormat.format("Role {0} does not have Privilege {1}", roleName, privilegeName); //$NON-NLS-1$
			throw new PrivilegeException(msg);
		}

		// create new set of privileges with out the to removed privilege
		Set<String> privilegeNames = role.getPrivilegeNames();
		Map<String, IPrivilege> newPrivileges = new HashMap<String, IPrivilege>(privilegeNames.size() - 1);
		for (String name : privilegeNames) {
			IPrivilege privilege = role.getPrivilege(name);
			if (!privilege.getName().equals(privilegeName))
				newPrivileges.put(privilege.getName(), privilege);
		}

		// create new role
		Role newRole = new Role(role.getName(), newPrivileges);

		// delegate user replacement to persistence handler
		this.persistenceHandler.addOrReplaceRole(newRole);
	}

	@Override
	public RoleRep removeRole(Certificate certificate, String roleName) {

		// validate who is doing this
		assertIsPrivilegeAdmin(certificate);

		// delegate role removal to persistence handler
		Role removedRole = this.persistenceHandler.removeRole(roleName);

		if (removedRole == null)
			return null;

		// return role rep if it was removed	
		return removedRole.asRoleRep();
	}

	@Override
	public void removeRoleFromUser(Certificate certificate, String username, String roleName) {

		// validate who is doing this
		assertIsPrivilegeAdmin(certificate);

		// get User
		User user = this.persistenceHandler.getUser(username);
		if (user == null) {
			throw new PrivilegeException(MessageFormat.format("User {0} does not exist!", username)); //$NON-NLS-1$
		}

		// ignore if user does not have role
		Set<String> currentRoles = user.getRoles();
		if (!currentRoles.contains(roleName)) {
			String msg = MessageFormat.format("User {0} does not have role {1}", user, roleName); //$NON-NLS-1$
			logger.error(msg);
			return;
		}

		// create new user
		Set<String> newRoles = new HashSet<String>(currentRoles);
		newRoles.remove(roleName);
		User newUser = new User(user.getUserId(), user.getUsername(), user.getPassword(), user.getFirstname(),
				user.getLastname(), user.getUserState(), newRoles, user.getLocale(), user.getProperties());

		// delegate user replacement to persistence handler
		this.persistenceHandler.addOrReplaceUser(newUser);
	}

	@Override
	public UserRep removeUser(Certificate certificate, String username) {

		// validate who is doing this
		assertIsPrivilegeAdmin(certificate);

		// delegate user removal to persistence handler
		User removedUser = this.persistenceHandler.removeUser(username);

		// return user rep if it was removed
		if (removedUser == null)
			return null;

		// return user rep if it was removed
		return removedUser.asUserRep();

	}

	@Override
	public void setUserLocale(Certificate certificate, String username, Locale locale) {

		// validate who is doing this
		assertIsPrivilegeAdmin(certificate);

		// get User
		User user = this.persistenceHandler.getUser(username);
		if (user == null) {
			throw new PrivilegeException(MessageFormat.format("User {0} does not exist!", username)); //$NON-NLS-1$
		}

		// create new user
		User newUser = new User(user.getUserId(), user.getUsername(), user.getPassword(), user.getFirstname(),
				user.getLastname(), user.getUserState(), user.getRoles(), locale, user.getProperties());

		// delegate user replacement to persistence handler
		this.persistenceHandler.addOrReplaceUser(newUser);
	}

	@Override
	public void setUserName(Certificate certificate, String username, String firstname, String lastname) {

		// validate who is doing this
		assertIsPrivilegeAdmin(certificate);

		// get User
		User user = this.persistenceHandler.getUser(username);
		if (user == null) {
			throw new PrivilegeException(MessageFormat.format("User {0} does not exist!", username)); //$NON-NLS-1$
		}

		// create new user
		User newUser = new User(user.getUserId(), user.getUsername(), user.getPassword(), firstname, lastname,
				user.getUserState(), user.getRoles(), user.getLocale(), user.getProperties());

		// delegate user replacement to persistence handler
		this.persistenceHandler.addOrReplaceUser(newUser);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#setUserPassword(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, byte[])
	 */
	@Override
	public void setUserPassword(Certificate certificate, String username, byte[] password) {
		try {

			// check if certificate is for same user, in which case user is changing their own password
			if (certificate.getUsername().equals(username)) {

				// validate the certificate
				isCertificateValid(certificate);

			} else {

				// otherwise validate the the certificate is for a privilege admin
				assertIsPrivilegeAdmin(certificate);
			}

			// get User
			User user = this.persistenceHandler.getUser(username);
			if (user == null) {
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
			User newUser = new User(user.getUserId(), user.getUsername(), passwordHash, user.getFirstname(),
					user.getLastname(), user.getUserState(), user.getRoles(), user.getLocale(), user.getProperties());

			// delegate user replacement to persistence handler
			this.persistenceHandler.addOrReplaceUser(newUser);

			// perform automatic persisting, if enabled
			if (this.autoPersistOnPasswordChange) {
				this.persistenceHandler.persist();
			}

		} finally {
			clearPassword(password);
		}
	}

	@Override
	public void setUserState(Certificate certificate, String username, UserState state) {

		// validate who is doing this
		assertIsPrivilegeAdmin(certificate);

		// get User
		User user = this.persistenceHandler.getUser(username);
		if (user == null) {
			throw new PrivilegeException(MessageFormat.format("User {0} does not exist!", username)); //$NON-NLS-1$
		}

		// create new user
		User newUser = new User(user.getUserId(), user.getUsername(), user.getPassword(), user.getFirstname(),
				user.getLastname(), state, user.getRoles(), user.getLocale(), user.getProperties());

		// delegate user replacement to persistence handler
		this.persistenceHandler.addOrReplaceUser(newUser);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#authenticate(java.lang.String, byte[])
	 * 
	 * @throws AccessDeniedException
	 *             if the user credentials are not valid
	 */
	@Override
	public Certificate authenticate(String username, byte[] password) {

		// create certificate
		Certificate certificate;
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
			String sessionId = nextSessionId();

			// create a new certificate, with details of the user
			certificate = new Certificate(sessionId, System.currentTimeMillis(), username, user.getFirstname(),
					user.getLastname(), authToken, user.getLocale(), userRoles, new HashMap<>(user.getProperties()));

			PrivilegeContext privilegeContext = buildPrivilegeContext(certificate, user);
			this.privilegeContextMap.put(sessionId, privilegeContext);

			// log
			DefaultPrivilegeHandler.logger.info(MessageFormat.format(
					"User {0} authenticated: {1}", username, certificate)); //$NON-NLS-1$

		} catch (RuntimeException e) {
			String msg = "User {0} Failed to authenticate: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, username, e.getMessage());
			DefaultPrivilegeHandler.logger.error(msg);
			throw e;
		} finally {
			clearPassword(password);
		}

		// return the certificate
		return certificate;
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
	 */
	private User checkCredentialsAndUserState(String username, byte[] password) throws AccessDeniedException {

		// and validate the password
		validatePassword(password);

		// we only work with hashed passwords
		String passwordHash = this.encryptionHandler.convertToHash(password);

		// get user object
		User user = this.persistenceHandler.getUser(username);
		// no user means no authentication
		if (user == null) {
			String msg = MessageFormat.format("There is no user defined with the username {0}", username); //$NON-NLS-1$
			throw new AccessDeniedException(msg);
		}

		// make sure not a system user - they may not login in
		if (user.getUserState() == UserState.SYSTEM) {
			String msg = "User {0} is a system user and may not login!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, username);
			throw new AccessDeniedException(msg);
		}

		// validate password
		String pwHash = user.getPassword();
		if (pwHash == null)
			throw new AccessDeniedException(MessageFormat.format(
					"User {0} has no password and may not login!", username)); //$NON-NLS-1$
		if (!pwHash.equals(passwordHash))
			throw new AccessDeniedException(MessageFormat.format("Password is incorrect for {0}", username)); //$NON-NLS-1$

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
		Map<String, IPrivilege> privileges = new HashMap<String, IPrivilege>();
		Map<String, PrivilegePolicy> policies = new HashMap<String, PrivilegePolicy>();

		// get a cache of the privileges and policies for this user
		for (String roleName : userRoles) {
			Role role = this.persistenceHandler.getRole(roleName);
			Set<String> privilegeNames = role.getPrivilegeNames();
			for (String privilegeName : privilegeNames) {

				// cache the privilege
				if (privileges.containsKey(privilegeName))
					continue;

				IPrivilege privilege = role.getPrivilege(privilegeName);
				if (privilege == null) {
					String msg = "The Privilege {0} does not exist for role {1}"; //$NON-NLS-1$
					msg = MessageFormat.format(msg, privilegeName, roleName);
					throw new PrivilegeException(msg);
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

	@Override
	public void assertIsPrivilegeAdmin(Certificate certificate) throws PrivilegeException {

		// validate certificate
		isCertificateValid(certificate);

		// get user object
		User user = this.persistenceHandler.getUser(certificate.getUsername());
		if (user == null) {
			String msg = "Oh boy, how did this happen: No User in user map although the certificate is valid! Certificate: {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, certificate);
			throw new PrivilegeException(msg);
		}

		// validate user has PrivilegeAdmin role
		if (!user.hasRole(PrivilegeHandler.PRIVILEGE_ADMIN_ROLE)) {
			String msg = "User does not have {0} role! Certificate: {1}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PrivilegeHandler.PRIVILEGE_ADMIN_ROLE, certificate);
			throw new AccessDeniedException(msg);
		}
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
		assertIsPrivilegeAdmin(certificate);

		return this.persistenceHandler.persist();
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

		String autoPersistS = parameterMap.get(PARAM_AUTO_PERSIST_ON_PASSWORD_CHANGE);
		if (autoPersistS == null || autoPersistS.equals(Boolean.FALSE.toString())) {
			this.autoPersistOnPasswordChange = false;
		} else if (autoPersistS.equals(Boolean.TRUE.toString())) {
			this.autoPersistOnPasswordChange = true;
			logger.info("Enabling automatic persistence on password change."); //$NON-NLS-1$
		} else {
			String msg = "Parameter {0} has illegal value {1}. Overriding with {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PARAM_AUTO_PERSIST_ON_PASSWORD_CHANGE, autoPersistS, Boolean.FALSE);
			logger.error(msg);
		}

		// validate policies on privileges of Roles
		for (Role role : persistenceHandler.getAllRoles()) {
			validatePolicies(role);
		}

		this.lastSessionId = 0l;
		this.privilegeContextMap = Collections.synchronizedMap(new HashMap<String, PrivilegeContext>());
		this.initialized = true;
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
	 * @return a new session id
	 */
	private synchronized String nextSessionId() {
		return Long.toString(++this.lastSessionId % Long.MAX_VALUE);
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
	public void runAsSystem(String systemUsername, SystemUserAction action) throws PrivilegeException {

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

		// validate this system user may perform the given action
		String actionClassname = action.getClass().getName();
		checkPrivilege(actionClassname, systemUser);

		// get certificate for this system user
		PrivilegeContext systemUserPrivilegeContext = getSystemUserPrivilegeContext(systemUsername);
		String sessionId = systemUserPrivilegeContext.getCertificate().getSessionId();
		this.privilegeContextMap.put(sessionId, systemUserPrivilegeContext);
		try {
			// perform the action
			action.execute(systemUserPrivilegeContext);
		} finally {
			this.privilegeContextMap.remove(sessionId);
		}
	}

	/**
	 * Checks if the given user has the given privilege
	 * 
	 * @param privilegeName
	 *            the name of the privilege to check on the user
	 * @param user
	 *            the user to check for the given privilege
	 * 
	 * @throws PrivilegeException
	 *             if the user does not have the privilege
	 */
	private void checkPrivilege(String privilegeName, User user) throws PrivilegeException {

		// check each role if it has the privilege
		for (String roleName : user.getRoles()) {

			Role role = this.persistenceHandler.getRole(roleName);

			// on the first occurrence of our privilege, stop
			if (role.hasPrivilege(privilegeName))
				return;
		}

		// default throw exception, as the user does not have the privilege
		String msg = MessageFormat.format("User {0} does not have Privilege {1}", user.getUsername(), privilegeName); //$NON-NLS-1$
		throw new PrivilegeException(msg);
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
		String sessionId = nextSessionId();

		// create a new certificate, with details of the user
		Certificate systemUserCertificate = new Certificate(sessionId, System.currentTimeMillis(), systemUsername,
				user.getFirstname(), user.getLastname(), authToken, user.getLocale(), user.getRoles(), new HashMap<>(
						user.getProperties()));

		// create and save a new privilege context
		PrivilegeContext privilegeContext = buildPrivilegeContext(systemUserCertificate, user);

		// log
		String msg = "The system user {0} is logged in with session {1}"; //$NON-NLS-1$
		msg = MessageFormat.format(msg, systemUsername, systemUserCertificate);
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
