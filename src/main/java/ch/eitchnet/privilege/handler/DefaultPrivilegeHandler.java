/*
 * Copyright (c) 2010 - 2012
 * 
 * This file is part of Privilege.
 *
 * Privilege is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Privilege.  If not, see <http://www.gnu.org/licenses/>.
 * 
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
import ch.eitchnet.privilege.helper.ClassHelper;
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
	private static final String PARAM_AUTO_PERSIST_ON_PASSWORD_CHANGE = "autoPersistOnPasswordChange";

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
		String selSurname = selectorRep.getSurname();
		UserState selUserState = selectorRep.getUserState();
		Locale selLocale = selectorRep.getLocale();
		Set<String> selRoles = selectorRep.getRoles();
		Map<String, String> selPropertyMap = selectorRep.getProperties();

		List<UserRep> result = new ArrayList<UserRep>();
		List<User> allUsers = this.persistenceHandler.getAllUsers();
		for (User user : allUsers) {

			// selections
			boolean userIdSelected;
			boolean usernameSelected;
			boolean firstnameSelected;
			boolean surnameSelected;
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

			// surname
			if (selSurname == null)
				surnameSelected = true;
			else if (selSurname.equals(user.getSurname()))
				surnameSelected = true;
			else
				surnameSelected = false;

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

			boolean selected = userIdSelected && usernameSelected && firstnameSelected && surnameSelected
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
					userRep.getSurname(), userRep.getUserState(), userRep.getRoles(), userRep.getLocale(),
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
			throw new PrivilegeException("Role " + roleName + " does not exist!");
		}

		// validate that policy exists if needed
		String policy = privilegeRep.getPolicy();
		if (policy != null && !this.policyMap.containsKey(policy)) {
			throw new PrivilegeException("Policy " + policy + " for Privilege " + privilegeRep.getName()
					+ " does not exist");
		}

		// create new role with the additional privilege
		IPrivilege newPrivilege = new PrivilegeImpl(privilegeRep);
		// copy existing privileges
		Set<String> existingPrivilegeNames = role.getPrivilegeNames();
		Map<String, IPrivilege> privilegeMap = new HashMap<String, IPrivilege>(existingPrivilegeNames.size() + 1);
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
			throw new PrivilegeException("User " + username + " does not exist!");
		}

		// ignore if user already has role
		Set<String> currentRoles = user.getRoles();
		if (currentRoles.contains(roleName)) {
			DefaultPrivilegeHandler.logger.error("User " + username + " already has role " + roleName);
			return;
		}

		// validate that role exists
		if (getRole(roleName) == null) {
			throw new PrivilegeException("Role " + roleName + " does not exist!");
		}

		// create new user
		Set<String> newRoles = new HashSet<String>(currentRoles);
		newRoles.add(roleName);

		User newUser = new User(user.getUserId(), user.getUsername(), user.getPassword(), user.getFirstname(),
				user.getSurname(), user.getUserState(), newRoles, user.getLocale(), user.getProperties());

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
			throw new PrivilegeException("Role " + roleName + " does not exist!");
		}

		// ignore if role does not have privilege
		if (!role.hasPrivilege(privilegeName))
			throw new PrivilegeException("Role " + roleName + " does not have Privilege " + privilegeName);

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
			throw new PrivilegeException("User " + username + " does not exist!");
		}

		// ignore if user does not have role
		Set<String> currentRoles = user.getRoles();
		if (!currentRoles.contains(roleName)) {
			DefaultPrivilegeHandler.logger.error("User " + user + " does not have role " + roleName);
			return;
		}

		// create new user
		Set<String> newRoles = new HashSet<String>(currentRoles);
		newRoles.remove(roleName);
		User newUser = new User(user.getUserId(), user.getUsername(), user.getPassword(), user.getFirstname(),
				user.getSurname(), user.getUserState(), newRoles, user.getLocale(), user.getProperties());

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
			throw new PrivilegeException("User " + username + " does not exist!");
		}

		// create new user
		User newUser = new User(user.getUserId(), user.getUsername(), user.getPassword(), user.getFirstname(),
				user.getSurname(), user.getUserState(), user.getRoles(), locale, user.getProperties());

		// delegate user replacement to persistence handler
		this.persistenceHandler.addOrReplaceUser(newUser);
	}

	@Override
	public void setUserName(Certificate certificate, String username, String firstname, String surname) {

		// validate who is doing this
		assertIsPrivilegeAdmin(certificate);

		// get User
		User user = this.persistenceHandler.getUser(username);
		if (user == null) {
			throw new PrivilegeException("User " + username + " does not exist!");
		}

		// create new user
		User newUser = new User(user.getUserId(), user.getUsername(), user.getPassword(), firstname, surname,
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
				throw new PrivilegeException("User " + username + " does not exist!");
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
					user.getSurname(), user.getUserState(), user.getRoles(), user.getLocale(), user.getProperties());

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
			throw new PrivilegeException("User " + username + " does not exist!");
		}

		// create new user
		User newUser = new User(user.getUserId(), user.getUsername(), user.getPassword(), user.getFirstname(),
				user.getSurname(), state, user.getRoles(), user.getLocale(), user.getProperties());

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
			if (username == null || username.length() < 2)
				throw new PrivilegeException("The given username '" + username + "' is shorter than 2 characters");

			// and validate the password
			validatePassword(password);

			// we only work with hashed passwords
			String passwordHash = this.encryptionHandler.convertToHash(password);

			// get user object
			User user = this.persistenceHandler.getUser(username);
			// no user means no authentication
			if (user == null)
				throw new AccessDeniedException("There is no user defined with the username " + username);

			// validate password
			String pwHash = user.getPassword();
			if (pwHash == null)
				throw new AccessDeniedException("User " + username + " has no password and may not login!");
			if (!pwHash.equals(passwordHash))
				throw new AccessDeniedException("Password is incorrect for " + username);

			// validate if user is allowed to login
			// this also capture the trying to login of SYSTEM user
			if (user.getUserState() != UserState.ENABLED)
				throw new AccessDeniedException("User " + username + " does not have state " + UserState.ENABLED
						+ " and can not login!");

			// validate user has at least one role
			Set<String> userRoles = user.getRoles();
			if (userRoles.isEmpty()) {
				throw new PrivilegeException("User " + username + " does not have any roles defined!");
			}

			// get 2 auth tokens
			String authToken = this.encryptionHandler.nextToken();
			String authPassword = this.encryptionHandler.nextToken();

			// get next session id
			String sessionId = nextSessionId();

			// create a new certificate, with details of the user
			certificate = new Certificate(sessionId, System.currentTimeMillis(), username, authToken, authPassword,
					user.getLocale(), new HashMap<String, String>(user.getProperties()));

			PrivilegeContext privilegeContext = buildPrivilegeContext(certificate, user);
			this.privilegeContextMap.put(sessionId, privilegeContext);

			// log
			DefaultPrivilegeHandler.logger.info("User " + username + " authenticated: " + certificate);

		} catch (RuntimeException e) {
			DefaultPrivilegeHandler.logger.error("User " + username + " Failed to authenticate: "
					+ e.getLocalizedMessage());
			throw e;
		} finally {
			clearPassword(password);
		}

		// return the certificate
		return certificate;
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
					throw new PrivilegeException(MessageFormat.format("The Privilege {0} does not exist for role {1}",
							privilegeName, roleName));
				}
				privileges.put(privilegeName, privilege);

				// cache the policy for the privilege
				String policyName = privilege.getPolicy();
				if (policies.containsKey(policyName))
					continue;

				PrivilegePolicy policy = getPolicy(policyName);
				if (policy == null) {
					throw new PrivilegeException(MessageFormat.format(
							"The Policy {0} does not exist for Privilege {1}", policyName, privilegeName));
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
			DefaultPrivilegeHandler.logger.info("User " + certificate.getUsername() + " logged out.");
		else
			DefaultPrivilegeHandler.logger.warn("User already logged out!");
		return loggedOut;
	}

	@Override
	public void isCertificateValid(Certificate certificate) {

		// certificate  must not be null
		if (certificate == null)
			throw new PrivilegeException("Certificate may not be null!");

		// first see if a session exists for this certificate
		PrivilegeContext privilegeContext = this.privilegeContextMap.get(certificate.getSessionId());
		if (privilegeContext == null)
			throw new AccessDeniedException("There is no session information for " + certificate.toString());

		// validate certificate has not been tampered with
		Certificate sessionCertificate = privilegeContext.getCertificate();
		if (!sessionCertificate.equals(certificate))
			throw new PrivilegeException("Received illegal certificate for session id " + certificate.getSessionId());

		// get user object
		User user = this.persistenceHandler.getUser(privilegeContext.getUsername());

		// if user exists, then certificate is valid
		if (user == null) {
			throw new PrivilegeException(
					"Oh boy, how did this happen: No User in user map although the certificate is valid!");
		}

		// everything is ok
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
			throw new PrivilegeException(
					"Oh boy, how did this happen: No User in user map although the certificate is valid! Certificate: "
							+ certificate);
		}

		// validate user has PrivilegeAdmin role
		if (!user.hasRole(PrivilegeHandler.PRIVILEGE_ADMIN_ROLE)) {
			throw new AccessDeniedException("User does not have " + PrivilegeHandler.PRIVILEGE_ADMIN_ROLE
					+ " role! Certificate: " + certificate);
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
			throw new PrivilegeException("A password may not be empty!");
		}

		if (password.length < 3) {
			throw new PrivilegeException("The given password is shorter than 3 characters");
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
			throw new PrivilegeException("Already initialized!");

		this.policyMap = policyMap;
		this.encryptionHandler = encryptionHandler;
		this.persistenceHandler = persistenceHandler;

		String autoPersistS = parameterMap.get(PARAM_AUTO_PERSIST_ON_PASSWORD_CHANGE);
		if (autoPersistS == null || autoPersistS.equals(Boolean.FALSE.toString())) {
			this.autoPersistOnPasswordChange = false;
		} else if (autoPersistS.equals(Boolean.TRUE.toString())) {
			this.autoPersistOnPasswordChange = true;
			logger.info("Enabling automatic persistence on password change.");
		} else {
			logger.error("Parameter " + PARAM_AUTO_PERSIST_ON_PASSWORD_CHANGE + " has illegal value " + autoPersistS
					+ ". Overriding with " + Boolean.FALSE.toString());
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
				throw new PrivilegeException("Policy " + policy + " for Privilege " + privilege.getName()
						+ " does not exist on role " + role);
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
			throw new PrivilegeException("systemUsername may not be null!");
		if (action == null)
			throw new PrivilegeException("action may not be null!");

		// get the system user
		User systemUser = this.persistenceHandler.getUser(systemUsername);
		if (systemUser == null)
			throw new PrivilegeException("System user " + systemUsername + " does not exist!");

		// validate this is a system user
		if (systemUser.getUserState() != UserState.SYSTEM)
			throw new PrivilegeException("User " + systemUsername + " is not a System user!");

		// validate this system user may perform the given action
		String actionClassname = action.getClass().getName();
		checkPrivilege(actionClassname, systemUser);

		// get certificate for this system user
		PrivilegeContext systemUserPrivilegeContext = getSystemUserPrivilegeContext(systemUsername);

		// perform the action
		action.execute(systemUserPrivilegeContext);
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
		throw new PrivilegeException("User " + user.getUsername() + " does not have Privilege " + privilegeName);
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

		// we only work with hashed passwords
		String passwordHash = this.encryptionHandler.convertToHash(systemUsername.getBytes());

		// get user object
		User user = this.persistenceHandler.getUser(systemUsername);
		// no user means no authentication
		if (user == null)
			throw new AccessDeniedException("The system user with username " + systemUsername + " does not exist!");

		// validate password
		String pwHash = user.getPassword();
		if (pwHash == null)
			throw new AccessDeniedException("System user " + systemUsername + " has no password and may not login!");
		if (!pwHash.equals(passwordHash))
			throw new AccessDeniedException("System user " + systemUsername + " has an incorrect password defined!");

		// validate user state is system
		if (user.getUserState() != UserState.SYSTEM)
			throw new PrivilegeException("The system " + systemUsername + " user does not have expected user state "
					+ UserState.SYSTEM);

		// validate user has at least one role
		if (user.getRoles().isEmpty()) {
			throw new PrivilegeException("The system user " + systemUsername + " does not have any roles defined!");
		}

		// get 2 auth tokens
		String authToken = this.encryptionHandler.nextToken();
		String authPassword = this.encryptionHandler.nextToken();

		// get next session id
		String sessionId = nextSessionId();

		// create a new certificate, with details of the user
		Certificate systemUserCertificate = new Certificate(sessionId, System.currentTimeMillis(), systemUsername,
				authToken, authPassword, user.getLocale(), new HashMap<String, String>(user.getProperties()));

		// create and save a new privilege context
		PrivilegeContext privilegeContext = buildPrivilegeContext(systemUserCertificate, user);

		// log
		DefaultPrivilegeHandler.logger.info("The system user " + systemUsername + " is logged in with session "
				+ systemUserCertificate);

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
			policy = ClassHelper.instantiateClass(policyClazz);
		} catch (Exception e) {
			throw new PrivilegeException("The class for the policy with the name " + policyName + " does not exist!"
					+ policyName, e);
		}

		return policy;
	}
}
