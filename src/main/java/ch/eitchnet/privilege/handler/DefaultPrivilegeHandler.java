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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;

import ch.eitchnet.privilege.helper.ClassHelper;
import ch.eitchnet.privilege.i18n.AccessDeniedException;
import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.RoleRep;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.Session;
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
 * calling {@link #validateIsPrivilegeAdmin(Certificate)}</li>
 * <li>all model requests are delegated to the configured {@link PrivilegeHandler}, except for the session id to
 * {@link Certificate} map, no model data is kept in this implementation. This also means that to return the
 * representation objects, for every new model query, a new representation object is created</li>
 * <li>when creating new users, or editing users then a null password is understood as no password set</li>
 * <li>Password requirements are simple: Non null and non empty/length 0</li>
 * </ul>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class DefaultPrivilegeHandler implements PrivilegeHandler {

	/**
	 * log4j logger
	 */
	protected static final Logger logger = Logger.getLogger(DefaultPrivilegeHandler.class);

	/**
	 * last assigned id for the {@link Session}s
	 */
	private long lastSessionId;

	/**
	 * This map stores certificates for system users as a cache
	 */
	private Map<String, Certificate> systemUserCertificateMap;

	/**
	 * Map keeping a reference to all active sessions with their certificates
	 */
	private Map<String, CertificateSessionPair> sessionMap;

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
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#getRole(java.lang.String)
	 */
	@Override
	public RoleRep getRole(String roleName) {
		Role role = this.persistenceHandler.getRole(roleName);
		if (role == null)
			return null;
		return role.asRoleRep();
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#getUser(java.lang.String)
	 */
	@Override
	public UserRep getUser(String username) {
		User user = this.persistenceHandler.getUser(username);
		if (user == null)
			return null;
		return user.asUserRep();
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

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#queryUsers(ch.eitchnet.privilege.model.UserRep)
	 */
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
	 * @param selPropertyMap
	 * @param properties
	 * @return
	 */
	private boolean isSelectedByProperty(Map<String, String> selPropertyMap, Map<String, String> properties) {

		if (selPropertyMap == null)
			return true;

		if (selPropertyMap.isEmpty() && properties.isEmpty())
			return true;

		for (String selKey : selPropertyMap.keySet()) {

			String value = properties.get(selKey);
			if (value == null || !value.equals(selPropertyMap.get(selKey)))
				return false;
		}

		return true;
	}

	/**
	 * @param selRoles
	 * @param roles
	 * @return
	 */
	private boolean isSelectedByRole(Set<String> selRoles, Set<String> roles) {

		if (selRoles == null)
			return true;

		return roles.containsAll(selRoles);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#addOrReplaceRole(ch.eitchnet.privilege.model.Certificate,
	 *      ch.eitchnet.privilege.model.RoleRep)
	 */
	@Override
	public void addOrReplaceRole(Certificate certificate, RoleRep roleRep) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// create new role from RoleRep
		Role role = new Role(roleRep);

		// validate policy if not null
		validatePolicies(role);

		// delegate to persistence handler
		this.persistenceHandler.addOrReplaceRole(role);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#addOrReplaceUser(ch.eitchnet.privilege.model.Certificate,
	 *      ch.eitchnet.privilege.model.UserRep, java.lang.String)
	 */
	@Override
	public void addOrReplaceUser(Certificate certificate, UserRep userRep, byte[] password) {
		try {

			// validate who is doing this
			validateIsPrivilegeAdmin(certificate);

			String passwordHash = null;
			if (password != null) {

				// validate password meets basic requirements
				validatePassword(password);

				// hash password
				passwordHash = this.encryptionHandler.convertToHash(password);
			}

			// create new user
			// XXX should the collections be recreated and the getRoles() and getProperties() methods be removed?
			User user = new User(userRep.getUserId(), userRep.getUsername(), passwordHash, userRep.getFirstname(),
					userRep.getSurname(), userRep.getUserState(), userRep.getRoles(), userRep.getLocale(),
					userRep.getProperties());

			// delegate to persistence handler
			this.persistenceHandler.addOrReplaceUser(user);

		} finally {
			clearPassword(password);
		}
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#addOrReplacePrivilegeOnRole(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, ch.eitchnet.privilege.model.PrivilegeRep)
	 */
	@Override
	public void addOrReplacePrivilegeOnRole(Certificate certificate, String roleName, PrivilegeRep privilegeRep) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

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
		Privilege newPrivilege = new Privilege(privilegeRep);
		Map<String, Privilege> privilegeMap = new HashMap<String, Privilege>(role.getPrivilegeMap());
		privilegeMap.put(newPrivilege.getName(), newPrivilege);

		Role newRole = new Role(role.getName(), privilegeMap);

		// delegate role replacement to persistence handler
		this.persistenceHandler.addOrReplaceRole(newRole);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#addRoleToUser(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void addRoleToUser(Certificate certificate, String username, String roleName) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// get user
		User user = this.persistenceHandler.getUser(username);
		if (user == null) {
			throw new PrivilegeException("User " + username + " does not exist!");
		}

		// ignore if user already has role
		Set<String> currentRoles = user.getRoles();
		if (currentRoles.contains(roleName)) {
			logger.error("User " + username + " already has role " + roleName);
			return;
		}

		// validate that role exists
		if (getRole(roleName) == null) {
			throw new PrivilegeException("Role " + roleName + " doest not exist!");
		}

		// create new user
		Set<String> newRoles = new HashSet<String>(currentRoles);
		newRoles.add(roleName);

		User newUser = new User(user.getUserId(), user.getUsername(), user.getPassword(), user.getFirstname(),
				user.getSurname(), user.getUserState(), newRoles, user.getLocale(), user.getProperties());

		// delegate user replacement to persistence handler
		this.persistenceHandler.addOrReplaceUser(newUser);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#removePrivilegeFromRole(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void removePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// get role
		Role role = this.persistenceHandler.getRole(roleName);
		if (role == null) {
			throw new PrivilegeException("Role " + roleName + " does not exist!");
		}

		// ignore if role does not have privilege
		if (!role.hasPrivilege(privilegeName))
			throw new PrivilegeException("Role " + roleName + " does not have Privilege " + privilegeName);

		// create new set of privileges with out the to remove privilege
		Map<String, Privilege> newPrivileges = new HashMap<String, Privilege>(role.getPrivilegeMap().size() - 1);
		for (Privilege privilege : role.getPrivilegeMap().values()) {
			if (!privilege.getName().equals(privilegeName))
				newPrivileges.put(privilege.getName(), privilege);
		}

		// create new role
		Role newRole = new Role(role.getName(), newPrivileges);

		// delegate user replacement to persistence handler
		this.persistenceHandler.addOrReplaceRole(newRole);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#removeRole(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String)
	 */
	@Override
	public RoleRep removeRole(Certificate certificate, String roleName) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// delegate role removal to persistence handler
		Role removedRole = this.persistenceHandler.removeRole(roleName);

		if (removedRole == null)
			return null;

		// return role rep if it was removed	
		return removedRole.asRoleRep();
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#removeRoleFromUser(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void removeRoleFromUser(Certificate certificate, String username, String roleName) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// get User
		User user = this.persistenceHandler.getUser(username);
		if (user == null) {
			throw new PrivilegeException("User " + username + " does not exist!");
		}

		// ignore if user does not have role
		Set<String> currentRoles = user.getRoles();
		if (!currentRoles.contains(roleName)) {
			logger.error("User " + user + " does not have role " + roleName);
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

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#removeUser(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String)
	 */
	@Override
	public UserRep removeUser(Certificate certificate, String username) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// delegate user removal to persistence handler
		User removedUser = this.persistenceHandler.removeUser(username);

		// return user rep if it was removed
		if (removedUser == null)
			return null;

		// return user rep if it was removed
		return removedUser.asUserRep();

	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#setUserLocale(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.util.Locale)
	 */
	@Override
	public void setUserLocale(Certificate certificate, String username, Locale locale) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

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

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#setUserName(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public void setUserName(Certificate certificate, String username, String firstname, String surname) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

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
	 *      java.lang.String, java.lang.String)
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
				validateIsPrivilegeAdmin(certificate);
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

		} finally {
			clearPassword(password);
		}
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#setUserState(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, ch.eitchnet.privilege.model.UserState)
	 */
	@Override
	public void setUserState(Certificate certificate, String username, UserState state) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

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
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#authenticate(java.lang.String, java.lang.String)
	 * 
	 * @throws AccessDeniedException
	 *             if the user credentials are not valid
	 */
	@Override
	public Certificate authenticate(String username, byte[] password) {

		// create certificate
		Certificate certificate;
		try {
			// username must be at least 3 characters in length
			if (username == null || username.length() < 3)
				throw new PrivilegeException("The given username is shorter than 3 characters");

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
				throw new AccessDeniedException("User has no password and may not login!");
			if (!pwHash.equals(passwordHash))
				throw new AccessDeniedException("Password is incorrect for " + username);

			// validate if user is allowed to login
			// this also capture the trying to login of SYSTEM user
			if (user.getUserState() != UserState.ENABLED)
				throw new AccessDeniedException("User " + username + " does not have state " + UserState.ENABLED
						+ " and can not login!");

			// validate user has at least one role
			if (user.getRoles().isEmpty()) {
				throw new PrivilegeException("User " + username + " does not have any roles defined!");
			}

			// get 2 auth tokens
			String authToken = this.encryptionHandler.nextToken();
			String authPassword = this.encryptionHandler.nextToken();

			// get next session id
			String sessionId = nextSessionId();

			// create a new certificate, with details of the user
			certificate = new Certificate(sessionId, username, authToken, authPassword, user.getLocale(),
					new HashMap<String, String>(user.getProperties()));

			// create and save a new session
			Session session = new Session(sessionId, username, authToken, authPassword, System.currentTimeMillis());
			this.sessionMap.put(sessionId, new CertificateSessionPair(session, certificate));

			// log
			logger.info("User " + username + " authenticated: " + session);

		} catch (RuntimeException e) {
			logger.error("User " + username + " Failed to authenticate: " + e.getLocalizedMessage());
			throw e;
		} finally {
			clearPassword(password);
		}

		// return the certificate
		return certificate;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#invalidateSession(ch.eitchnet.privilege.model.Certificate)
	 */
	@Override
	public boolean invalidateSession(Certificate certificate) {

		// first validate certificate
		isCertificateValid(certificate);

		// remove registration
		CertificateSessionPair certificateSessionPair = this.sessionMap.remove(certificate.getSessionId());

		// return true if object was really removed
		boolean loggedOut = certificateSessionPair != null;
		if (loggedOut)
			logger.info("User " + certificate.getUsername() + " logged out.");
		else
			logger.warn("User already logged out!");
		return loggedOut;
	}

	/**
	 * Checks if the action is allowed by iterating the roles of the certificates user and then delegating to
	 * {@link #actionAllowed(Role, Restrictable)}
	 * 
	 * @throws AccessDeniedException
	 *             if the {@link Certificate} is not for a currently logged in {@link User} or if the user may not
	 *             perform the action defined by the {@link Restrictable} implementation
	 * 
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#actionAllowed(ch.eitchnet.privilege.model.Certificate,
	 *      ch.eitchnet.privilege.model.Restrictable)
	 */
	@Override
	public void actionAllowed(Certificate certificate, Restrictable restrictable) {

		// TODO What is better, validate from {@link Restrictable} to {@link User} or the opposite direction?

		// first validate certificate
		isCertificateValid(certificate);

		// restrictable must not be null
		if (restrictable == null)
			throw new PrivilegeException("Restrictable may not be null!");

		// get user object
		User user = this.persistenceHandler.getUser(certificate.getUsername());
		if (user == null) {
			throw new PrivilegeException(
					"Oh boy, how did this happen: No User in user map although the certificate is valid!");
		}

		String privilegeName = restrictable.getPrivilegeName();

		// now iterate roles and validate on policies
		for (String roleName : user.getRoles()) {

			Role role = this.persistenceHandler.getRole(roleName);
			if (role == null) {
				logger.error("No role is defined with name " + roleName + " which is configured for user " + user);
				continue;
			}

			// ignore if this role does not have the privilege
			if (!role.hasPrivilege(privilegeName))
				continue;

			// if action is allowed, then break iteration as a privilege match has been made
			if (actionAllowed(role, restrictable))
				return;
		}

		throw new AccessDeniedException("User " + user.getUsername() + " does not have Privilege " + privilegeName
				+ " needed for Restrictable " + restrictable.getClass().getName());
	}

	/**
	 * Checks if the {@link RoleRep} has access to the {@link Restrictable} by delegating to
	 * {@link PrivilegePolicy#actionAllowed(Role, Privilege, Restrictable)}
	 * 
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#actionAllowed(ch.eitchnet.privilege.model.RoleRep,
	 *      ch.eitchnet.privilege.model.Restrictable)
	 */
	@Override
	public void actionAllowed(RoleRep roleRep, Restrictable restrictable) {

		// user and restrictable must not be null
		if (roleRep == null)
			throw new PrivilegeException("Role may not be null!");
		else if (restrictable == null)
			throw new PrivilegeException("Restrictable may not be null!");

		// get role for the roleRep
		Role role = this.persistenceHandler.getRole(roleRep.getName());

		// validate that the role exists
		if (role == null) {
			throw new PrivilegeException("No Role exists with the name " + roleRep.getName());
		}

		// delegate to method with Role
		actionAllowed(role, restrictable);
	}

	/**
	 * Checks if the {@link Role} has access to the {@link Restrictable} by delegating to
	 * {@link PrivilegePolicy#actionAllowed(Role, Privilege, Restrictable)}
	 * 
	 * @param role
	 * @param restrictable
	 */
	private boolean actionAllowed(Role role, Restrictable restrictable) {

		// validate PrivilegeName for this restrictable
		String privilegeName = restrictable.getPrivilegeName();
		if (privilegeName == null || privilegeName.length() < 3) {
			throw new PrivilegeException(
					"The PrivilegeName may not be shorter than 3 characters. Invalid Restrictable "
							+ restrictable.getClass().getName());
		}

		// If the role does not have this privilege, then stop as another role might have this privilege
		if (!role.hasPrivilege(privilegeName)) {
			return false;
		}

		// get the privilege for this restrictable
		Privilege privilege = role.getPrivilegeMap().get(privilegeName);

		// check if all is allowed
		if (privilege.isAllAllowed()) {
			return true;
		}

		// otherwise delegate checking to the policy configured for this privilege
		PrivilegePolicy policy = this.getPolicy(privilege.getPolicy());
		if (policy == null) {
			throw new PrivilegeException("PrivilegePolicy " + privilege.getPolicy() + " does not exist for Privilege "
					+ privilegeName);
		}

		// delegate checking to privilege policy
		policy.actionAllowed(role, privilege, restrictable);

		// the policy must throw an exception if action not allowed,
		// so return true if we arrive here
		return true;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#isCertificateValid(ch.eitchnet.privilege.model.Certificate)
	 */
	@Override
	public void isCertificateValid(Certificate certificate) {

		// certificate  must not be null
		if (certificate == null)
			throw new PrivilegeException("Certificate may not be null!");

		// first see if a session exists for this certificate
		CertificateSessionPair certificateSessionPair = this.sessionMap.get(certificate.getSessionId());
		if (certificateSessionPair == null)
			throw new AccessDeniedException("There is no session information for " + certificate.toString());

		// validate certificate has not been tampered with
		Certificate sessionCertificate = certificateSessionPair.certificate;
		if (!sessionCertificate.equals(certificate))
			throw new PrivilegeException("Received illegal certificate for session id " + certificate.getSessionId());

		// TODO is validating authToken overkill since the two certificates have already been checked on equality?
		// validate authToken from certificate using the sessions authPassword
		String authToken = certificate.getAuthToken(certificateSessionPair.session.getAuthPassword());
		if (authToken == null || !authToken.equals(certificateSessionPair.session.getAuthToken()))
			throw new PrivilegeException("Received illegal certificate data for session id "
					+ certificate.getSessionId());

		// get user object
		User user = this.persistenceHandler.getUser(certificateSessionPair.session.getUsername());

		// if user exists, then certificate is valid
		if (user == null) {
			throw new PrivilegeException(
					"Oh boy, how did this happen: No User in user map although the certificate is valid!");
		}

		// everything is ok
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#validateIsPrivilegeAdmin(ch.eitchnet.privilege.model.Certificate)
	 */
	@Override
	public void validateIsPrivilegeAdmin(Certificate certificate) throws PrivilegeException {

		// validate certificate
		this.isCertificateValid(certificate);

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
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#validatePassword(java.lang.String)
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

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#persist(ch.eitchnet.privilege.model.Certificate)
	 */
	@Override
	public boolean persist(Certificate certificate) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

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

		// validate policies on privileges of Roles
		for (Role role : persistenceHandler.getAllRoles()) {
			validatePolicies(role);
		}

		this.lastSessionId = 0l;
		this.sessionMap = Collections.synchronizedMap(new HashMap<String, CertificateSessionPair>());
		this.systemUserCertificateMap = Collections.synchronizedMap(new HashMap<String, Certificate>());
		this.initialized = true;
	}

	/**
	 * Validates that the policies which are not null on the privileges of the role exist
	 * 
	 * @param role
	 *            the role for which the policies are to be checked
	 */
	private void validatePolicies(Role role) {
		for (Privilege privilege : role.getPrivilegeMap().values()) {
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
	 * An internal class used to keep a record of sessions with the certificate
	 * 
	 * @author Robert von Burg <eitch@eitchnet.ch>
	 */
	protected class CertificateSessionPair {
		/**
		 * The {@link Session}
		 */
		public final Session session;
		/**
		 * The {@link Certificate}
		 */
		public final Certificate certificate;

		/**
		 * Creates a new {@link CertificateSessionPair} with the given session and certificate
		 * 
		 * @param session
		 *            the session
		 * @param certificate
		 *            the certificate
		 */
		public CertificateSessionPair(Session session, Certificate certificate) {
			this.session = session;
			this.certificate = certificate;
		}
	}

	/**
	 * @param password
	 */
	private void clearPassword(byte[] password) {
		if (password != null) {
			for (int i = 0; i < password.length; i++) {
				password[i] = 0;
			}
		}
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#runAsSystem(java.lang.String,
	 *      ch.eitchnet.privilege.handler.SystemUserAction)
	 */
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
		Certificate systemUserCertificate = getSystemUserCertificate(systemUsername);

		// perform the action
		action.execute(systemUserCertificate);
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
	private Certificate getSystemUserCertificate(String systemUsername) {

		// see if a certificate has already been created for this system user
		Certificate systemUserCertificate = systemUserCertificateMap.get(systemUsername);
		if (systemUserCertificate != null)
			return systemUserCertificate;

		// otherwise log this system user in, by performing a slightly different authentication

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
		systemUserCertificate = new Certificate(sessionId, systemUsername, authToken, authPassword, user.getLocale(),
				new HashMap<String, String>(user.getProperties()));

		// create and save a new session
		Session session = new Session(sessionId, systemUsername, authToken, authPassword, System.currentTimeMillis());
		this.sessionMap.put(sessionId, new CertificateSessionPair(session, systemUserCertificate));

		// log
		logger.info("The system user " + systemUsername + " is logged in with session " + session);

		return systemUserCertificate;
	}
}
