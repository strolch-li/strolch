/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.handler;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
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
 * @author rvonburg
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
	protected long lastSessionId;

	/**
	 * Map of {@link PrivilegePolicy} classes
	 */
	private Map<String, Class<PrivilegePolicy>> policyMap;

	/**
	 * Map keeping a reference to all active sessions with their certificates
	 */
	protected Map<String, CertificateSessionPair> sessionMap;

	/**
	 * The persistence handler is used for getting objects and saving changes
	 */
	protected PersistenceHandler persistenceHandler;

	/**
	 * The encryption handler is used for generating hashes and tokens
	 */
	protected EncryptionHandler encryptionHandler;

	/**
	 * flag to define if already initialized
	 */
	protected boolean initialized;

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#getRole(java.lang.String)
	 */
	@Override
	public RoleRep getRole(String roleName) {
		Role role = this.persistenceHandler.getRole(roleName);
		if (role == null)
			throw new PrivilegeException("Role " + roleName + " does not exist!");
		return role.asRoleRep();
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#getUser(java.lang.String)
	 */
	@Override
	public UserRep getUser(String username) {
		User user = this.persistenceHandler.getUser(username);
		if (user == null)
			throw new PrivilegeException("User " + username + " does not exist!");
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
	protected PrivilegePolicy getPolicy(String policyName) {

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
	public void addOrReplaceUser(Certificate certificate, UserRep userRep, String password) {

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
		User user = new User(userRep.getUserId(), userRep.getUsername(), passwordHash, userRep.getFirstname(),
				userRep.getSurname(), userRep.getUserState(), userRep.getRoles(), userRep.getLocale());

		// delegate to persistence handler
		this.persistenceHandler.addOrReplaceUser(user);
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
				user.getSurname(), user.getUserState(), newRoles, user.getLocale());

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
				user.getSurname(), user.getUserState(), newRoles, user.getLocale());

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
				user.getSurname(), user.getUserState(), user.getRoles(), locale);

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
				user.getUserState(), user.getRoles(), user.getLocale());

		// delegate user replacement to persistence handler
		this.persistenceHandler.addOrReplaceUser(newUser);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#setUserPassword(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void setUserPassword(Certificate certificate, String username, String password) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

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
				user.getSurname(), user.getUserState(), user.getRoles(), user.getLocale());

		// delegate user replacement to persistence handler
		this.persistenceHandler.addOrReplaceUser(newUser);
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
				user.getSurname(), state, user.getRoles(), user.getLocale());

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
	public Certificate authenticate(String username, String password) {

		// create certificate
		Certificate certificate;
		try {
			// both username and password must at least have 3 characters in length
			if (username == null || username.length() < 3)
				throw new PrivilegeException("The given username is shorter than 3 characters");
			else if (password == null || password.length() < 3)
				throw new PrivilegeException("The given password is shorter than 3 characters");

			// we only work with hashed passwords
			String passwordHash = this.encryptionHandler.convertToHash(password);

			// get user object
			User user = this.persistenceHandler.getUser(username);
			// no user means no authentication
			if (user == null)
				throw new AccessDeniedException("There is no user defined with the credentials: " + username
						+ " / ***...");

			// validate password
			String pwHash = user.getPassword();
			if (pwHash == null)
				throw new AccessDeniedException("User has no password and may not login!");
			if (!pwHash.equals(passwordHash))
				throw new AccessDeniedException("Password is incorrect for " + username + " / ***...");

			// validate if user is allowed to login
			if (user.getUserState() != UserState.ENABLED)
				throw new AccessDeniedException("User " + username + " is not ENABLED. State is: "
						+ user.getUserState());

			// validate user has at least one role
			if (user.getRoles().isEmpty()) {
				throw new PrivilegeException("User " + username + " does not have any roles defined!");
			}

			// get 2 auth tokens
			String authToken = this.encryptionHandler.nextToken();
			String authPassword = this.encryptionHandler.nextToken();

			// get next session id
			String sessionId = nextSessionId();

			certificate = new Certificate(sessionId, username, authToken, authPassword, user.getLocale());

			// create and save a new session
			Session session = new Session(sessionId, username, authToken, authPassword, System.currentTimeMillis());
			this.sessionMap.put(sessionId, new CertificateSessionPair(session, certificate));

			// log
			logger.info("User " + username + " authenticated: " + session);

		} catch (RuntimeException e) {
			logger.error("User " + username + " Failed to authenticate: " + e.getLocalizedMessage());
			throw e;
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
		if (!isCertificateValid(certificate)) {
			logger.info("Certificate is not valid, so no session to invalidate: " + certificate.toString());
			return false;
		}

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
	public boolean actionAllowed(Certificate certificate, Restrictable restrictable) {

		// TODO What is better, validate from {@link Restrictable} to {@link User} or the opposite direction?

		// first validate certificate
		if (!isCertificateValid(certificate)) {
			throw new PrivilegeException("Certificate is not valid, so action is not allowed: " + certificate
					+ " for restrictable: " + restrictable);
		}

		// restrictable must not be null
		if (restrictable == null)
			throw new PrivilegeException("Restrictable may not be null!");

		// get user object
		User user = this.persistenceHandler.getUser(certificate.getUsername());
		if (user == null) {
			throw new PrivilegeException(
					"Oh boy, how did this happen: No User in user map although the certificate is valid!");
		}

		// default is to not allow the action
		// TODO should default deny/allow policy be configurable?
		boolean actionAllowed = false;

		// now iterate roles and validate on policies
		for (String roleName : user.getRoles()) {

			Role role = this.persistenceHandler.getRole(roleName);
			if (role == null) {
				logger.error("No role is defined with name " + roleName + " which is configured for user " + user);
				continue;
			}

			actionAllowed = actionAllowed(role, restrictable);

			// if action is allowed, then break iteration as a privilege match has been made
			if (actionAllowed)
				break;
		}

		return actionAllowed;
	}

	/**
	 * Checks if the {@link RoleRep} has access to the {@link Restrictable} by delegating to
	 * {@link PrivilegePolicy#actionAllowed(Role, Privilege, Restrictable)}
	 * 
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#actionAllowed(ch.eitchnet.privilege.model.RoleRep,
	 *      ch.eitchnet.privilege.model.Restrictable)
	 */
	@Override
	public boolean actionAllowed(RoleRep roleRep, Restrictable restrictable) {

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

		return actionAllowed(role, restrictable);
	}

	/**
	 * Checks if the {@link Role} has access to the {@link Restrictable} by delegating to
	 * {@link PrivilegePolicy#actionAllowed(Role, Privilege, Restrictable)}
	 * 
	 * @param role
	 * @param restrictable
	 * 
	 * @return true if the privilege is granted, false otherwise
	 */
	protected boolean actionAllowed(Role role, Restrictable restrictable) {

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
		if (privilege.isAllAllowed())
			return true;

		// otherwise delegate checking to the policy configured for this privilege
		PrivilegePolicy policy = this.getPolicy(privilege.getPolicy());
		if (policy == null) {
			throw new PrivilegeException("PrivilegePolicy " + privilege.getPolicy() + " does not exist for Privilege "
					+ privilegeName);
		}

		// delegate checking to privilege policy
		return policy.actionAllowed(role, privilege, restrictable);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#isCertificateValid(ch.eitchnet.privilege.model.Certificate)
	 */
	@Override
	public boolean isCertificateValid(Certificate certificate) {

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

		// everything is ok, so return true as the certificate must be valid
		return true;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#validateIsPrivilegeAdmin(ch.eitchnet.privilege.model.Certificate)
	 */
	@Override
	public void validateIsPrivilegeAdmin(Certificate certificate) throws PrivilegeException {

		// validate certificate
		if (!this.isCertificateValid(certificate)) {
			throw new PrivilegeException("Certificate " + certificate + " is not valid!");
		}

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
	public void validatePassword(String password) throws PrivilegeException {

		if (password == null || password.isEmpty()) {
			throw new PrivilegeException("A password may not be empty!");
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
	public void initialize(Map<String, String> parameterMap, EncryptionHandler encryptionHandler,
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
	 * @author rvonburg
	 */
	private class CertificateSessionPair {
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

}
