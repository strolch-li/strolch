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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;

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
 * @author rvonburg
 * 
 */
public class DefaultPrivilegeHandler implements PrivilegeHandler {

	/**
	 * log4j logger
	 */
	private static final Logger logger = Logger.getLogger(DefaultPrivilegeHandler.class);

	/**
	 * last assigned id for the {@link Session}s
	 */
	private static long lastSessionId;

	/**
	 * Map keeping a reference to all active sessions with their certificates
	 */
	private Map<String, CertificateSessionPair> sessionMap;

	/**
	 * The persistence handler is used for getting objects and saving changes
	 */
	private PersistenceHandler persistenceHandler;

	/**
	 * The encryption handler is used for generating hashes and tokens
	 */
	private EncryptionHandler encryptionHandler;

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#getPrivilege(java.lang.String)
	 */
	@Override
	public PrivilegeRep getPrivilege(String privilegeName) {
		return this.persistenceHandler.getPrivilege(privilegeName).asPrivilegeRep();
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#getRole(java.lang.String)
	 */
	@Override
	public RoleRep getRole(String roleName) {
		return this.persistenceHandler.getRole(roleName).asRoleRep();
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#getUser(java.lang.String)
	 */
	@Override
	public UserRep getUser(String username) {
		return this.persistenceHandler.getUser(username).asUserRep();
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#getPolicy(java.lang.String)
	 */
	@Override
	public PrivilegePolicy getPolicy(String policyName) {
		return this.persistenceHandler.getPolicy(policyName);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#addOrReplacePrivilege(ch.eitchnet.privilege.model.Certificate,
	 *      ch.eitchnet.privilege.model.PrivilegeRep)
	 */
	@Override
	public void addOrReplacePrivilege(Certificate certificate, PrivilegeRep privilegeRep) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// create a new privilege
		Privilege privilege = new Privilege(privilegeRep.getName(), privilegeRep.getPolicy(), privilegeRep
				.isAllAllowed(), privilegeRep.getDenyList(), privilegeRep.getAllowList());

		// delegate to persistence handler
		this.persistenceHandler.addOrReplacePrivilege(privilege);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#addOrReplaceRole(ch.eitchnet.privilege.model.Certificate,
	 *      ch.eitchnet.privilege.model.RoleRep)
	 */
	@Override
	public void addOrReplaceRole(Certificate certificate, RoleRep roleRep) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// create new role
		Role role = new Role(roleRep.getName(), roleRep.getPrivileges());

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
		User user = new User(userRep.getUsername(), passwordHash, userRep.getFirstname(), userRep.getSurname(), userRep
				.getUserState(), userRep.getRoles(), userRep.getLocale());

		// delegate to persistence handler
		this.persistenceHandler.addOrReplaceUser(user);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#addPrivilegeToRole(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void addPrivilegeToRole(Certificate certificate, String roleName, String privilegeName) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// get role
		Role role = this.persistenceHandler.getRole(roleName);
		if (role == null) {
			throw new PrivilegeException("Role " + roleName + " does not exist!");
		}

		// ignore if role already has this privilege
		Set<String> currentPrivileges = role.getPrivileges();
		if (currentPrivileges.contains(roleName)) {
			logger.error("Role " + roleName + " already has privilege " + privilegeName);
			return;
		}

		// validate that privilege exists
		if (getPrivilege(privilegeName) == null) {
			throw new PrivilegeException("Privilege " + privilegeName + " does not exist and can not be added to role "
					+ roleName);
		}

		// create new role with the additional privilege
		Set<String> newPrivileges = new HashSet<String>(currentPrivileges);
		newPrivileges.add(roleName);

		Role newRole = new Role(role.getName(), newPrivileges);

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

		User newUser = new User(user.getUsername(), user.getPassword(), user.getFirstname(), user.getSurname(), user
				.getState(), newRoles, user.getLocale());

		// delegate user replacement to persistence handler
		this.persistenceHandler.addOrReplaceUser(newUser);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#removePrivilege(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String)
	 */
	@Override
	public PrivilegeRep removePrivilege(Certificate certificate, String privilegeName) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// delegate privilege removal to persistence handler
		Privilege removedPrivilege = this.persistenceHandler.removePrivilege(privilegeName);

		if (removedPrivilege == null)
			return null;

		// return privilege rep if it was removed
		return removedPrivilege.asPrivilegeRep();
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
		Set<String> currentPrivileges = role.getPrivileges();
		if (!currentPrivileges.contains(privilegeName)) {
			logger.error("Role " + roleName + " doest not have privilege " + privilegeName);
			return;
		}

		// create new role
		Set<String> newPrivileges = new HashSet<String>(currentPrivileges);
		newPrivileges.remove(privilegeName);
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
		User newUser = new User(user.getUsername(), user.getPassword(), user.getFirstname(), user.getSurname(), user
				.getState(), newRoles, user.getLocale());

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
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#setPrivilegeAllAllowed(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, boolean)
	 */
	@Override
	public void setPrivilegeAllAllowed(Certificate certificate, String privilegeName, boolean allAllowed) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// get Privilege
		Privilege privilege = this.persistenceHandler.getPrivilege(privilegeName);
		if (privilege == null) {
			throw new PrivilegeException("Privilege " + privilegeName + " does not exist!");
		}

		// ignore if privilege is already set to argument
		if (privilege.isAllAllowed() == allAllowed) {
			logger.error("Privilege " + privilegeName + " is already set to "
					+ (allAllowed ? "all allowed" : "not all allowed"));
			return;
		}

		// create new privilege
		Privilege newPrivilege = new Privilege(privilege.getName(), privilege.getPolicy(), allAllowed, privilege
				.getDenyList(), privilege.getAllowList());

		// delegate privilege replacement to persistence handler
		this.persistenceHandler.addOrReplacePrivilege(newPrivilege);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#setPrivilegeAllowList(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.util.Set)
	 */
	@Override
	public void setPrivilegeAllowList(Certificate certificate, String privilegeName, Set<String> allowList) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// get Privilege
		Privilege privilege = this.persistenceHandler.getPrivilege(privilegeName);
		if (privilege == null) {
			throw new PrivilegeException("Privilege " + privilegeName + " does not exist!");
		}

		// create new privilege
		Privilege newPrivilege = new Privilege(privilege.getName(), privilege.getPolicy(), privilege.isAllAllowed(),
				privilege.getDenyList(), allowList);

		// delegate privilege replacement to persistence handler
		this.persistenceHandler.addOrReplacePrivilege(newPrivilege);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#setPrivilegeDenyList(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.util.Set)
	 */
	@Override
	public void setPrivilegeDenyList(Certificate certificate, String privilegeName, Set<String> denyList) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// get Privilege
		Privilege privilege = this.persistenceHandler.getPrivilege(privilegeName);
		if (privilege == null) {
			throw new PrivilegeException("Privilege " + privilegeName + " does not exist!");
		}

		// create new privilege
		Privilege newPrivilege = new Privilege(privilege.getName(), privilege.getPolicy(), privilege.isAllAllowed(),
				denyList, privilege.getAllowList());

		// delegate privilege replacement to persistence handler
		this.persistenceHandler.addOrReplacePrivilege(newPrivilege);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#setPrivilegePolicy(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void setPrivilegePolicy(Certificate certificate, String privilegeName, String policyName) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// get Privilege
		Privilege privilege = this.persistenceHandler.getPrivilege(privilegeName);
		if (privilege == null) {
			throw new PrivilegeException("Privilege " + privilegeName + " does not exist!");
		}

		// create new privilege
		Privilege newPrivilege = new Privilege(privilege.getName(), policyName, privilege.isAllAllowed(), privilege
				.getDenyList(), privilege.getAllowList());

		// delegate privilege replacement to persistence handler
		this.persistenceHandler.addOrReplacePrivilege(newPrivilege);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#setUserLocaleState(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.util.Locale)
	 */
	@Override
	public void setUserLocaleState(Certificate certificate, String username, Locale locale) {

		// validate who is doing this
		validateIsPrivilegeAdmin(certificate);

		// get User
		User user = this.persistenceHandler.getUser(username);
		if (user == null) {
			throw new PrivilegeException("User " + username + " does not exist!");
		}

		// create new user
		User newUser = new User(user.getUsername(), user.getPassword(), user.getFirstname(), user.getSurname(), user
				.getState(), user.getRoles(), locale);

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
		User newUser = new User(user.getUsername(), user.getPassword(), firstname, surname, user.getState(), user
				.getRoles(), user.getLocale());

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
		User newUser = new User(user.getUsername(), passwordHash, user.getFirstname(), user.getSurname(), user
				.getState(), user.getRoles(), user.getLocale());

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
		User newUser = new User(user.getUsername(), user.getPassword(), user.getFirstname(), user.getSurname(), state,
				user.getRoles(), user.getLocale());

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
			throw new AccessDeniedException("There is no user defined with the credentials: " + username + " / ***...");

		// validate password
		String pwHash = user.getPassword();
		if (pwHash == null)
			throw new AccessDeniedException("User has no password and may not login!");
		if (!pwHash.equals(passwordHash))
			throw new AccessDeniedException("Password is incorrect for " + username + " / ***...");

		// validate if user is allowed to login
		if (user.getState() != UserState.ENABLED)
			throw new AccessDeniedException("User " + username + " is not ENABLED. State is: " + user.getState());

		// validate user has at least one role
		if (user.getRoles().isEmpty()) {
			throw new PrivilegeException("User " + username + " does not have any roles defined!");
		}

		// get 2 auth tokens
		String authToken = this.encryptionHandler.nextToken();
		String authPassword = this.encryptionHandler.nextToken();

		// get next session id
		String sessionId = nextSessionId();

		// create certificate
		Certificate certificate = new Certificate(sessionId, username, authToken, authPassword, user.getLocale());

		// create and save a new session
		Session session = new Session(sessionId, authToken, authPassword, user.getUsername(), System
				.currentTimeMillis());
		this.sessionMap.put(sessionId, new CertificateSessionPair(session, certificate));

		// log
		logger.info("Authenticated: " + session);

		// return the certificate
		return certificate;
	}

	/**
	 * TODO What is better, validate from {@link Restrictable} to {@link User} or the opposite direction?
	 * 
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

		// first validate certificate
		if (!isCertificateValid(certificate)) {
			logger.info("Certificate is not valid, so action is not allowed: " + certificate + " for restrictable: "
					+ restrictable);
			return false;
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
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#actionAllowed(ch.eitchnet.privilege.model.internal.Role,
	 *      ch.eitchnet.privilege.model.Restrictable)
	 */
	@Override
	public boolean actionAllowed(Role role, Restrictable restrictable) {

		// user and restrictable must not be null
		if (role == null)
			throw new PrivilegeException("Role may not be null!");
		else if (restrictable == null)
			throw new PrivilegeException("Restrictable may not be null!");

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
		Privilege privilege = this.persistenceHandler.getPrivilege(privilegeName);
		if (privilege == null) {
			throw new PrivilegeException("No Privilege exists with the name " + privilegeName + " for Restrictable "
					+ restrictable.getClass().getName());
		}

		// get the policy configured for this privilege
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

		return this.persistenceHandler.persist(certificate);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#initialize(java.util.Map,
	 *      ch.eitchnet.privilege.handler.EncryptionHandler, ch.eitchnet.privilege.handler.PersistenceHandler)
	 */
	@Override
	public void initialize(Map<String, String> parameterMap, EncryptionHandler encryptionHandler,
			PersistenceHandler persistenceHandler) {

		this.encryptionHandler = encryptionHandler;
		this.persistenceHandler = persistenceHandler;

		lastSessionId = 0l;
		this.sessionMap = new HashMap<String, CertificateSessionPair>();
	}

	/**
	 * @return a new session id
	 */
	private synchronized String nextSessionId() {
		return Long.toString(++lastSessionId % Long.MAX_VALUE);
	}

	/**
	 * An internal class used to keep a record of sessions with the certificate
	 * 
	 * @author rvonburg
	 */
	private class CertificateSessionPair {
		public final Session session;
		public final Certificate certificate;

		/**
		 * @param session
		 * @param certificate
		 */
		public CertificateSessionPair(Session session, Certificate certificate) {
			this.session = session;
			this.certificate = certificate;
		}
	}

}
