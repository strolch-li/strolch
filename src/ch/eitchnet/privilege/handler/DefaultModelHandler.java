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

import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import org.apache.log4j.Logger;
import org.dom4j.Element;

import ch.eitchnet.privilege.base.PrivilegeContainer;
import ch.eitchnet.privilege.helper.PrivilegeHelper;
import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.RoleRep;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;

/**
 * @author rvonburg
 * 
 */
public class DefaultModelHandler implements ModelHandler {

	private static final Logger logger = Logger.getLogger(DefaultModelHandler.class);

	private PersistenceHandler persistenceHandler;

	/**
	 * @see ch.eitchnet.privilege.handler.SessionHandler#setPersistenceHandler(ch.eitchnet.privilege.handler.PersistenceHandler)
	 */
	public void setPersistenceHandler(PersistenceHandler persistenceHandler) {
		this.persistenceHandler = persistenceHandler;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#addOrReplacePrivilege(ch.eitchnet.privilege.model.Certificate,
	 *      ch.eitchnet.privilege.model.PrivilegeRep)
	 */
	@Override
	public void addOrReplacePrivilege(Certificate certificate, PrivilegeRep privilegeRep) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// create a new privilege
		Privilege privilege = new Privilege(privilegeRep.getName(), privilegeRep.getPolicy(), privilegeRep
				.isAllAllowed(), privilegeRep.getDenyList(), privilegeRep.getAllowList());

		// delegate to persistence handler
		persistenceHandler.addOrReplacePrivilege(privilege);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#addOrReplaceRole(ch.eitchnet.privilege.model.Certificate,
	 *      ch.eitchnet.privilege.model.RoleRep)
	 */
	@Override
	public void addOrReplaceRole(Certificate certificate, RoleRep roleRep) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// create new role
		Role role = new Role(roleRep.getName(), roleRep.getPrivileges());

		// delegate to persistence handler
		persistenceHandler.addOrReplaceRole(role);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#addOrReplaceUser(ch.eitchnet.privilege.model.Certificate,
	 *      ch.eitchnet.privilege.model.UserRep, java.lang.String)
	 */
	@Override
	public void addOrReplaceUser(Certificate certificate, UserRep userRep, String password) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// hash password
		String passwordHash;
		if (password == null)
			passwordHash = null;
		else
			passwordHash = PrivilegeContainer.getInstance().getEncryptionHandler().convertToHash(password);

		// create new user
		User user = new User(userRep.getUsername(), passwordHash, userRep.getFirstname(), userRep.getSurname(), userRep
				.getUserState(), userRep.getRoles(), userRep.getLocale());

		// delegate to persistence handler
		persistenceHandler.addOrReplaceUser(user);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#addPrivilegeToRole(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void addPrivilegeToRole(Certificate certificate, String roleName, String privilegeName) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// get role
		Role role = getRole(roleName);
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
		persistenceHandler.addOrReplaceRole(newRole);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#addRoleToUser(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void addRoleToUser(Certificate certificate, String username, String roleName) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// get user
		User user = getUser(username);
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
		currentRoles.add(roleName);

		User newUser = new User(user.getUsername(), user.getPassword(certificate), user.getFirstname(), user
				.getSurname(), user.getState(), newRoles, user.getLocale());

		// delegate user replacement to persistence handler
		persistenceHandler.addOrReplaceUser(newUser);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#persist(ch.eitchnet.privilege.model.Certificate)
	 */
	@Override
	public boolean persist(Certificate certificate) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return false;
		}

		return persistenceHandler.persist(certificate);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#removePrivilege(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String)
	 */
	@Override
	public PrivilegeRep removePrivilege(Certificate certificate, String privilegeName) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return null;
		}

		// delegate privilege removal to persistence handler
		Privilege removedPrivilege = persistenceHandler.removePrivilege(privilegeName);

		// return privilege rep if it was removed
		if (removedPrivilege != null)
			return removedPrivilege.asPrivilegeRep();
		else
			return null;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#removePrivilegeFromRole(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void removePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// get role
		Role role = getRole(roleName);
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
		persistenceHandler.addOrReplaceRole(newRole);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#removeRole(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String)
	 */
	@Override
	public RoleRep removeRole(Certificate certificate, String roleName) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return null;
		}

		// delegate role removal to persistence handler
		Role removedRole = persistenceHandler.removeRole(roleName);

		// return role rep if it was removed
		if (removedRole != null)
			return removedRole.asRoleRep();
		else
			return null;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#removeRoleFromUser(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void removeRoleFromUser(Certificate certificate, String username, String roleName) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// get User
		User user = getUser(username);
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
		User newUser = new User(user.getUsername(), user.getPassword(certificate), user.getFirstname(), user
				.getSurname(), user.getState(), newRoles, user.getLocale());

		// delegate user replacement to persistence handler
		persistenceHandler.addOrReplaceUser(newUser);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#removeUser(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String)
	 */
	@Override
	public UserRep removeUser(Certificate certificate, String username) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return null;
		}

		// delegate user removal to persistence handler
		User removedUser = persistenceHandler.removeUser(username);

		// return user rep if it was removed
		if (removedUser != null)
			return removedUser.asUserRep();
		else
			return null;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setPrivilegeAllAllowed(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, boolean)
	 */
	@Override
	public void setPrivilegeAllAllowed(Certificate certificate, String privilegeName, boolean allAllowed) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// get Privilege
		Privilege privilege = getPrivilege(privilegeName);
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
		persistenceHandler.addOrReplacePrivilege(newPrivilege);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setPrivilegeAllowList(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.util.Set)
	 */
	@Override
	public void setPrivilegeAllowList(Certificate certificate, String privilegeName, Set<String> allowList) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// get Privilege
		Privilege privilege = getPrivilege(privilegeName);
		if (privilege == null) {
			throw new PrivilegeException("Privilege " + privilegeName + " does not exist!");
		}

		// create new privilege
		Privilege newPrivilege = new Privilege(privilege.getName(), privilege.getPolicy(), privilege.isAllAllowed(),
				privilege.getDenyList(), allowList);

		// delegate privilege replacement to persistence handler
		persistenceHandler.addOrReplacePrivilege(newPrivilege);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setPrivilegeDenyList(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.util.Set)
	 */
	@Override
	public void setPrivilegeDenyList(Certificate certificate, String privilegeName, Set<String> denyList) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// get Privilege
		Privilege privilege = getPrivilege(privilegeName);
		if (privilege == null) {
			throw new PrivilegeException("Privilege " + privilegeName + " does not exist!");
		}

		// create new privilege
		Privilege newPrivilege = new Privilege(privilege.getName(), privilege.getPolicy(), privilege.isAllAllowed(),
				denyList, privilege.getAllowList());

		// delegate privilege replacement to persistence handler
		persistenceHandler.addOrReplacePrivilege(newPrivilege);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setPrivilegePolicy(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void setPrivilegePolicy(Certificate certificate, String privilegeName, String policyName) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// get Privilege
		Privilege privilege = getPrivilege(privilegeName);
		if (privilege == null) {
			throw new PrivilegeException("Privilege " + privilegeName + " does not exist!");
		}

		// create new privilege
		Privilege newPrivilege = new Privilege(privilege.getName(), policyName, privilege.isAllAllowed(), privilege
				.getDenyList(), privilege.getAllowList());

		// delegate privilege replacement to persistence handler
		persistenceHandler.addOrReplacePrivilege(newPrivilege);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setUserLocaleState(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.util.Locale)
	 */
	@Override
	public void setUserLocaleState(Certificate certificate, String username, Locale locale) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// get User
		User user = getUser(username);
		if (user == null) {
			throw new PrivilegeException("User " + username + " does not exist!");
		}

		// create new user
		User newUser = new User(user.getUsername(), user.getPassword(certificate), user.getFirstname(), user
				.getSurname(), user.getState(), user.getRoles(), locale);

		// delegate user replacement to persistence handler
		persistenceHandler.addOrReplaceUser(newUser);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setUserName(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public void setUserName(Certificate certificate, String username, String firstname, String surname) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// get User
		User user = getUser(username);
		if (user == null) {
			throw new PrivilegeException("User " + username + " does not exist!");
		}

		// create new user
		User newUser = new User(user.getUsername(), user.getPassword(certificate), firstname, surname, user.getState(),
				user.getRoles(), user.getLocale());

		// delegate user replacement to persistence handler
		persistenceHandler.addOrReplaceUser(newUser);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setUserPassword(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void setUserPassword(Certificate certificate, String username, String password) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// get User
		User user = getUser(username);
		if (user == null) {
			throw new PrivilegeException("User " + username + " does not exist!");
		}

		// hash password
		String passwordHash = PrivilegeContainer.getInstance().getEncryptionHandler().convertToHash(password);

		// create new user
		User newUser = new User(user.getUsername(), passwordHash, user.getFirstname(), user.getSurname(), user
				.getState(), user.getRoles(), user.getLocale());

		// delegate user replacement to persistence handler
		persistenceHandler.addOrReplaceUser(newUser);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setUserState(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, ch.eitchnet.privilege.model.UserState)
	 */
	@Override
	public void setUserState(Certificate certificate, String username, UserState state) {

		// validate who is doing this
		if (!PrivilegeHelper.isUserPrivilegeAdmin(certificate)) {
			logger.error("User does not have " + PrivilegeContainer.PRIVILEGE_ADMIN_ROLE + " role! Certificate: "
					+ certificate);
			return;
		}

		// get User
		User user = getUser(username);
		if (user == null) {
			throw new PrivilegeException("User " + username + " does not exist!");
		}

		// create new user
		User newUser = new User(user.getUsername(), user.getPassword(certificate), user.getFirstname(), user
				.getSurname(), state, user.getRoles(), user.getLocale());

		// delegate user replacement to persistence handler
		persistenceHandler.addOrReplaceUser(newUser);
	}

	/**
	 * @see ch.eitchnet.privilege.base.PrivilegeContainerObject#initialize(org.dom4j.Element)
	 */
	@Override
	public void initialize(Element element) {
		// nothing to initialize
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#getPrivilege(java.lang.String)
	 */
	@Override
	public Privilege getPrivilege(String privilegeName) {
		return persistenceHandler.getPrivilege(privilegeName);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#getRole(java.lang.String)
	 */
	@Override
	public Role getRole(String roleName) {
		return persistenceHandler.getRole(roleName);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#getUser(java.lang.String)
	 */
	@Override
	public User getUser(String username) {
		return persistenceHandler.getUser(username);
	}
}
