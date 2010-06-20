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

import java.util.List;
import java.util.Locale;

import org.dom4j.Element;

import ch.eitchnet.privilege.helper.PrivilegeHelper;
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
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#addOrReplaceRole(ch.eitchnet.privilege.model.Certificate,
	 *      ch.eitchnet.privilege.model.RoleRep)
	 */
	@Override
	public void addOrReplaceRole(Certificate certificate, RoleRep roleRep) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#addOrReplaceUser(ch.eitchnet.privilege.model.Certificate,
	 *      ch.eitchnet.privilege.model.UserRep)
	 */
	@Override
	public void addOrReplaceUser(Certificate certificate, UserRep userRep) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#addPrivilegeToRole(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void addPrivilegeToRole(Certificate certificate, String roleName, String privilegeName) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#addRoleToUser(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void addRoleToUser(Certificate certificate, String username, String rolename) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#persist(ch.eitchnet.privilege.model.Certificate)
	 */
	@Override
	public boolean persist(Certificate certificate) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

		return false;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#removePrivilege(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String)
	 */
	@Override
	public PrivilegeRep removePrivilege(Certificate certificate, String privilegeName) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

		return null;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#removePrivilegeFromRole(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void removePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#removeRole(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String)
	 */
	@Override
	public RoleRep removeRole(Certificate certificate, String roleName) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

		return null;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#removeRoleFromUser(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void removeRoleFromUser(Certificate certificate, String username, String rolename) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#removeUser(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String)
	 */
	@Override
	public UserRep removeUser(Certificate certificate, String username) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

		return null;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setPrivilegeAllAllowed(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, boolean)
	 */
	@Override
	public void setPrivilegeAllAllowed(Certificate certificate, String privilegeName, boolean allAllowed) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setPrivilegeAllowList(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.util.List)
	 */
	@Override
	public void setPrivilegeAllowList(Certificate certificate, String privilegeName, List<String> allowList) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setPrivilegeDenyList(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.util.List)
	 */
	@Override
	public void setPrivilegeDenyList(Certificate certificate, String privilegeName, List<String> denyList) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setPrivilegePolicy(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void setPrivilegePolicy(Certificate certificate, String privilegeName, String policyName) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setUserLocaleState(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.util.Locale)
	 */
	@Override
	public void setUserLocaleState(Certificate certificate, String username, Locale locale) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setUserNamePassword(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String, java.lang.String)
	 */
	@Override
	public void setUserNamePassword(Certificate certificate, String username, String firstname, String surname) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setUserPassword(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, java.lang.String)
	 */
	@Override
	public void setUserPassword(Certificate certificate, String username, String password) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.handler.ModelHandler#setUserState(ch.eitchnet.privilege.model.Certificate,
	 *      java.lang.String, ch.eitchnet.privilege.model.UserState)
	 */
	@Override
	public void setUserState(Certificate certificate, String username, UserState state) {

		// validate who is doing this
		PrivilegeHelper.isUserPrivilegeAdmin(certificate);

		// TODO Auto-generated method stub

	}

	/**
	 * @see ch.eitchnet.privilege.base.PrivilegeContainerObject#initialize(org.dom4j.Element)
	 */
	@Override
	public void initialize(Element element) {
		// TODO Auto-generated method stub

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
