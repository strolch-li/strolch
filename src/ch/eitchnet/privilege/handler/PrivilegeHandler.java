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

import java.util.Locale;
import java.util.Set;

import org.dom4j.Element;

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
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

/**
 * @author rvonburg
 * 
 */
public interface PrivilegeHandler {

	/**
	 * @param certificate
	 * @param restrictable
	 * 
	 * @return
	 * 
	 * @throws AccessDeniedException
	 *             if the {@link Certificate} is not for a currently logged in {@link User} or if the user may not
	 *             perform the action defined by the {@link Restrictable} implementation
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public boolean actionAllowed(Certificate certificate, Restrictable restrictable);

	/**
	 * @param role
	 * @param restrictable
	 * @return
	 * 
	 * @throws AccessDeniedException
	 *             if the {@link Certificate} is not for a currently logged in {@link User} or if the user may not
	 *             perform the action defined by the {@link Restrictable} implementation
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public boolean actionAllowed(Role role, Restrictable restrictable);

	/**
	 * @param certificate
	 * @return
	 * 
	 * @throws AccessDeniedException
	 *             if the {@link Certificate} is not for a currently logged in {@link User}
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public boolean isCertificateValid(Certificate certificate);

	/**
	 * @param username
	 * @param password
	 * 
	 * @return
	 * 
	 * @throws AccessDeniedException
	 *             if the user credentials are not valid
	 */
	public Certificate authenticate(String username, String password);

	public User getUser(String username);

	public void addOrReplaceUser(Certificate certificate, UserRep userRep, String password);

	public UserRep removeUser(Certificate certificate, String username);

	public void setUserPassword(Certificate certificate, String username, String password);

	public void setUserName(Certificate certificate, String username, String firstname, String surname);

	public void setUserState(Certificate certificate, String username, UserState state);

	public void setUserLocaleState(Certificate certificate, String username, Locale locale);

	public void addRoleToUser(Certificate certificate, String username, String roleName);

	public void removeRoleFromUser(Certificate certificate, String username, String roleName);

	public void addOrReplaceRole(Certificate certificate, RoleRep roleRep);

	public Role getRole(String roleName);

	public RoleRep removeRole(Certificate certificate, String roleName);

	public void addPrivilegeToRole(Certificate certificate, String roleName, String privilegeName);

	public void removePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName);

	public Privilege getPrivilege(String privilegeName);

	public void addOrReplacePrivilege(Certificate certificate, PrivilegeRep privilegeRep);

	public PrivilegeRep removePrivilege(Certificate certificate, String privilegeName);

	public void setPrivilegePolicy(Certificate certificate, String privilegeName, String policyName);

	public void setPrivilegeAllAllowed(Certificate certificate, String privilegeName, boolean allAllowed);

	public void setPrivilegeDenyList(Certificate certificate, String privilegeName, Set<String> denyList);

	public void setPrivilegeAllowList(Certificate certificate, String privilegeName, Set<String> allowList);

	public PrivilegePolicy getPolicy(String policyName);

	public boolean persist(Certificate certificate);

	public void initialize(Element element);
}
