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
import java.util.Map;
import java.util.Set;

import ch.eitchnet.privilege.i18n.AccessDeniedException;
import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.RoleRep;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

/**
 * @author rvonburg
 * 
 */
public interface PrivilegeHandler {

	/**
	 * This is the role users must have, if they are allowed to modify objects
	 */
	public static final String PRIVILEGE_ADMIN_ROLE = "PrivilegeAdmin";

	/**
	 * @param username
	 * 
	 * @return
	 */
	public UserRep getUser(String username);

	/**
	 * @param roleName
	 * 
	 * @return
	 */
	public RoleRep getRole(String roleName);

	/**
	 * @param privilegeName
	 * 
	 * @return
	 */
	public PrivilegeRep getPrivilege(String privilegeName);

	/**
	 * @param policyName
	 * 
	 * @return
	 */
	public PrivilegePolicy getPolicy(String policyName);

	/**
	 * @param certificate
	 * @param username
	 * 
	 * @return
	 */
	public UserRep removeUser(Certificate certificate, String username);

	/**
	 * @param certificate
	 * @param username
	 * @param roleName
	 */
	public void removeRoleFromUser(Certificate certificate, String username, String roleName);

	/**
	 * @param certificate
	 * @param roleName
	 * 
	 * @return
	 */
	public RoleRep removeRole(Certificate certificate, String roleName);

	/**
	 * @param certificate
	 * @param roleName
	 * @param privilegeName
	 */
	public void removePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName);

	/**
	 * @param certificate
	 * @param privilegeName
	 * 
	 * @return
	 */
	public PrivilegeRep removePrivilege(Certificate certificate, String privilegeName);

	/**
	 * @param certificate
	 * @param userRep
	 * @param password
	 */
	public void addOrReplaceUser(Certificate certificate, UserRep userRep, String password);

	/**
	 * @param certificate
	 * @param roleRep
	 */
	public void addOrReplaceRole(Certificate certificate, RoleRep roleRep);

	/**
	 * @param certificate
	 * @param privilegeRep
	 */
	public void addOrReplacePrivilege(Certificate certificate, PrivilegeRep privilegeRep);

	/**
	 * @param certificate
	 * @param username
	 * @param roleName
	 */
	public void addRoleToUser(Certificate certificate, String username, String roleName);

	/**
	 * @param certificate
	 * @param roleName
	 * @param privilegeName
	 */
	public void addPrivilegeToRole(Certificate certificate, String roleName, String privilegeName);

	/**
	 * @param certificate
	 * @param username
	 * @param password
	 */
	public void setUserPassword(Certificate certificate, String username, String password);

	/**
	 * @param certificate
	 * @param username
	 * @param firstname
	 * @param surname
	 */
	public void setUserName(Certificate certificate, String username, String firstname, String surname);

	/**
	 * @param certificate
	 * @param username
	 * @param state
	 */
	public void setUserState(Certificate certificate, String username, UserState state);

	/**
	 * @param certificate
	 * @param username
	 * @param locale
	 */
	public void setUserLocaleState(Certificate certificate, String username, Locale locale);

	/**
	 * @param certificate
	 * @param privilegeName
	 * @param policyName
	 */
	public void setPrivilegePolicy(Certificate certificate, String privilegeName, String policyName);

	/**
	 * @param certificate
	 * @param privilegeName
	 * @param allAllowed
	 */
	public void setPrivilegeAllAllowed(Certificate certificate, String privilegeName, boolean allAllowed);

	/**
	 * @param certificate
	 * @param privilegeName
	 * @param denyList
	 */
	public void setPrivilegeDenyList(Certificate certificate, String privilegeName, Set<String> denyList);

	/**
	 * @param certificate
	 * @param privilegeName
	 * @param allowList
	 */
	public void setPrivilegeAllowList(Certificate certificate, String privilegeName, Set<String> allowList);

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
	 * <p>
	 * Validates if this {@link Certificate} is for a {@link ch.eitchnet.privilege.model.internal.User} with
	 * {@link Role} with name {@link PrivilegeHandler#PRIVILEGE_ADMIN_ROLE}
	 * </p>
	 * 
	 * <p>
	 * In other words, this method checks if the given certificate is for a user who has the rights to change objects
	 * </p>
	 * 
	 * <p>
	 * If the user is not the administrator, then a {@link ch.eitchnet.privilege.i18n.PrivilegeException} is thrown
	 * </p>
	 * 
	 * @param certificate
	 *            the {@link Certificate} for which the role should be validated against
	 * 
	 * @throws ch.eitchnet.privilege.i18n.PrivilegeException
	 *             if the user does not not have admin privileges
	 */
	public void validateIsPrivilegeAdmin(Certificate certificate) throws PrivilegeException;

	/**
	 * Validate that the given password meets any requirements. What these requirements are is a decision made by the
	 * concrete implementation
	 * 
	 * @param password
	 * 
	 * @throws PrivilegeException
	 */
	public void validatePassword(String password) throws PrivilegeException;

	/**
	 * @param certificate
	 * 
	 * @return
	 */
	public boolean persist(Certificate certificate);

	/**
	 * 
	 * @param parameterMap
	 * @param encryptionHandler
	 * @param persistenceHandler
	 */
	public void initialize(Map<String, String> parameterMap, EncryptionHandler encryptionHandler,
			PersistenceHandler persistenceHandler);
}
