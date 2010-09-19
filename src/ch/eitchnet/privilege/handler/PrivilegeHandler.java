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
	 * value = PrivilegeAdmin: This is the role users must have, if they are allowed to modify objects
	 */
	public static final String PRIVILEGE_ADMIN_ROLE = "PrivilegeAdmin";

	/**
	 * Returns a {@link UserRep} for the given username
	 * 
	 * @param username
	 *            the name of the {@link UserRep} to return
	 * 
	 * @return the {@link UserRep} for the given username, or null if it was not found
	 */
	public UserRep getUser(String username);

	/**
	 * Returns a {@link RoleRep} for the given roleName
	 * 
	 * @param roleName
	 *            the name of the {@link RoleRep} to return
	 * 
	 * @return the {@link RoleRep} for the given roleName, or null if it was not found
	 */
	public RoleRep getRole(String roleName);

	/**
	 * Returns a {@link PrivilegeRep} for the given privilegeName
	 * 
	 * @param privilegeName
	 *            the name of the {@link PrivilegeRep} to return
	 * 
	 * @return the {@link PrivilegeRep} for the given privilegeName, or null if it was not found
	 */
	public PrivilegeRep getPrivilege(String privilegeName);

	/**
	 * Returns a {@link PrivilegePolicy} for the given policyName
	 * 
	 * @param policyName
	 *            the name of the {@link PrivilegePolicy} to return
	 * 
	 * @return the {@link PrivilegePolicy} for the given policyName, or null if it was not found
	 */
	public PrivilegePolicy getPolicy(String policyName);

	/**
	 * Removes the user with the given username
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 *            the username of the user to remove
	 * 
	 * @return the {@link UserRep} of the user removed, or null if the user did not exist
	 */
	public UserRep removeUser(Certificate certificate, String username);

	/**
	 * Removes the role with the given roleName from the user with the given username
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 *            the username of the user from which the role is to be removed
	 * @param roleName
	 *            the rolename of the role to remove from the user
	 */
	public void removeRoleFromUser(Certificate certificate, String username, String roleName);

	/**
	 * Removes the role with the given roleName
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleName
	 *            the roleName of the role to remove
	 * 
	 * @return the {@link RoleRep} of the role removed, or null if the role did not exist
	 */
	public RoleRep removeRole(Certificate certificate, String roleName);

	/**
	 * Removes the privilege with the given privilegeName from the role with the given roleName
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleName
	 *            the roleName of the role from which the privilege is to be removed
	 * @param privilegeName
	 *            the privilegeName of the privilege to remove from the role
	 */
	public void removePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName);

	/**
	 * Removes the privilege with the given privilegeName
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param privilegeName
	 *            the privilegeName of the privilege to remove
	 * 
	 * @return the {@link PrivilegeRep} of the privilege removed, or null if the privilege did not exist
	 */
	public PrivilegeRep removePrivilege(Certificate certificate, String privilegeName);

	/**
	 * <p>
	 * Adds a new user, or replaces the user with the information from this {@link UserRep} if the user already exists
	 * </p>
	 * 
	 * <p>
	 * If the password given is null, then the user is created, but can not not login! Otherwise the password must meet
	 * the requirements of the implementation under {@link PrivilegeHandler#validatePassword(String)}
	 * </p>
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param userRep
	 *            the {@link UserRep} containing the information to create the new {@link User}
	 * @param password
	 *            the password of the new user. If the password is null, then this is accepted but the user can not
	 *            login, otherwise the password must be validated against
	 *            {@link PrivilegeHandler#validatePassword(String)}
	 */
	public void addOrReplaceUser(Certificate certificate, UserRep userRep, String password);

	/**
	 * Adds a new role, or replaces the role with the information from this {@link RoleRep} if the role already exists
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleRep
	 *            the {@link RoleRep} containing the information to create the new {@link Role}
	 */
	public void addOrReplaceRole(Certificate certificate, RoleRep roleRep);

	/**
	 * Adds a new privilege, or replaces the privilege with the information from this {@link PrivilegeRep} if the
	 * privilege already exists
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param privilegeRep
	 *            the {@link PrivilegeRep} containing the information to create the new {@link Privilege}
	 */
	public void addOrReplacePrivilege(Certificate certificate, PrivilegeRep privilegeRep);

	/**
	 * Adds the role with the given roleName to the {@link User} with the given username
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 *            the username of the {@link User} to which the role should be added
	 * @param roleName
	 *            the roleName of the {@link Role} which should be added to the {@link User}
	 */
	public void addRoleToUser(Certificate certificate, String username, String roleName);

	/**
	 * Adds the privilege with the given privilegeName to the {@link Role} with the given roleName
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleName
	 *            the roleName of the {@link Role} to which the privilege should be added
	 * @param privilegeName
	 *            the privilegeName of the {@link Privilege} which should be added to the {@link Role}
	 */
	public void addPrivilegeToRole(Certificate certificate, String roleName, String privilegeName);

	/**
	 * Changes the password for the {@link User} with the given username. If the password is null, then the {@link User}
	 * can not login anymore. Otherwise the password must meet the requirements of the implementation under
	 * {@link PrivilegeHandler#validatePassword(String)}
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 *            the username of the {@link User} for which the password is to be changed
	 * @param password
	 *            the new password for this user. If the password is null, then the {@link User} can not login anymore.
	 *            Otherwise the password must meet the requirements of the implementation under
	 *            {@link PrivilegeHandler#validatePassword(String)}
	 */
	public void setUserPassword(Certificate certificate, String username, String password);

	/**
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 * @param firstname
	 * @param surname
	 */
	public void setUserName(Certificate certificate, String username, String firstname, String surname);

	/**
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 * @param state
	 */
	public void setUserState(Certificate certificate, String username, UserState state);

	/**
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 * @param locale
	 */
	public void setUserLocaleState(Certificate certificate, String username, Locale locale);

	/**
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param privilegeName
	 * @param policyName
	 */
	public void setPrivilegePolicy(Certificate certificate, String privilegeName, String policyName);

	/**
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param privilegeName
	 * @param allAllowed
	 */
	public void setPrivilegeAllAllowed(Certificate certificate, String privilegeName, boolean allAllowed);

	/**
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param privilegeName
	 * @param denyList
	 */
	public void setPrivilegeDenyList(Certificate certificate, String privilegeName, Set<String> denyList);

	/**
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
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
	 *            the {@link Certificate} of the user which has the privilege to perform this action
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
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * 
	 * @return
	 */
	public boolean persist(Certificate certificate);

	/**
	 * Initialize the concrete {@link EncryptionHandler}. The passed parameter map contains any configuration this map
	 * might need
	 * 
	 * @param parameterMap
	 *            a map containing configuration properties
	 * 
	 * @param encryptionHandler
	 * @param persistenceHandler
	 */
	public void initialize(Map<String, String> parameterMap, EncryptionHandler encryptionHandler,
			PersistenceHandler persistenceHandler);
}
