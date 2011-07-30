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

/**
 * The {@link PrivilegeHandler} is the centrally exposed API for accessing the privilege library. It exposes all needed
 * methods to access Privilege data model objects, modify them and validate if users or roles have privileges to perform
 * an action
 * 
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
	 * Removes the user with the given username
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 *            the username of the user to remove
	 * 
	 * @return the {@link UserRep} of the user removed, or null if the user did not exist
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public UserRep removeUser(Certificate certificate, String username) throws AccessDeniedException,
			PrivilegeException;

	/**
	 * Removes the role with the given roleName from the user with the given username
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 *            the username of the user from which the role is to be removed
	 * @param roleName
	 *            the roleName of the role to remove from the user
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public void removeRoleFromUser(Certificate certificate, String username, String roleName)
			throws AccessDeniedException, PrivilegeException;

	/**
	 * Removes the role with the given roleName
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleName
	 *            the roleName of the role to remove
	 * 
	 * @return the {@link RoleRep} of the role removed, or null if the role did not exist
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public RoleRep removeRole(Certificate certificate, String roleName) throws AccessDeniedException,
			PrivilegeException;

	/**
	 * Removes the privilege with the given privilegeName from the role with the given roleName
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleName
	 *            the roleName of the role from which the privilege is to be removed
	 * @param privilegeName
	 *            the privilegeName of the privilege to remove from the role
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public void removePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName)
			throws AccessDeniedException, PrivilegeException;

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
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public void addOrReplaceUser(Certificate certificate, UserRep userRep, String password)
			throws AccessDeniedException, PrivilegeException;

	/**
	 * Adds a new role, or replaces the role with the information from this {@link RoleRep} if the role already exists
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleRep
	 *            the {@link RoleRep} containing the information to create the new {@link Role}
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public void addOrReplaceRole(Certificate certificate, RoleRep roleRep) throws AccessDeniedException,
			PrivilegeException;

	/**
	 * Adds the role with the given roleName to the {@link User} with the given username
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 *            the username of the {@link User} to which the role should be added
	 * @param roleName
	 *            the roleName of the {@link Role} which should be added to the {@link User}
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public void addRoleToUser(Certificate certificate, String username, String roleName) throws AccessDeniedException,
			PrivilegeException;

	/**
	 * Adds the {@link PrivilegeRep} to the {@link Role} with the given roleName
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleName
	 *            the roleName of the {@link Role} to which the privilege should be added
	 * @param privilegeRep
	 *            the representation of the {@link Privilege} which should be added to the {@link Role}
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public void addOrReplacePrivilegeOnRole(Certificate certificate, String roleName, PrivilegeRep privilegeRep)
			throws AccessDeniedException, PrivilegeException;

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
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public void setUserPassword(Certificate certificate, String username, String password)
			throws AccessDeniedException, PrivilegeException;

	/**
	 * Changes the name of the user. This changes the first name and the surname. If either value is null, then that
	 * value is not changed
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 *            the username of the {@link User} for which the name is to be changed
	 * @param firstname
	 *            the new first name
	 * @param surname
	 *            the new surname
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public void setUserName(Certificate certificate, String username, String firstname, String surname)
			throws AccessDeniedException, PrivilegeException;

	/**
	 * Changes the {@link UserState} of the user
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 *            the username of the {@link User} for which the {@link UserState} is to be changed
	 * @param state
	 *            the new state for the user
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public void setUserState(Certificate certificate, String username, UserState state) throws AccessDeniedException,
			PrivilegeException;

	/**
	 * Changes the {@link Locale} of the user
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 *            the username of the {@link User} for which the {@link Locale} is to be changed
	 * @param locale
	 *            the new {@link Locale} for the user
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public void setUserLocale(Certificate certificate, String username, Locale locale) throws AccessDeniedException,
			PrivilegeException;

	/**
	 * Authenticates a user by validating that a {@link User} for the given username and password exist and then returns
	 * a {@link Certificate} with which this user may then perform actions
	 * 
	 * @param username
	 *            the username of the {@link User} which is registered in the {@link PersistenceHandler}
	 * @param password
	 *            the password with which this user is to be authenticated. Null passwords are not accepted and they
	 *            must meet the requirements of the {@link #validatePassword(String)}-method
	 * 
	 * @return a {@link Certificate} with which this user may then perform actions
	 * 
	 * @throws AccessDeniedException
	 *             if the user credentials are not valid
	 */
	public Certificate authenticate(String username, String password) throws AccessDeniedException;

	/**
	 * Invalidates the {@link Session} for the given {@link Certificate}, effectively logging out the user who was
	 * authenticated with the credentials associated to the given {@link Certificate}
	 * 
	 * @param certificate
	 *            the {@link Certificate} for which the {@link Session} is to be invalidated
	 * @return true if the {@link Session} was still valid and is now invalidated, false otherwise
	 */
	public boolean invalidateSession(Certificate certificate);

	/**
	 * Checks if the {@link User} registered to the given {@link Certificate} is allowed to access the
	 * {@link Restrictable}
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param restrictable
	 *            the {@link Restrictable} to which the user wants access
	 * 
	 * @return true if the access is allowed, false otherwise
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action defined by the {@link Restrictable}
	 *             implementation
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public boolean actionAllowed(Certificate certificate, Restrictable restrictable) throws AccessDeniedException,
			PrivilegeException;

	/**
	 * Checks if the {@link RoleRep} is allowed to access the {@link Restrictable}
	 * 
	 * @param roleRep
	 *            the {@link RoleRep} for which access to the {@link Restrictable} is to be checked
	 * @param restrictable
	 *            the {@link Restrictable} to which access is to be checked
	 * 
	 * @return true if the {@link RoleRep} has access, false otherwise
	 * 
	 * @throws AccessDeniedException
	 *             if the role may not perform the action defined by the {@link Restrictable} implementation
	 */
	public boolean actionAllowed(RoleRep roleRep, Restrictable restrictable) throws AccessDeniedException;

	/**
	 * Checks if the given {@link Certificate} is valid. This means that the certificate is for a valid session and that
	 * the user exists for the certificate. This method checks if the {@link Certificate} has been tampered with
	 * 
	 * @param certificate
	 *            the {@link Certificate} to check
	 * 
	 * @return true if the {@link Certificate} is valid, false otherwise
	 * 
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public boolean isCertificateValid(Certificate certificate) throws PrivilegeException;

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
	 * @throws AccessDeniedException
	 *             if the user does not not have admin privileges
	 */
	public void validateIsPrivilegeAdmin(Certificate certificate) throws AccessDeniedException;

	/**
	 * Validate that the given password meets certain requirements. What these requirements are is a decision made by
	 * the concrete implementation
	 * 
	 * @param password
	 *            the password to be validated to meet certain requirements
	 * 
	 * @throws PrivilegeException
	 *             if the password does not implement the requirement of the concrete implementation
	 */
	public void validatePassword(String password) throws PrivilegeException;

	/**
	 * Persists any changes to the privilege data model. Changes are thus not persisted immediately, but must be
	 * actively performed
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * 
	 * @return true if changes were persisted, false if no changes were persisted
	 * 
	 * @throws AccessDeniedException
	 *             if the users of the given certificate does not have the privilege to perform this action
	 */
	public boolean persist(Certificate certificate) throws AccessDeniedException;
}
