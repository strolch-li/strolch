/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ch.eitchnet.privilege.handler;

import java.util.List;
import java.util.Locale;
import java.util.Map;

import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.IPrivilege;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.privilege.model.PrivilegeRep;
import ch.eitchnet.privilege.model.RoleRep;
import ch.eitchnet.privilege.model.UserRep;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

/**
 * The {@link PrivilegeHandler} is the centrally exposed API for accessing the privilege library. It exposes all needed
 * methods to access Privilege data model objects, modify them and validate if users or roles have privileges to perform
 * an action
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PrivilegeHandler {

	/**
	 * PRIVILEGE_ADMIN_ROLE = PrivilegeAdmin: This is the role users must have, if they are allowed to modify objects
	 */
	public static final String PRIVILEGE_ADMIN_ROLE = "PrivilegeAdmin"; //$NON-NLS-1$

	/**
	 * Returns a {@link UserRep} for the given username
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 *            the name of the {@link UserRep} to return
	 * 
	 * @return the {@link UserRep} for the given username, or null if it was not found
	 */
	public UserRep getUser(Certificate certificate, String username);

	/**
	 * Returns a {@link RoleRep} for the given roleName
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleName
	 *            the name of the {@link RoleRep} to return
	 * 
	 * @return the {@link RoleRep} for the given roleName, or null if it was not found
	 */
	public RoleRep getRole(Certificate certificate, String roleName);

	/**
	 * Returns the map of {@link PrivilegePolicy} definitions
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * 
	 * @return the map of {@link PrivilegePolicy} definitions
	 */
	public Map<String, String> getPolicyDefs(Certificate certificate);

	/**
	 * Returns all {@link RoleRep RoleReps}
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * 
	 * @return the list of {@link RoleRep RoleReps}
	 */
	public List<RoleRep> getRoles(Certificate certificate);

	/**
	 * Returns all {@link UserRep UserReps}
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * 
	 * @return the list of {@link UserRep UserReps}
	 */
	public List<UserRep> getUsers(Certificate certificate);

	/**
	 * Method to query {@link UserRep} which meet the criteria set in the given {@link UserRep}. Null fields mean the
	 * fields are irrelevant.
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param selectorRep
	 *            the {@link UserRep} to use as criteria selection
	 * 
	 * @return a list of {@link UserRep}s which fit the given criteria
	 */
	public List<UserRep> queryUsers(Certificate certificate, UserRep selectorRep);

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
	public UserRep removeRoleFromUser(Certificate certificate, String username, String roleName)
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
	 *             if there is anything wrong with this certificate or the role is still in use by a user
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
	public RoleRep removePrivilegeFromRole(Certificate certificate, String roleName, String privilegeName)
			throws AccessDeniedException, PrivilegeException;

	/**
	 * <p>
	 * Adds a new user with the information from this {@link UserRep}
	 * </p>
	 * 
	 * <p>
	 * If the password given is null, then the user is created, but can not not login! Otherwise the password must meet
	 * the requirements of the implementation under {@link PrivilegeHandler#validatePassword(byte[])}
	 * </p>
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param userRep
	 *            the {@link UserRep} containing the information to create the new {@link User}
	 * @param password
	 *            the password of the new user. If the password is null, then this is accepted but the user can not
	 *            login, otherwise the password must be validated against
	 *            {@link PrivilegeHandler#validatePassword(byte[])}
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate or the user already exists
	 */
	public UserRep addUser(Certificate certificate, UserRep userRep, byte[] password) throws AccessDeniedException,
			PrivilegeException;

	/**
	 * <p>
	 * Updates the fields for the user with the given user. All fields on the given {@link UserRep} which are non-null
	 * will be updated on the existing user. The username on the given {@link UserRep} must be set and correspond to an
	 * existing user.
	 * </p>
	 * 
	 * The following fields are considered updateable:
	 * <ul>
	 * <li>{@link UserRep#getFirstname()}</li>
	 * <li>{@link UserRep#getLastname()}</li>
	 * <li>{@link UserRep#getLocale()}</li>
	 * <li>{@link UserRep#getProperties()} - the existing properties will be replaced with the given properties</li>
	 * </ul>
	 * 
	 * <p>
	 * Any other fields will be ignored
	 * </p>
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param userRep
	 *            the {@link UserRep} with the fields set to their new values
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate or if the user does not exist
	 */
	public UserRep updateUser(Certificate certificate, UserRep userRep) throws AccessDeniedException,
			PrivilegeException;

	/**
	 * <p>
	 * Replaces the existing user with the information from this {@link UserRep} if the user already exists
	 * </p>
	 * 
	 * <p>
	 * If the password given is null, then the user is created, but can not not login! Otherwise the password must meet
	 * the requirements of the implementation under {@link PrivilegeHandler#validatePassword(byte[])}
	 * </p>
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param userRep
	 *            the {@link UserRep} containing the information to replace the existing {@link User}
	 * @param password
	 *            the password of the new user. If the password is null, then this is accepted but the user can not
	 *            login, otherwise the password must be validated against
	 *            {@link PrivilegeHandler#validatePassword(byte[])}
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate or if the user does not exist
	 */
	public UserRep replaceUser(Certificate certificate, UserRep userRep, byte[] password) throws AccessDeniedException,
			PrivilegeException;

	/**
	 * Adds a new role with the information from this {@link RoleRep}
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleRep
	 *            the {@link RoleRep} containing the information to create the new {@link Role}
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate or if the role already exists
	 */
	public RoleRep addRole(Certificate certificate, RoleRep roleRep) throws AccessDeniedException, PrivilegeException;

	/**
	 * Replaces the existing role with the information from this {@link RoleRep}
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleRep
	 *            the {@link RoleRep} containing the information to replace the existing {@link Role}
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate or if the role does not exist
	 */
	public RoleRep replaceRole(Certificate certificate, RoleRep roleRep) throws AccessDeniedException,
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
	 *             if there is anything wrong with this certificate or if the role does not exist
	 */
	public UserRep addRoleToUser(Certificate certificate, String username, String roleName)
			throws AccessDeniedException, PrivilegeException;

	/**
	 * Adds the {@link PrivilegeRep} to the {@link Role} with the given roleName or replaces it, if it already exists
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleName
	 *            the roleName of the {@link Role} to which the privilege should be added
	 * @param privilegeRep
	 *            the representation of the {@link IPrivilege} which should be added or replaced on the {@link Role}
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate or the role does not exist
	 */
	public RoleRep addOrReplacePrivilegeOnRole(Certificate certificate, String roleName, PrivilegeRep privilegeRep)
			throws AccessDeniedException, PrivilegeException;

	/**
	 * <p>
	 * Changes the password for the {@link User} with the given username. If the password is null, then the {@link User}
	 * can not login anymore. Otherwise the password must meet the requirements of the implementation under
	 * {@link PrivilegeHandler#validatePassword(byte[])}
	 * </p>
	 * 
	 * <p>
	 * It should be possible for a user to change their own password
	 * </p>
	 * 
	 * @param certificate
	 *            the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username
	 *            the username of the {@link User} for which the password is to be changed
	 * @param password
	 *            the new password for this user. If the password is null, then the {@link User} can not login anymore.
	 *            Otherwise the password must meet the requirements of the implementation under
	 *            {@link PrivilegeHandler#validatePassword(byte[])}
	 * 
	 * @throws AccessDeniedException
	 *             if the user for this certificate may not perform the action
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public void setUserPassword(Certificate certificate, String username, byte[] password)
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
	public UserRep setUserState(Certificate certificate, String username, UserState state)
			throws AccessDeniedException, PrivilegeException;

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
	public UserRep setUserLocale(Certificate certificate, String username, Locale locale) throws AccessDeniedException,
			PrivilegeException;

	/**
	 * Authenticates a user by validating that a {@link User} for the given username and password exist and then returns
	 * a {@link Certificate} with which this user may then perform actions
	 * 
	 * @param username
	 *            the username of the {@link User} which is registered in the {@link PersistenceHandler}
	 * @param password
	 *            the password with which this user is to be authenticated. Null passwords are not accepted and they
	 *            must meet the requirements of the {@link #validatePassword(byte[])}-method
	 * 
	 * @return a {@link Certificate} with which this user may then perform actions
	 * 
	 * @throws AccessDeniedException
	 *             if the user credentials are not valid
	 */
	public Certificate authenticate(String username, byte[] password) throws AccessDeniedException;

	/**
	 * Invalidates the session for the given {@link Certificate}, effectively logging out the user who was authenticated
	 * with the credentials associated to the given {@link Certificate}
	 * 
	 * @param certificate
	 *            the {@link Certificate} for which the session is to be invalidated
	 * @return true if the session was still valid and is now invalidated, false otherwise
	 */
	public boolean invalidateSession(Certificate certificate);

	/**
	 * Checks if the given {@link Certificate} is valid. This means that the certificate is for a valid session and that
	 * the user exists for the certificate. This method checks if the {@link Certificate} has been tampered with
	 * 
	 * @param certificate
	 *            the {@link Certificate} to check
	 * 
	 * @throws PrivilegeException
	 *             if there is anything wrong with this certificate
	 */
	public void isCertificateValid(Certificate certificate) throws PrivilegeException;

	/**
	 * Checks that the given password belongs to the given {@link Certificate}. If it doesn't, then a
	 * {@link PrivilegeException} is thrown
	 * 
	 * @param certificate
	 *            the certificate for which to check the password
	 * @param password
	 *            the password to check against the user from the certificate
	 * 
	 * @throws PrivilegeException
	 *             if the certificate is invalid or the password does not match
	 */
	public void checkPassword(Certificate certificate, byte[] password) throws PrivilegeException;

	/**
	 * Returns the {@link PrivilegeContext} for the given {@link Certificate}. The {@link PrivilegeContext} is an
	 * encapsulated state of a user's privileges so that for the duration of a user's call, the user can perform their
	 * actions and do not need to access the {@link PrivilegeHandler} anymore
	 * 
	 * @param certificate
	 *            a valid {@link Certificate} for which a {@link PrivilegeContext} is to be returned
	 * @return the {@link PrivilegeContext} for the given {@link Certificate}
	 * 
	 * @throws PrivilegeException
	 *             if there is a configuration error or the {@link Certificate} is invalid
	 */
	public PrivilegeContext getPrivilegeContext(Certificate certificate) throws PrivilegeException;

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
	 * If the user is not the administrator, then a {@link ch.eitchnet.privilege.base.PrivilegeException} is thrown
	 * </p>
	 * 
	 * @param certificate
	 *            the {@link Certificate} for which the role should be validated against
	 * 
	 * @throws AccessDeniedException
	 *             if the user does not not have admin privileges
	 */
	public void assertIsPrivilegeAdmin(Certificate certificate) throws AccessDeniedException;

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
	public void validatePassword(byte[] password) throws PrivilegeException;

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

	/**
	 * Special method to perform work as a System user, meaning the given systemUsername corresponds to an account which
	 * has the state {@link UserState#SYSTEM} and this user must have privilege to perform the concrete implementation
	 * of the given {@link SystemUserAction} instance
	 * 
	 * @param systemUsername
	 *            the username of the system user to perform the action as
	 * @param action
	 *            the action to be performed as the system user
	 */
	public void runAsSystem(String systemUsername, SystemUserAction action) throws PrivilegeException;
}
