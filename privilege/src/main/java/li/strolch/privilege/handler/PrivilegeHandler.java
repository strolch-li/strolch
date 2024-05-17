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
package li.strolch.privilege.handler;

import li.strolch.privilege.base.*;
import li.strolch.privilege.model.*;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.policy.PrivilegePolicy;

import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * The {@link PrivilegeHandler} is the centrally exposed API for accessing the privilege library. It exposes all needed
 * methods to access Privilege data model objects, modify them and validate if users or roles have privileges to perform
 * an action
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface PrivilegeHandler {

	///

	/**
	 * Privilege "PrivilegeAction" which is used for privileges which are not further categorized e.g. s
	 * {@link #PRIVILEGE_ACTION_PERSIST} and {@link #PRIVILEGE_ACTION_GET_POLICIES}
	 */
	String PRIVILEGE_ACTION = "PrivilegeAction";

	/**
	 * For Privilege "PrivilegeAction" value required to be able to persist changes if not exempted by auto persist or
	 * <code>allAllowed</code>
	 */
	String PRIVILEGE_ACTION_PERSIST = "Persist";

	/**
	 * For Privilege "PrivilegeAction" value required to be able to persist session if not exempted by
	 * <code>allAllowed</code>
	 */
	String PRIVILEGE_ACTION_PERSIST_SESSIONS = "PersistSessions";

	/**
	 * For Privilege "PrivilegeAction" value required to be able to reload changes if not exempted by
	 * <code>allAllowed</code>
	 */
	String PRIVILEGE_ACTION_RELOAD = "Reload";

	/**
	 * For Privilege "PrivilegeAction" value required to get currently configured policies if not
	 * <code>allAllowed</code>
	 */
	String PRIVILEGE_ACTION_GET_POLICIES = "GetPolicies";

	/**
	 * For Privilege "PrivilegeAction" value required to get a certificate if not <code>allAllowed</code>
	 */
	String PRIVILEGE_ACTION_GET_CERTIFICATE = "GetCertificate";

	/**
	 * For Privilege "PrivilegeAction" value required to get all certificates if not <code>allAllowed</code>
	 */
	String PRIVILEGE_ACTION_GET_CERTIFICATES = "GetCertificates";

	///

	/**
	 * Privilege "PrivilegeGetRole" which is used to validate that a user can get a specific role
	 */
	String PRIVILEGE_GET_ROLE = "PrivilegeGetRole";

	/**
	 * Privilege "PrivilegeAddRole" which is used to validate that a user can add a specific role
	 */
	String PRIVILEGE_ADD_ROLE = "PrivilegeAddRole";

	/**
	 * Privilege "PrivilegeRemoveRole" which is used to validate that a user can remove a specific role
	 */
	String PRIVILEGE_REMOVE_ROLE = "PrivilegeRemoveRole";

	/**
	 * Privilege "PrivilegeModifyRole" which is used to validate that a user can modify a specific role. <b>Note:</b>
	 * This includes modifying of the privileges on the role
	 */
	String PRIVILEGE_MODIFY_ROLE = "PrivilegeModifyRole";

	///

	/**
	 * For Privilege "PrivilegeGetUserPrivileges" value required to get privileges for a user
	 */
	String PRIVILEGE_GET_USER_PRIVILEGES = "PrivilegeGetUserPrivileges";

	/**
	 * Privilege "PrivilegeGetUser" which is used to validate that a user can get a specific user
	 */
	String PRIVILEGE_GET_USER = "PrivilegeGetUser";

	/**
	 * Privilege "PrivilegeAddUser" which is used to validate that a user can add a specific user
	 */
	String PRIVILEGE_ADD_USER = "PrivilegeAddUser";

	/**
	 * Privilege "PrivilegeRemoveUser" which is used to validate that a user can remove a specific user
	 */
	String PRIVILEGE_REMOVE_USER = "PrivilegeRemoveUser";

	/**
	 * Privilege "PrivilegeModifyUser" which is used to validate that a user can modify a specific user
	 */
	String PRIVILEGE_MODIFY_USER = "PrivilegeModifyUser";

	/**
	 * Privilege "PRIVILEGE_MODIFY_USER_ROLES" which is used to validate that a user may modify the roles of a user
	 * user
	 */
	String PRIVILEGE_MODIFY_USER_ROLES = "PrivilegeModifyUserRoles";

	/**
	 * Privilege "PrivilegeAddRoleToUser" which is used to validate that a user can add a specific role to a specific
	 * user
	 */
	String PRIVILEGE_ADD_ROLE_TO_USER = "PrivilegeAddRoleToUser";

	/**
	 * Privilege "PrivilegeRemoveRoleFromUser" which is used to validate that a user can remove a specific role from a
	 * specific user
	 */
	String PRIVILEGE_REMOVE_ROLE_FROM_USER = "PrivilegeRemoveRoleFromUser";

	/**
	 * Privilege "PRIVILEGE_SET_USER_LOCALE" which is used to validate that a user can set the locale of a user, or
	 * their own
	 */
	String PRIVILEGE_SET_USER_LOCALE = "PrivilegeSetUserLocale";

	/**
	 * Privilege "PRIVILEGE_SET_USER_STATE" which is used to validate that a user can set the state of a user
	 */
	String PRIVILEGE_SET_USER_STATE = "PrivilegeSetUserState";

	/**
	 * Privilege "PRIVILEGE_SET_USER_PASSWORD" which is used to validate that a user can set the password of a user, or
	 * their own
	 */
	String PRIVILEGE_SET_USER_PASSWORD = "PrivilegeSetUserPassword";

	/**
	 * Privilege "PRIVILEGE_SET_USER_PASSWORD" which is used to validate that a user can set the password of a user, or
	 * their own
	 */
	String PRIVILEGE_REQUIRE_PASSWORD_CHANGE = "RequirePasswordChange";

	///

	/**
	 * configuration parameter to define a secret_key
	 */
	String PARAM_SECRET_KEY = "secretKey";

	/**
	 * configuration parameter to define if session refreshing is allowed
	 */
	String PARAM_ALLOW_SESSION_REFRESH = "allowSessionRefresh";

	/**
	 * configuration parameter to define if a session's source change is disallowed
	 */
	String PARAM_DISALLOW_SOURCE_CHANGE = "disallowSourceChange";

	/**
	 * configuration parameter to define if username is case insensitive
	 */
	String PARAM_CASE_INSENSITIVE_USERNAME = "caseInsensitiveUsername";

	/**
	 * configuration parameter to define a secret salt
	 */
	String PARAM_SECRET_SALT = "secretSalt";

	/**
	 * configuration parameter to define automatic persisting on password change
	 */
	String PARAM_AUTO_PERSIST_ON_USER_CHANGES_DATA = "autoPersistOnUserChangesData";

	/**
	 * configuration parameter to define if sessions should be persisted
	 */
	String PARAM_PERSIST_SESSIONS = "persistSessions";

	/**
	 * configuration parameter to define where sessions are to be persisted
	 */
	String PARAM_PERSIST_SESSIONS_PATH = "persistSessionsPath";

	/**
	 * configuration parameter to define {@link PrivilegeConflictResolution}
	 */
	String PARAM_PRIVILEGE_CONFLICT_RESOLUTION = "privilegeConflictResolution";

	/**
	 * Returns a {@link UserRep} for the given username
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username    the name of the {@link UserRep} to return
	 *
	 * @return the {@link UserRep} for the given username, or null if it was not found
	 */
	UserRep getUser(Certificate certificate, String username);

	/**
	 * Returns a {@link UserPrivileges} for the given username. This object contains all the privileges the user has
	 * assigned by its roles and groups
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username    the name of the {@link UserRep} to return
	 *
	 * @return the {@link UserPrivileges} for the given username
	 *
	 * @throws PrivilegeException if the certificate may not access the user, or the request user does not exist
	 */
	UserPrivileges getUserPrivileges(Certificate certificate, String username);

	/**
	 * Returns a {@link RoleRep} for the given roleName
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleName    the name of the {@link RoleRep} to return
	 *
	 * @return the {@link RoleRep} for the given roleName, or null if it was not found
	 */
	RoleRep getRole(Certificate certificate, String roleName);

	/**
	 * Returns the map of {@link PrivilegePolicy} definitions
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 *
	 * @return the map of {@link PrivilegePolicy} definitions
	 */
	Map<String, String> getPolicyDefs(Certificate certificate);

	/**
	 * Returns the list of {@link Certificate Certificates}
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 *
	 * @return the list of {@link Certificate Certificates}
	 */
	List<Certificate> getCertificates(Certificate certificate);

	/**
	 * Returns all {@link RoleRep RoleReps}
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 *
	 * @return the list of {@link RoleRep RoleReps}
	 */
	List<RoleRep> getRoles(Certificate certificate);

	/**
	 * Returns all {@link UserRep UserReps}
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 *
	 * @return the list of {@link UserRep UserReps}
	 */
	List<UserRep> getUsers(Certificate certificate);

	/**
	 * Method to query {@link UserRep} which meet the criteria set in the given {@link UserRep}. Null fields mean the
	 * fields are irrelevant.
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param selectorRep the {@link UserRep} to use as criteria selection
	 *
	 * @return a list of {@link UserRep}s which fit the given criteria
	 */
	List<UserRep> queryUsers(Certificate certificate, UserRep selectorRep);

	/**
	 * Removes the user with the given username
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username    the username of the user to remove
	 *
	 * @return the {@link UserRep} of the user removed, or null if the user did not exist
	 *
	 * @throws AccessDeniedException if the user for this certificate may not perform the action
	 * @throws PrivilegeException    if there is anything wrong with this certificate
	 */
	UserRep removeUser(Certificate certificate, String username) throws PrivilegeException;

	/**
	 * Removes the role with the given roleName
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleName    the roleName of the role to remove
	 *
	 * @return the {@link RoleRep} of the role removed, or null if the role did not exist
	 *
	 * @throws AccessDeniedException if the user for this certificate may not perform the action
	 * @throws PrivilegeException    if there is anything wrong with this certificate or the role is still in use by a
	 *                               user
	 */
	RoleRep removeRole(Certificate certificate, String roleName) throws PrivilegeException;

	/**
	 * <p>
	 * Adds a new user with the information from this {@link UserRep}
	 * </p>
	 *
	 * <p>
	 * If the password given is null, then the user is created, but can not not login! Otherwise the password must meet
	 * the requirements of the implementation under {@link PrivilegeHandler#validatePassword(Locale, char[])}
	 * </p>
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param userRep     the {@link UserRep} containing the information to create the new {@link User}
	 * @param password    the password of the new user. If the password is null, then this is accepted but the user can
	 *                    not login, otherwise the password must be validated against
	 *                    {@link PrivilegeHandler#validatePassword(Locale, char[])}
	 *
	 * @throws AccessDeniedException if the user for this certificate may not perform the action
	 * @throws PrivilegeException    if there is anything wrong with this certificate or the user already exists
	 */
	UserRep addUser(Certificate certificate, UserRep userRep, char[] password) throws PrivilegeException;

	/**
	 * Allows the bulk adding or updating of users. If the user exists, the user's history and password is kept,
	 * otherwise the user is created without a password
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param userReps    the list of users to add or update
	 */
	void addOrUpdateUsers(Certificate certificate, List<UserRep> userReps) throws PrivilegeException;

	/**
	 * <p>
	 * Updates the fields for the user with the given user. All fields on the given {@link UserRep} which are non-null
	 * will be updated on the existing user. The username on the given {@link UserRep} must be set and correspond to an
	 * existing user.
	 * </p>
	 * <p>
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
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param userRep     the {@link UserRep} with the fields set to their new values
	 * @param password    the password of the new user. If the password is null, then this is accepted but the user can
	 *                    not login, otherwise the password must be validated against
	 *                    {@link PrivilegeHandler#validatePassword(Locale, char[])}
	 *
	 * @throws AccessDeniedException if the user for this certificate may not perform the action
	 * @throws PrivilegeException    if there is anything wrong with this certificate or if the user does not exist
	 */
	UserRep updateUser(Certificate certificate, UserRep userRep, char[] password) throws PrivilegeException;

	/**
	 * Adds a new role with the information from this {@link RoleRep}
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleRep     the {@link RoleRep} containing the information to create the new {@link Role}
	 *
	 * @throws AccessDeniedException if the user for this certificate may not perform the action
	 * @throws PrivilegeException    if there is anything wrong with this certificate or if the role already exists
	 */
	RoleRep addRole(Certificate certificate, RoleRep roleRep) throws PrivilegeException;

	/**
	 * Replaces the existing role with the information from this {@link RoleRep}
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param roleRep     the {@link RoleRep} containing the information to replace the existing {@link Role}
	 *
	 * @throws AccessDeniedException if the user for this certificate may not perform the action
	 * @throws PrivilegeException    if there is anything wrong with this certificate or if the role does not exist
	 */
	RoleRep replaceRole(Certificate certificate, RoleRep roleRep) throws PrivilegeException;

	/**
	 * <p>
	 * Changes the password for the {@link User} with the given username. If the password is null, then the {@link User}
	 * can not login anymore. Otherwise the password must meet the requirements of the implementation under
	 * {@link PrivilegeHandler#validatePassword(Locale, char[])}
	 * </p>
	 *
	 * <p>
	 * It should be possible for a user to change their own password
	 * </p>
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username    the username of the {@link User} for which the password is to be changed
	 * @param password    the new password for this user. If the password is null, then the {@link User} can not login
	 *                    anymore. Otherwise the password must meet the requirements of the implementation under
	 *                    {@link PrivilegeHandler#validatePassword(Locale, char[])}
	 *
	 * @throws AccessDeniedException if the user for this certificate may not perform the action
	 * @throws PrivilegeException    if there is anything wrong with this certificate
	 */
	void setUserPassword(Certificate certificate, String username, char[] password) throws PrivilegeException;

	/**
	 * <p>
	 * Requires the given user to change their password after next login
	 * </p>
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username    the username of the {@link User} for which the password change is requested
	 *
	 * @throws AccessDeniedException if the user for this certificate may not perform the action
	 * @throws PrivilegeException    if there is anything wrong with this certificate
	 */
	void requirePasswordChange(Certificate certificate, String username) throws PrivilegeException;

	/**
	 * Changes the {@link UserState} of the user
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username    the username of the {@link User} for which the {@link UserState} is to be changed
	 * @param state       the new state for the user
	 *
	 * @throws AccessDeniedException if the user for this certificate may not perform the action
	 * @throws PrivilegeException    if there is anything wrong with this certificate
	 */
	UserRep setUserState(Certificate certificate, String username, UserState state) throws PrivilegeException;

	/**
	 * Changes the {@link Locale} of the user
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param username    the username of the {@link User} for which the {@link Locale} is to be changed
	 * @param locale      the new {@link Locale} for the user
	 *
	 * @throws AccessDeniedException if the user for this certificate may not perform the action
	 * @throws PrivilegeException    if there is anything wrong with this certificate
	 */
	UserRep setUserLocale(Certificate certificate, String username, Locale locale) throws PrivilegeException;

	/**
	 * Initiate a password reset challenge for the given username
	 *
	 * @param usage    the usage for which the challenge is requested
	 * @param username the username of the user to initiate the challenge for
	 */
	void initiateChallengeFor(Usage usage, String username);

	/**
	 * Initiate a password reset challenge for the given username
	 *
	 * @param usage    the usage for which the challenge is requested
	 * @param username the username of the user to initiate the challenge for
	 * @param source   the source of the challenge
	 */
	void initiateChallengeFor(Usage usage, String username, String source);

	/**
	 * Validate the response of a challenge for the given username
	 *
	 * @param username  the username of the user for which the challenge is to be validated
	 * @param challenge the challenge from the user
	 *
	 * @return certificate with which the user can access the system with the {@link Usage} set to the value from the
	 * initiated challenge
	 *
	 * @throws PrivilegeException if anything goes wrong
	 */
	Certificate validateChallenge(String username, String challenge) throws PrivilegeException;

	/**
	 * Validate the response of a challenge for the given username
	 *
	 * @param username  the username of the user for which the challenge is to be validated
	 * @param challenge the challenge from the user
	 * @param source    the source of the challenge validation
	 *
	 * @return certificate with which the user can access the system with the {@link Usage} set to the value from the
	 * initiated challenge
	 *
	 * @throws PrivilegeException if anything goes wrong
	 */
	Certificate validateChallenge(String username, String challenge, String source) throws PrivilegeException;

	/**
	 * Authenticates a user by validating that a {@link User} for the given username and password exist and then returns
	 * a {@link Certificate} with which this user may then perform actions
	 *
	 * @param username  the username of the {@link User} which is registered in the {@link PersistenceHandler}
	 * @param password  the password with which this user is to be authenticated. Null passwords are not accepted and
	 *                  they must meet the requirements of the {@link #validatePassword(Locale, char[])}-method
	 * @param keepAlive should this session be kept alive
	 *
	 * @return a {@link Certificate} with which this user may then perform actions
	 *
	 * @throws AccessDeniedException if the user credentials are not valid
	 */
	Certificate authenticate(String username, char[] password, boolean keepAlive) throws AccessDeniedException;

	/**
	 * Authenticates a user by validating that a {@link User} for the given username and password exist and then returns
	 * a {@link Certificate} with which this user may then perform actions
	 *
	 * @param username  the username of the {@link User} which is registered in the {@link PersistenceHandler}
	 * @param password  the password with which this user is to be authenticated. Null passwords are not accepted and
	 *                  they must meet the requirements of the {@link #validatePassword(Locale, char[])}-method
	 * @param source    the source of the authentication request, i.e. remote IP
	 * @param usage     the usage type for this authentication
	 * @param keepAlive should this session be kept alive
	 *
	 * @return a {@link Certificate} with which this user may then perform actions
	 *
	 * @throws AccessDeniedException if the user credentials are not valid
	 */
	Certificate authenticate(String username, char[] password, String source, Usage usage, boolean keepAlive)
			throws AccessDeniedException;

	/**
	 * Authenticates a user on a remote Single Sign On service. This is implemented by the
	 *
	 * @param data      the data to perform the SSO
	 * @param keepAlive should this session be kept alive
	 *
	 * @return the {@link Certificate} for the user
	 *
	 * @throws PrivilegeException if something goes wrong with the SSO
	 */
	Certificate authenticateSingleSignOn(Object data, boolean keepAlive) throws PrivilegeException;

	/**
	 * Authenticates a user on a remote Single Sign On service. This is implemented by the
	 *
	 * @param data      the data to perform the SSO
	 * @param source    the source of the SSO authentication
	 * @param keepAlive may the certificate be kept alive
	 *
	 * @return the {@link Certificate} for the user
	 *
	 * @throws PrivilegeException if something goes wrong with the SSO
	 */
	Certificate authenticateSingleSignOn(Object data, String source, boolean keepAlive) throws PrivilegeException;

	/**
	 * Refreshes the given certificate's session with a new session, i.e. a new certificate
	 *
	 * @param certificate the certificate for which to perform a refresh
	 * @param source      the source of the refresh request
	 *
	 * @return a {@link Certificate} with which this user may then perform actions
	 *
	 * @throws AccessDeniedException if the certificate is now valid, or refreshing is not allowed
	 */
	Certificate refresh(Certificate certificate, String source) throws AccessDeniedException;

	/**
	 * Return true if refreshing sessions is allowed
	 *
	 * @return true if refreshing sessions is allowed
	 */
	boolean isRefreshAllowed();

	/**
	 * Returns true if persisting on user data changed enabled
	 *
	 * @return true persisting on user data changed enabled
	 */
	boolean isPersistOnUserDataChanged();

	/**
	 * Invalidates the session for the given {@link Certificate}, effectively logging out the user who was authenticated
	 * with the credentials associated to the given {@link Certificate}
	 *
	 * @param certificate the {@link Certificate} for which the session is to be invalidated
	 *
	 * @return true if the session was still valid and is now invalidated, false otherwise
	 */
	boolean invalidate(Certificate certificate);

	/**
	 * Checks if the given {@link Certificate} is valid. This means that the certificate is for a valid session and that
	 * the user exists for the certificate. This method checks if the {@link Certificate} has been tampered with
	 * <p>
	 * Returns the {@link PrivilegeContext} for the given {@link Certificate}. The {@link PrivilegeContext} is an
	 * encapsulated state of a user's privileges so that for the duration of a user's call, the user can perform their
	 * actions and do not need to access the {@link PrivilegeHandler} anymore
	 *
	 * @param certificate the {@link Certificate} to check
	 *
	 * @return the {@link PrivilegeContext} for the given {@link Certificate}
	 *
	 * @throws PrivilegeException        if there is anything wrong with this certificate
	 * @throws NotAuthenticatedException if the certificate has expired
	 */
	PrivilegeContext validate(Certificate certificate) throws PrivilegeException;

	/**
	 * Checks if the given {@link PrivilegeContext} is valid. This means that the privilege context is for a valid
	 * system user session and that the user exists for the certificate. This method checks if the {@link Certificate}
	 * has been tampered with
	 *
	 * @param ctx the {@link PrivilegeContext} to check
	 *
	 * @throws PrivilegeException        if there is anything wrong with this privilege context
	 * @throws NotAuthenticatedException if the privilege context has expired
	 */
	void validateSystemSession(PrivilegeContext ctx) throws PrivilegeException;

	/**
	 * Checks if the given {@link Certificate} is valid. This means that the certificate is for a valid session and that
	 * the user exists for the certificate. This method checks if the {@link Certificate} has been tampered with
	 * <p>
	 * Returns the {@link PrivilegeContext} for the given {@link Certificate}. The {@link PrivilegeContext} is an
	 * encapsulated state of a user's privileges so that for the duration of a user's call, the user can perform their
	 * actions and do not need to access the {@link PrivilegeHandler} anymore
	 *
	 * @param certificate the {@link Certificate} to check
	 * @param source      the source, e.g. remote IP for this validation request
	 *
	 * @return the {@link PrivilegeContext} for the given {@link Certificate}
	 *
	 * @throws PrivilegeException        if there is anything wrong with this certificate
	 * @throws NotAuthenticatedException if the certificate has expired
	 */
	PrivilegeContext validate(Certificate certificate, String source) throws PrivilegeException;

	/**
	 * @see li.strolch.privilege.handler.PasswordStrengthHandler#validateStrength(char[])
	 */
	void validatePassword(Locale locale, char[] password) throws PasswordStrengthException;

	/**
	 * Starts the privilege handler, starting any threads and services which might be needed
	 */
	void start();

	/**
	 * Stops the privilege handler, starting any threads and services which might be needed
	 */
	void stop();

	/**
	 * <p>
	 * Informs this {@link PersistenceHandler} to reload the data from the backend
	 * </p>
	 *
	 * <b>Note:</b> It depends on the underlying {@link PersistenceHandler} implementation if data really is read
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param source      the source of the request
	 *
	 * @return true if the reload was successful, false if something went wrong
	 *
	 * @throws AccessDeniedException if the users of the given certificate does not have the privilege to perform this
	 *                               action
	 */
	boolean reload(Certificate certificate, String source);

	/**
	 * Persists any changes to the privilege data model. Changes are thus not persisted immediately, but must be
	 * actively performed
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 *
	 * @return true if changes were persisted, false if no changes were persisted
	 *
	 * @throws AccessDeniedException if the users of the given certificate does not have the privilege to perform this
	 *                               action
	 */
	boolean persist(Certificate certificate) throws AccessDeniedException;

	/**
	 * Persists all currently active sessions
	 *
	 * @param certificate the {@link Certificate} of the user which has the privilege to perform this action
	 * @param source      the source of the request
	 *
	 * @return true if changes were persisted, false if not (i.e. not enabled)
	 *
	 * @throws AccessDeniedException if the users of the given certificate does not have the privilege to perform this
	 *                               action
	 */
	boolean persistSessions(Certificate certificate, String source) throws AccessDeniedException;

	/**
	 * Special method to perform work as a System user, meaning the given systemUsername corresponds to an account which
	 * has the state {@link UserState#SYSTEM} and this user must have privilege to perform the concrete implementation
	 * of the given {@link SystemAction} instance
	 *
	 * @param systemUsername the username of the system user to perform the action as
	 * @param action         the action to be performed as the system user
	 *
	 * @throws PrivilegeException if the user does not exist, or the system action is not allowed
	 * @throws Exception          if anything else goes wrong during execution
	 */
	void runAs(String systemUsername, SystemAction action) throws PrivilegeException, Exception;

	/**
	 * Special method to perform work as a System user, meaning the given systemUsername corresponds to an account which
	 * has the state {@link UserState#SYSTEM} and this user must have privilege to perform the concrete implementation
	 * of the given {@link SystemAction} instance
	 *
	 * @param systemUsername the username of the system user to perform the action as
	 * @param action         the action to be performed as the system user
	 *
	 * @return the action
	 *
	 * @throws PrivilegeException if the user does not exist, or the system action is not allowed
	 * @throws Exception          if anything else goes wrong during execution
	 */
	<T> T runWithResult(String systemUsername, SystemActionWithResult<T> action) throws PrivilegeException, Exception;

	/**
	 * Special method to open a {@link PrivilegeContext} as a System user, meaning the given systemUsername corresponds
	 * to an account which has the state {@link UserState#SYSTEM}. This is used in cases where a system user's
	 * {@link PrivilegeContext} should be open for a longer period of time, or where opening many
	 * {@link PrivilegeContext} is resource intensive e.g. on low power devices.
	 *
	 * @param systemUsername the username of the system user to perform the action as
	 *
	 * @return the action
	 *
	 * @throws PrivilegeException if the user does not exist, or the system action is not allowed
	 */
	PrivilegeContext openSystemUserContext(String systemUsername) throws PrivilegeException;

	/**
	 * Returns the configuration for this {@link PrivilegeHandler}
	 *
	 * @return the configuration as a Map
	 */
	Map<String, String> getParameterMap();

	/**
	 * Returns the {@link EncryptionHandler} instance
	 *
	 * @return the {@link EncryptionHandler} instance
	 */
	EncryptionHandler getEncryptionHandler() throws PrivilegeException;

	/**
	 * Returns the {@link PersistenceHandler}
	 *
	 * @return the {@link PersistenceHandler}
	 */
	PersistenceHandler getPersistenceHandler();

	/**
	 * Returns the {@link SingleSignOnHandler}
	 *
	 * @return the {@link SingleSignOnHandler}
	 */
	SingleSignOnHandler getSsoHandler();

	/**
	 * Returns the {@link UserChallengeHandler}
	 *
	 * @return the {@link UserChallengeHandler}
	 */
	UserChallengeHandler getUserChallengeHandler();
}
