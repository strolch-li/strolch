/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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
import li.strolch.privilege.model.internal.*;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.privilege.xml.CertificateStubsSaxReader;
import li.strolch.privilege.xml.CertificateStubsSaxReader.CertificateStub;
import li.strolch.privilege.xml.CertificateStubsSaxWriter;
import li.strolch.utils.concurrent.ElementLockingHandler;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.AesCryptoHelper;
import li.strolch.utils.helper.AesCryptoHelper.SecretKeys;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXParseException;

import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.time.ZonedDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static java.text.MessageFormat.format;
import static java.util.stream.Collectors.toList;
import static li.strolch.privilege.handler.PrivilegeCrudHandler.clearPassword;
import static li.strolch.privilege.helper.ModelHelper.streamAllRolesForUser;
import static li.strolch.utils.helper.ExceptionHelper.getRootCause;
import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.helper.StringHelper.trimOrEmpty;

/**
 * <p>
 * This is default implementation of the {@link PrivilegeHandler}
 * </p>
 * <p>
 * The following list describes implementation details:
 * <ul>
 * <li>any methods which change the model are first validated by checking if the certificate has the appropriate
 * privilege</li>
 * <li>all model requests are delegated to the configured {@link PrivilegeHandler}, except for the session id to
 * {@link Certificate} map, no model data is kept in this implementation. This also means that to return the
 * representation objects, for every new model query, a new representation object is created</li>
 * <li>when creating new users, or editing users then a null password is understood as no password set</li>
 * <li>Password requirements are simple: Non null and non empty/length 0</li>
 * </ul>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultPrivilegeHandler implements PrivilegeHandler {

	protected static final Logger logger = LoggerFactory.getLogger(DefaultPrivilegeHandler.class);
	public static final String SOURCE_UNKNOWN = "unknown";
	protected PrivilegeCrudHandler crudHandler;

	/**
	 * Reference to all active sessions
	 */
	protected Map<String, PrivilegeContext> privilegeContextMap;

	/**
	 * Map of {@link PrivilegePolicy} classes
	 */
	protected Map<String, Class<PrivilegePolicy>> policyMap;

	/**
	 * The persistence handler is used for getting objects and saving changes
	 */
	protected PersistenceHandler persistenceHandler;

	/**
	 * The encryption handler is used for generating hashes and tokens
	 */
	protected EncryptionHandler encryptionHandler;

	/**
	 * The password strength handler is used for validating the strength of a password when being set
	 */
	protected PasswordStrengthHandler passwordStrengthHandler;

	/**
	 * The Single Sign On Handler
	 */
	protected SingleSignOnHandler ssoHandler;

	/**
	 * The {@link UserChallengeHandler} is used to challenge a user which tries to authenticate and/or change their
	 * password
	 */
	protected UserChallengeHandler userChallengeHandler;

	/**
	 * flag to define if already initialized
	 */
	protected boolean initialized;

	/**
	 * flag to define if a persist should be performed after a user changes their own data
	 */
	protected boolean autoPersistOnUserChangesData;

	/**
	 * flag to define if sessions should be persisted
	 */
	protected boolean persistSessions;

	/**
	 * Path to sessions file for persistence
	 */
	protected File persistSessionsPath;

	/**
	 * Secret key
	 */
	protected SecretKeys secretKey;

	/**
	 * flag if session refreshing is allowed
	 */
	protected boolean allowSessionRefresh;
	protected boolean allowPasswordReset;
	protected boolean disallowSourceChange;

	protected PrivilegeConflictResolution privilegeConflictResolution;

	protected Map<String, String> parameterMap;

	protected ElementLockingHandler<String> lockingHandler;
	protected Map<String, PersonalAccessTokenCacheEntry> personalAccessTokenCache;
	protected ScheduledExecutorService executorService;
	protected Future<?> persistSessionsTask;
	protected Future<?> persistModelTask;
	protected Future<?> prunePersonalAccessTokenCacheTask;

	@Override
	public SingleSignOnHandler getSsoHandler() {
		return this.ssoHandler;
	}

	@Override
	public UserChallengeHandler getUserChallengeHandler() {
		return this.userChallengeHandler;
	}

	@Override
	public PersistenceHandler getPersistenceHandler() {
		return this.persistenceHandler;
	}

	@Override
	public Map<String, String> getParameterMap() {
		return this.parameterMap;
	}

	@Override
	public boolean isRefreshAllowed() {
		return this.allowSessionRefresh;
	}

	@Override
	public boolean isPasswordResetAllowed() {
		return this.allowPasswordReset;
	}

	@Override
	public boolean isPersistOnUserDataChanged() {
		return this.autoPersistOnUserChangesData;
	}

	@Override
	public EncryptionHandler getEncryptionHandler() throws PrivilegeException {
		return this.encryptionHandler;
	}

	@Override
	public RoleRep getRole(Certificate certificate, String roleName) {
		return this.crudHandler.getRole(certificate, roleName);
	}

	@Override
	public Group getGroup(Certificate certificate, String groupName) {
		return this.crudHandler.getGroup(certificate, groupName);
	}

	@Override
	public UserRep getUser(Certificate certificate, String username) {
		return this.crudHandler.getUserRep(certificate, username);
	}

	@Override
	public UserRep getUserById(Certificate certificate, String userId) {
		return this.crudHandler.getUserRepById(certificate, userId);
	}

	@Override
	public UserPrivileges getUserPrivileges(Certificate certificate, String username) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_GET_USER_PRIVILEGES);

		User user = this.crudHandler.getUser(certificate, username);
		if (user == null)
			throw new PrivilegeModelException(format("User {0} does not exist!", username));
		return getPrivilegeContextBuilder().buildUserPrivilege(user);
	}

	@Override
	public UserPrivileges getUserPrivilegesById(Certificate certificate, String userId) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_GET_USER_PRIVILEGES);

		User user = this.crudHandler.getUserById(certificate, userId);
		if (user == null)
			throw new PrivilegeModelException(format("User with ID {0} does not exist!", userId));
		return getPrivilegeContextBuilder().buildUserPrivilege(user);
	}

	@Override
	public GroupPrivileges getGroupPrivileges(Certificate certificate, String groupName) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.assertHasPrivilege(PRIVILEGE_GET_GROUP_PRIVILEGES);

		Group group = this.crudHandler.getGroup(certificate, groupName);
		if (group == null)
			throw new PrivilegeModelException(format("Group {0} does not exist!", groupName));
		return getPrivilegeContextBuilder().buildGroupPrivilege(group);
	}

	@Override
	public Map<String, String> getPolicyDefs(Certificate certificate) {
		return this.crudHandler.getPolicyDefs(certificate);
	}

	@Override
	public List<Certificate> getCertificates(Certificate certificate) {

		// validate user actually has this type of privilege
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_GET_CERTIFICATES));

		return this.privilegeContextMap.values().stream().map(PrivilegeContext::getCertificate).collect(toList());
	}

	@Override
	public List<RoleRep> getRoles(Certificate certificate) {
		return this.crudHandler.getRoles(certificate);
	}

	@Override
	public List<UserRep> getUsers(Certificate certificate) {
		return this.crudHandler.getUsers(certificate);
	}

	@Override
	public List<UserRep> queryUsers(Certificate certificate, UserRep selectorRep) {
		return this.crudHandler.queryUsers(certificate, selectorRep);
	}

	@Override
	public UserRep addUser(Certificate certificate, UserRep userRepParam, char[] password) {
		return this.lockingHandler.lockedExecuteWithResult(userRepParam.getUsername(),
				() -> this.crudHandler.addUser(certificate, userRepParam, password));
	}

	@Override
	public void addOrUpdateUsers(Certificate certificate, List<UserRep> userReps) throws PrivilegeException {
		this.lockingHandler.lockedExecute(PrivilegeHandler.class.getSimpleName(),
				() -> this.crudHandler.addOrUpdateUsers(certificate, userReps));
	}

	@Override
	public UserRep updateUser(Certificate certificate, UserRep userRep, char[] password) throws PrivilegeException {
		return this.lockingHandler.lockedExecuteWithResult(userRep.getUsername(),
				() -> this.crudHandler.updateUser(certificate, userRep, password));
	}

	@Override
	public UserRep removeUser(Certificate certificate, String username) {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> this.crudHandler.removeUser(certificate, username));
	}

	@Override
	public UserRep removeUserById(Certificate certificate, String userId) throws PrivilegeException {
		return this.lockingHandler.lockedExecuteWithResult(userId,
				() -> this.crudHandler.removeUserById(certificate, userId));
	}

	@Override
	public UserRep setUserLocale(Certificate certificate, String username, Locale locale) {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> this.crudHandler.setUserLocale(certificate, username, locale));
	}

	@Override
	public UserRep setUserLocaleById(Certificate certificate, String userId, Locale locale) throws PrivilegeException {
		return this.lockingHandler.lockedExecuteWithResult(userId,
				() -> this.crudHandler.setUserLocaleById(certificate, userId, locale));
	}

	@Override
	public UserRep requirePasswordChange(Certificate certificate, String username) throws PrivilegeException {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> this.crudHandler.requirePasswordChange(certificate, username));
	}

	@Override
	public UserRep requirePasswordChangeById(Certificate certificate, String userId) throws PrivilegeException {
		return this.lockingHandler.lockedExecuteWithResult(userId,
				() -> this.crudHandler.requirePasswordChangeById(certificate, userId));
	}

	@Override
	public UserRep setUserPassword(Certificate certificate, String username, char[] password) {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> this.crudHandler.setUserPassword(certificate, username, password));
	}

	@Override
	public UserRep setUserPasswordById(Certificate certificate, String userId, char[] password)
			throws PrivilegeException {
		return this.lockingHandler.lockedExecuteWithResult(userId,
				() -> this.crudHandler.setUserPasswordById(certificate, userId, password));
	}

	@Override
	public UserRep setUserState(Certificate certificate, String username, UserState state) {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> this.crudHandler.setUserState(certificate, username, state));
	}

	@Override
	public UserRep setUserStateById(Certificate certificate, String userId, UserState state) throws PrivilegeException {
		return this.lockingHandler.lockedExecuteWithResult(userId,
				() -> this.crudHandler.setUserStateById(certificate, userId, state));
	}

	@Override
	public RoleRep addRole(Certificate certificate, RoleRep roleRep) {
		return this.lockingHandler.lockedExecuteWithResult(roleRep.getName(),
				() -> this.crudHandler.addRole(certificate, roleRep));
	}

	@Override
	public RoleRep replaceRole(Certificate certificate, RoleRep roleRep) {
		return this.lockingHandler.lockedExecuteWithResult(roleRep.getName(),
				() -> this.crudHandler.replaceRole(certificate, roleRep));
	}

	@Override
	public RoleRep removeRole(Certificate certificate, String roleName) {
		return this.lockingHandler.lockedExecuteWithResult(roleName,
				() -> this.crudHandler.removeRole(certificate, roleName));
	}

	@Override
	public List<Group> getGroups(Certificate certificate) {
		return this.crudHandler.getGroups(certificate);
	}

	@Override
	public Group addGroup(Certificate certificate, Group group) {
		return this.lockingHandler.lockedExecuteWithResult(group.name(),
				() -> this.crudHandler.addGroup(certificate, group));
	}

	@Override
	public Group replaceGroup(Certificate certificate, Group group) {
		return this.lockingHandler.lockedExecuteWithResult(group.name(),
				() -> this.crudHandler.replaceGroup(certificate, group));
	}

	@Override
	public Group removeGroup(Certificate certificate, String groupName) {
		return this.lockingHandler.lockedExecuteWithResult(groupName,
				() -> this.crudHandler.removeGroup(certificate, groupName));
	}

	@Override
	public void initiateChallengeFor(Usage usage, String username) {
		assertPasswordResetAllowed(usage);
		initiateChallengeFor(usage, username, SOURCE_UNKNOWN);
	}

	@Override
	public void initiateChallengeFor(Usage usage, String username, String source) {
		assertPasswordResetAllowed(usage);
		this.lockingHandler.lockedExecute(username, () -> internalInitiateChallengeFor(usage, username, source));
	}

	protected void internalInitiateChallengeFor(Usage usage, String username, String source) {
		DBC.PRE.assertNotEmpty("source must not be empty!", source);

		assertPasswordResetAllowed(usage);

		// get User
		User user = this.persistenceHandler.getUser(username);
		if (user == null)
			throw new PrivilegeModelException(format("User {0} does not exist!", username));

		// initiate the challenge
		this.userChallengeHandler.initiateChallengeFor(usage, user, source);

		logger.info("Initiated Challenge for {} with usage {}", username, usage);
	}

	private void assertPasswordResetAllowed(Usage usage) {
		if (usage == Usage.SET_PASSWORD && !this.allowPasswordReset)
			throw new AccessDeniedException("Resetting password not allowed!");
	}

	@Override
	public Certificate validateChallenge(String username, String challenge) throws PrivilegeException {
		return validateChallenge(username, challenge, "unknown");
	}

	@Override
	public Certificate validateChallenge(String username, String challenge, String source) throws PrivilegeException {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> internalValidateChallenge(username, challenge, source));
	}

	protected Certificate internalValidateChallenge(String username, String challenge, String source)
			throws PrivilegeException {
		DBC.PRE.assertNotEmpty("source must not be empty!", source);

		// get User
		User user = this.persistenceHandler.getUser(username);
		if (user == null) {
			throw new PrivilegeModelException(format("User {0} does not exist!", username));
		}

		// validate the response
		UserChallenge userChallenge = this.userChallengeHandler.validateResponse(user, challenge);

		// initialize a new privilege context
		Usage usage = userChallenge.getUsage();
		Certificate certificate = buildPrivilegeContext(usage, user, userChallenge.getSource(), ZonedDateTime.now(),
				false).getCertificate();

		if (!source.equals("unknown") && !source.equals(userChallenge.getSource())) {
			logger.warn("Challenge request and response source''s are different: request: {} to {}",
					userChallenge.getSource(), source);
		}

		persistSessionsAsync();

		logger.info("Challenge validated for user {} with usage {}", username, usage);
		return certificate;
	}

	@Override
	public List<PersonalAccessTokenRep> getPersonalAccessTokens(Certificate certificate) {
		return this.crudHandler.getPersonalAccessTokens(certificate, certificate.getUsername());
	}

	@Override
	public List<PersonalAccessTokenRep> getPersonalAccessTokens(Certificate certificate, String username) {
		return this.crudHandler.getPersonalAccessTokens(certificate, username);
	}

	@Override
	public String createPersonalAccessToken(Certificate certificate, String name, ZonedDateTime validFrom,
			ZonedDateTime validTo, Set<String> roles, List<Privilege> privileges) {
		return this.crudHandler.createPersonalAccessToken(certificate, name, validFrom, validTo, roles, privileges);
	}

	@Override
	public void removePersonalAccessToken(Certificate certificate, String tokenId) {
		this.crudHandler.removePersonalAccessToken(certificate, tokenId);
	}

	@Override
	public Certificate authenticatePersonalAccessToken(String token, String source) throws AccessDeniedException {
		DBC.PRE.assertNotEmpty("token", token);
		DBC.PRE.assertNotEmpty("source", source);

		// The token is expected to be in the format: tokenId:tokenValue
		String[] parts = token.split(":", 2);
		if (parts.length != 2)
			throw new AccessDeniedException("Invalid personal access token format!");

		String tokenId = parts[0];
		String tokenValue = parts[1];

		// check cache
		PersonalAccessTokenCacheEntry cachedEntry = this.personalAccessTokenCache.get(tokenId);
		if (cachedEntry != null) {
			// validate token still exists and is valid
			PersonalAccessToken pat = this.persistenceHandler.getAccessToken(tokenId);
			if (pat != null && pat.validFrom().isBefore(ZonedDateTime.now()) && pat
					.validTo()
					.isAfter(ZonedDateTime.now())) {
				// validate user still exists and is enabled
				User user = this.persistenceHandler.getUser(pat.username());
				if (user != null && user.getUserState() == UserState.ENABLED) {
					// Cache hit and valid
					cachedEntry.lastAccess = System.currentTimeMillis();
					return cachedEntry.context.getCertificate();
				}
			}

			// Cache is invalid or expired
			this.personalAccessTokenCache.remove(tokenId);
		}

		// tokens are stored as hashed PasswordCrypt
		PersonalAccessToken pat = this.persistenceHandler.getAccessToken(tokenId);
		if (pat == null)
			throw new AccessDeniedException("Invalid personal access token!");

		PasswordCrypt passwordCrypt = pat.passwordCrypt();
		try {
			PasswordCrypt hashedToken = this.encryptionHandler.hashPassword(tokenValue.toCharArray(),
					passwordCrypt.salt(), passwordCrypt.hashAlgorithm(), passwordCrypt.hashIterations(),
					passwordCrypt.hashKeyLength());
			if (!Arrays.equals(passwordCrypt.password(), hashedToken.password())) {
				throw new AccessDeniedException("Invalid personal access token!");
			}
		} catch (AccessDeniedException e) {
			throw e;
		} catch (Exception e) {
			logger.error("Failed to hash token for comparison", e);
			throw new AccessDeniedException("Failed to authenticate personal access token!");
		}

		if (pat.validFrom().isAfter(ZonedDateTime.now()) || pat.validTo().isBefore(ZonedDateTime.now()))
			throw new AccessDeniedException("Personal access token is expired or not yet valid!");

		User user = this.persistenceHandler.getUser(pat.username());
		if (user == null)
			throw new AccessDeniedException("User " + pat.username() + " does not exist anymore!");

		if (user.getUserState() != UserState.ENABLED)
			throw new AccessDeniedException("User " + pat.username() + " is " + user.getUserState());

		// update last used
		PersonalAccessToken updatedToken = pat.withLastUsed(ZonedDateTime.now());
		this.persistenceHandler.removeAccessToken(pat.tokenId());
		this.persistenceHandler.addAccessToken(updatedToken);

		// Build context
		PrivilegeContext prvCtx = getPrivilegeContextBuilder().buildPrivilegeContext(updatedToken, user, source,
				ZonedDateTime.now());
		this.privilegeContextMap.put(prvCtx.getCertificate().getSessionId(), prvCtx);
		this.personalAccessTokenCache.put(tokenId, new PersonalAccessTokenCacheEntry(prvCtx));

		return prvCtx.getCertificate();
	}

	@Override
	public Certificate authenticate(String username, char[] password, boolean keepAlive) {
		return authenticate(username, password, "unknown", Usage.ANY, keepAlive);
	}

	@Override
	public Certificate authenticate(String username, char[] password, String source, Usage usage, boolean keepAlive) {
		return this.lockingHandler.lockedExecuteWithResult(username,
				() -> internalAuthenticate(username, password, source, usage, keepAlive));
	}

	protected Certificate internalAuthenticate(String username, char[] password, String source, Usage usage,
			boolean keepAlive) {
		DBC.PRE.assertNotEmpty("source must not be empty!", source);

		// we don't want the user to worry about whitespace
		username = trimOrEmpty(username);

		try {
			// username must be at least 2 characters in length
			if (username.length() < 2) {
				String msg = format("The given username ''{0}'' is shorter than 2 characters", username);
				throw new InvalidCredentialsException(msg);
			}

			// check the password
			User user = checkCredentialsAndUserState(username, password);

			// validate user has at least one role
			if (streamAllRolesForUser(this.persistenceHandler, user).findAny().isEmpty())
				throw new AccessDeniedException(
						format("User {0} does not have any groups or roles defined!", username));

			if (user.isPasswordChangeRequested()) {
				if (usage == Usage.SINGLE)
					throw new IllegalStateException("Password change requested!");
				usage = Usage.SET_PASSWORD;
			}

			// initialize a new privilege context
			Certificate certificate = buildPrivilegeContext(usage, user, source, ZonedDateTime.now(),
					keepAlive).getCertificate();

			switch (usage) {
				case ANY -> {
					persistSessionsAsync();
					logger.info("User {} authenticated with password: {}", username, certificate);
				}
				case SINGLE ->
						logger.info("User {} authenticated with basic auth/sigle use: {}", username, certificate);
				case SET_PASSWORD ->
						logger.info("User {} authenticated for SetPassword use: {}", username, certificate);
				default -> throw new IllegalStateException("Unexpected usage: " + usage);
			}

			// save last login
			UserHistory history = user.getHistory().withLastLogin(ZonedDateTime.now());
			if (history.isFirstLoginEmpty())
				history = history.withFirstLogin(ZonedDateTime.now());

			if (this.persistenceHandler.hasUser(user.getUsername())) {
				// we replace the user
				this.persistenceHandler.replaceUser(user.withHistory(history));
			} else {
				// for remote, the user won't exist, if it is the user's first login
				this.persistenceHandler.addUser(user.withHistory(history));
			}
			persistModelAsync();

			// return the certificate
			return certificate;

		} catch (PrivilegeException e) {
			throw e;
		} catch (RuntimeException e) {
			logger.error(e.getMessage(), e);
			String msg = "User {0} failed to authenticate: {1}";
			msg = format(msg, username, e.getMessage());
			throw new PrivilegeException(msg, e);
		} finally {
			clearPassword(password);
		}
	}

	@Override
	public Certificate authenticateSingleSignOn(Object data, boolean keepAlive) throws PrivilegeException {
		return authenticateSingleSignOn(data, "unknown", keepAlive);
	}

	@Override
	public Certificate authenticateSingleSignOn(Object data, String source, boolean keepAlive) {
		DBC.PRE.assertNotEmpty("source must not be empty!", source);
		if (this.ssoHandler == null)
			throw new IllegalStateException("The SSO Handler is not configured!");

		User user = this.ssoHandler.authenticateSingleSignOn(data);

		return this.lockingHandler.lockedExecuteWithResult(user.getUsername(),
				() -> internalAuthenticateSingleSignOn(user, source, keepAlive));
	}

	protected Certificate internalAuthenticateSingleSignOn(User user, String source, boolean keepAlive)
			throws PrivilegeException {

		DBC.PRE.assertEquals("SSO Users must have UserState.REMOTE!", UserState.REMOTE, user.getUserState());
		UserHistory history = user.getHistory();
		history = history.withLastLogin(ZonedDateTime.now());

		// persist this user
		User internalUser = this.persistenceHandler.getUser(user.getUsername());
		if (internalUser == null) {

			// from single-sign-on we don't necessarily have a user ID, so if it is null, update it
			if (user.getUserId() == null)
				user = user.withUserId(UUID.randomUUID().toString());

			history = history.withFirstLogin(ZonedDateTime.now());
			this.persistenceHandler.addUser(user.withHistory(history));
		} else {

			// from single-sign-on we don't necessarily have a user ID, so if it is null, update it
			if (user.getUserId() == null)
				user = user.withUserId(internalUser.getUserId());

			user = copyCustomProperties(user, internalUser);

			history = history.withFirstLogin(internalUser.getHistory().getFirstLogin());
			this.persistenceHandler.replaceUser(user.withHistory(history));
		}
		persistModelAsync();

		// initialize a new privilege context
		Certificate certificate = buildPrivilegeContext(Usage.ANY, user, source, ZonedDateTime.now(),
				keepAlive).getCertificate();

		persistSessionsAsync();

		// log
		logger.info("User {} authenticated with single sign on: {}", user.getUsername(), certificate);

		return certificate;
	}

	protected User copyCustomProperties(User user, User internalUser) {
		Map<String, String> properties = new HashMap<>();
		for (String key : internalUser.getPropertyKeySet()) {
			if (!user.hasProperty(key))
				properties.put(key, internalUser.getProperty(key));
		}

		return user.withAdditionalProperties(properties);
	}

	@Override
	public Certificate refresh(Certificate certificate, String source) throws AccessDeniedException {
		return this.lockingHandler.lockedExecuteWithResult(certificate.getUsername(),
				() -> internalRefresh(certificate, source));
	}

	protected Certificate internalRefresh(Certificate certificate, String source) throws AccessDeniedException {
		DBC.PRE.assertNotNull("certificate must not be null!", certificate);

		try {
			// username must be at least 2 characters in length
			if (!this.allowSessionRefresh)
				throw new AccessDeniedException("Refreshing of sessions not allowed!");

			validate(certificate);

			if (!certificate.isKeepAlive())
				throw new AccessDeniedException("Refreshing of session not allowed!");

			if (!certificate.getSource().equals(source)) {
				logger.error("Source of existing session {} is not the same as the refresh request's source {}",
						certificate.getSource(), source);
			}

			// check the password
			User user = this.persistenceHandler.getUser(certificate.getUsername());

			// initialize a new privilege context
			PrivilegeContext refreshedContext = buildPrivilegeContext(certificate.getUsage(), user, source,
					ZonedDateTime.now(), true);

			// invalidate the previous session
			invalidate(certificate);

			// log
			Certificate refreshedCertificate = refreshedContext.getCertificate();
			logger.info("User {} refreshed session: {}", user.getUsername(), refreshedCertificate);

			// return the certificate
			return refreshedCertificate;

		} catch (PrivilegeException e) {
			throw e;
		} catch (RuntimeException e) {
			logger.error(e.getMessage(), e);
			String msg = "User {0} failed to refresh session: {1}";
			msg = format(msg, certificate.getUsername(), e.getMessage());
			throw new PrivilegeException(msg, e);
		}
	}

	protected synchronized boolean persistSessionsAsync() {
		if (!this.persistSessions)
			return false;

		// async execution, max. once per second
		if (this.persistSessionsTask != null)
			this.persistSessionsTask.cancel(true);
		this.persistSessionsTask = this.executorService.schedule(this::internalPersistSessions, 1, TimeUnit.SECONDS);
		return true;
	}

	protected void internalPersistSessions() {
		// get sessions reference
		AtomicReference<List<Certificate>> sessions = new AtomicReference<>();
		this.lockingHandler.lockedExecute("persist-sessions", () -> sessions.set(
				new ArrayList<>(this.privilegeContextMap.values())
						.stream()
						.map(PrivilegeContext::getCertificate)
						.filter(c -> c.getUsage().isAny())
						.filter(c -> !c.getUserState().isSystem())
						.collect(toList())));

		// write the sessions
		try (OutputStream out = Files.newOutputStream(this.persistSessionsPath.toPath());
		     OutputStream outputStream = AesCryptoHelper.wrapEncrypt(this.secretKey, out)) {

			CertificateStubsSaxWriter writer = new CertificateStubsSaxWriter(sessions.get(), outputStream);
			writer.write();
			outputStream.flush();

		} catch (Exception e) {
			logger.error("Failed to persist sessions!", e);
			if (this.persistSessionsPath.exists() && !this.persistSessionsPath.delete()) {
				logger.error("Failed to delete sessions file after failing to write to it, at {}",
						this.persistSessionsPath.getAbsolutePath());
			}
		}
	}

	protected void loadSessions() {
		if (!this.persistSessions) {
			logger.info("Persisting of sessions not enabled, so not loading!.");
			return;
		}

		if (!this.persistSessionsPath.exists()) {
			logger.info("Sessions file does not exist");
			return;
		}

		if (!this.persistSessionsPath.isFile())
			throw new PrivilegeModelException(
					"Sessions data file is not a file but exists at " + this.persistSessionsPath.getAbsolutePath());

		List<CertificateStub> certificateStubs;
		try (InputStream fin = Files.newInputStream(this.persistSessionsPath.toPath());
		     InputStream inputStream = AesCryptoHelper.wrapDecrypt(this.secretKey, fin)) {

			CertificateStubsSaxReader reader = new CertificateStubsSaxReader(inputStream);
			certificateStubs = reader.read();

		} catch (Exception e) {
			if (getRootCause(e) instanceof SAXParseException)
				logger.error("Failed to load sessions: {}", getRootCause(e).getMessage());
			else
				logger.error("Failed to load sessions!", e);
			if (!this.persistSessionsPath.delete())
				logger.error("Failed to delete session file at {}", this.persistSessionsPath.getAbsolutePath());
			return;
		}

		if (certificateStubs.isEmpty()) {
			logger.info("No persisted sessions exist to be loaded.");
			return;
		}

		for (CertificateStub stub : certificateStubs) {
			String username = stub.getUsername();
			User user = this.persistenceHandler.getUser(username);
			if (user == null) {
				logger.error("Ignoring session data for missing user {}", username);
				continue;
			}

			if (user.getUserState() == UserState.DISABLED || user.getUserState() == UserState.EXPIRED) {
				logger.error("Ignoring session data for disabled/expired user {}", username);
				continue;
			}

			if (streamAllRolesForUser(this.persistenceHandler, user).findAny().isEmpty()) {
				logger.error("Ignoring session data for user {} which has no roles or groups defined!", username);
				continue;
			}

			// initialize a new privilege context
			buildPrivilegeContext(user, stub);
		}

		logger.info("Loaded {} sessions.", this.privilegeContextMap.size());
	}

	/**
	 * Checks the credentials and validates that the user may log in.
	 *
	 * @param username the username of the {@link User} to check against
	 * @param password the password of this user
	 *
	 * @return the {@link User} if the credentials are valid and the user may login
	 *
	 * @throws AccessDeniedException       if anything is wrong with the credentials or the user state
	 * @throws InvalidCredentialsException if the given credentials are invalid, the user does not exist, or has no
	 *                                     password set
	 */
	protected User checkCredentialsAndUserState(String username, char[] password)
			throws InvalidCredentialsException, AccessDeniedException {

		// and validate the password
		if (password == null || password.length < 3)
			throw new InvalidCredentialsException("Password is invalid!");

		// get user object
		User user = this.persistenceHandler.getUser(username);
		// no user means no authentication
		if (user == null) {
			String msg = format("There is no user defined with the username {0}", username);
			throw new InvalidCredentialsException(msg);
		}

		// make sure not a system user - they may not login
		if (user.getUserState() == UserState.SYSTEM) {
			String msg = "User {0} is a system user and may not login!";
			msg = format(msg, username);
			throw new AccessDeniedException(msg);
		}

		// validate if user is allowed to login
		// this also capture the trying to login of SYSTEM user
		if (user.getUserState() != UserState.ENABLED) {
			String msg = "User {0} does not have state {1} and can not login!";
			msg = format(msg, username, UserState.ENABLED);
			throw new AccessDeniedException(msg);
		}

		PasswordCrypt userPasswordCrypt = user.getPasswordCrypt();
		if (userPasswordCrypt == null || userPasswordCrypt.password() == null)
			throw new InvalidCredentialsException(format("User {0} has no password and may not login!", username));

		// we only work with hashed passwords
		PasswordCrypt requestPasswordCrypt;
		if (userPasswordCrypt.salt() == null) {
			requestPasswordCrypt = this.encryptionHandler.hashPasswordWithoutSalt(password);
		} else if (userPasswordCrypt.hashAlgorithm() == null
				|| userPasswordCrypt.hashIterations() == -1
				|| userPasswordCrypt.hashKeyLength() == -1) {
			requestPasswordCrypt = this.encryptionHandler.hashPassword(password, userPasswordCrypt.salt());
		} else {
			requestPasswordCrypt = this.encryptionHandler.hashPassword(password, userPasswordCrypt.salt(),
					userPasswordCrypt.hashAlgorithm(), userPasswordCrypt.hashIterations(),
					userPasswordCrypt.hashKeyLength());
		}

		// validate password
		if (!Arrays.equals(requestPasswordCrypt.password(), userPasswordCrypt.password()))
			throw new InvalidCredentialsException(format("Password is incorrect for {0}", username));

		// see if we need to update the hash
		if (this.encryptionHandler.isPasswordCryptOutdated(userPasswordCrypt)) {

			logger.warn("Updating user {} due to change in hashing algorithm properties ", username);

			// get new salt for user
			byte[] salt = this.encryptionHandler.nextSalt();

			// hash password
			PasswordCrypt newPasswordCrypt = this.encryptionHandler.hashPassword(password, salt);

			// create new user
			user = user.withPasswordCrypt(newPasswordCrypt);

			// delegate user replacement to persistence handler
			this.persistenceHandler.replaceUser(user);
			persistModelAsync();

			logger.info("Updated password for {}", user.getUsername());
		}

		return user;
	}

	@Override
	public boolean invalidate(Certificate certificate) {

		// remove registration
		PrivilegeContext privilegeContext = this.privilegeContextMap.remove(certificate.getSessionId());
		if (privilegeContext == null)
			return false;

		// persist sessions
		if (privilegeContext.getCertificate().getUsage().isAny())
			persistSessionsAsync();

		// return true if object was really removed
		if (certificate.getUsage().isAny())
			logger.info("User {} logged out.", certificate.getUsername());

		return true;
	}

	@Override
	public PrivilegeContext validate(Certificate certificate) throws PrivilegeException {
		return validate(certificate, "unknown");
	}

	@Override
	public void validateSystemSession(PrivilegeContext ctx) throws PrivilegeException {
		// ctx  must not be null
		if (ctx == null)
			throw new PrivilegeException("PrivilegeContext may not be null!");

		// validate user state is system
		if (ctx.getUserState() != UserState.SYSTEM) {
			String msg = "The PrivilegeContext user {0} does not have expected user state {1}";
			msg = format(msg, ctx.getUsername(), UserState.SYSTEM);
			throw new PrivilegeException(msg);
		}

		// see if a session exists for this certificate
		Certificate certificate = ctx.getCertificate();
		PrivilegeContext privilegeContext = this.privilegeContextMap.get(certificate.getSessionId());
		if (privilegeContext == null) {
			String msg = format("There is no session information for {0}", certificate);
			throw new NotAuthenticatedException(msg);
		}

		// validate same privilege contexts
		if (ctx != privilegeContext) {
			String msg = format("The given PrivilegeContext {0} is not the same as registered under the sessionId {1}",
					ctx.getCertificate().getSessionId(), privilegeContext.getCertificate().getSessionId());
			throw new PrivilegeException(msg);
		}

		certificate.setLastAccess(ZonedDateTime.now());
	}

	@Override
	public PrivilegeContext validate(Certificate certificate, String source) throws PrivilegeException {
		DBC.PRE.assertNotEmpty("source must not be empty!", source);

		// certificate  must not be null
		if (certificate == null)
			throw new PrivilegeException("Certificate may not be null!");

		// first see if a session exists for this certificate
		PrivilegeContext privilegeContext = this.privilegeContextMap.get(certificate.getSessionId());
		if (privilegeContext == null) {
			String msg = format("There is no session information for {0}", certificate);
			throw new NotAuthenticatedException(msg);
		}

		// validate certificate has not been tampered with
		Certificate sessionCertificate = privilegeContext.getCertificate();
		if (!sessionCertificate.equals(certificate)) {
			String msg = "Received illegal certificate for session id {0}";
			msg = format(msg, certificate.getSessionId());
			throw new PrivilegeException(msg);
		}

		// validate that challenge certificate is not expired (1 hour only) 
		if (sessionCertificate.getUsage() != Usage.ANY) {
			ZonedDateTime dateTime = sessionCertificate.getLoginTime();
			if (dateTime.plusHours(1).isBefore(ZonedDateTime.now())) {
				invalidate(sessionCertificate);
				throw new NotAuthenticatedException("Certificate has already expired!");
			}
		}

		certificate.setLastAccess(ZonedDateTime.now());

		// assert source did not change
		if (this.disallowSourceChange && !source.equals(SOURCE_UNKNOWN) && !certificate.getSource().equals(source)) {
			invalidate(certificate);
			throw new NotAuthenticatedException("Source has changed for certificate " + certificate + " to " + source);
		}

		return privilegeContext;
	}

	@Override
	public void validatePassword(Locale locale, char[] password) throws PasswordStrengthException {
		if (!this.passwordStrengthHandler.validateStrength(password))
			throw new PasswordStrengthException(this.passwordStrengthHandler.getDescription(locale));
	}

	@Override
	public boolean persist(Certificate certificate) {

		// validate who is doing this
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_PERSIST));

		// persist non async
		try {
			return this.persistenceHandler.persist();
		} catch (XMLStreamException | IOException e) {
			throw new IllegalStateException("Failed to persist model", e);
		}
	}

	protected synchronized void persistModelAsync() {
		if (!this.autoPersistOnUserChangesData)
			return;

		// async execution, max. once per second
		if (this.persistModelTask != null)
			this.persistModelTask.cancel(true);
		this.persistModelTask = this.executorService.schedule(
				() -> this.lockingHandler.lockedExecute("persist-model", () -> {
					try {
						this.persistenceHandler.persist();
					} catch (Exception e) {
						logger.error("Failed to persist model!", e);
					}
				}), 1, TimeUnit.SECONDS);
	}

	@Override
	public boolean persistSessions(Certificate certificate, String source) {

		// validate who is doing this
		PrivilegeContext prvCtx = validate(certificate);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_PERSIST_SESSIONS));

		return persistSessionsAsync();
	}

	@Override
	public void start() {
		this.lockingHandler.start();

		this.prunePersonalAccessTokenCacheTask = this.executorService.scheduleAtFixedRate(
				this::prunePersonalAccessTokenCache, 1, 1, TimeUnit.MINUTES);
	}

	protected void prunePersonalAccessTokenCache() {
		long now = System.currentTimeMillis();
		long tenMinutesAgo = now - TimeUnit.MINUTES.toMillis(10);
		this.personalAccessTokenCache.entrySet().removeIf(entry -> entry.getValue().lastAccess < tenMinutesAgo);
	}

	@Override
	public void stop() {
		this.lockingHandler.stop();

		if (this.prunePersonalAccessTokenCacheTask != null) {
			this.prunePersonalAccessTokenCacheTask.cancel(true);
			this.prunePersonalAccessTokenCacheTask = null;
		}
	}

	@Override
	public boolean reload(Certificate certificate, String source) {

		// validate who is doing this
		PrivilegeContext prvCtx = validate(certificate, source);
		prvCtx.validateAction(new SimpleRestrictable(PRIVILEGE_ACTION, PRIVILEGE_ACTION_RELOAD));

		return this.persistenceHandler.reload();
	}

	/**
	 * Initializes the concrete {@link PrivilegeHandler}. The passed parameter map contains any configuration this
	 * {@link PrivilegeHandler} might need. This method may only be called once and this must be enforced by the
	 * concrete implementation
	 *
	 * @param parameterMap            a map containing configuration properties
	 * @param encryptionHandler       the {@link EncryptionHandler} instance for this {@link PrivilegeHandler}
	 * @param passwordStrengthHandler the {@link PasswordStrengthHandler} instance for this {@link PrivilegeHandler}
	 * @param persistenceHandler      the {@link PersistenceHandler} instance for this {@link PrivilegeHandler}
	 * @param userChallengeHandler    the handler to challenge a user's actions e.g. password change or authentication
	 * @param ssoHandler              the {@link SingleSignOnHandler}
	 * @param policyMap               map of {@link PrivilegePolicy} classes
	 *
	 * @throws PrivilegeException if this method is called multiple times or an initialization exception occurs
	 */
	public void initialize(ScheduledExecutorService executorService, Map<String, String> parameterMap,
			EncryptionHandler encryptionHandler, PasswordStrengthHandler passwordStrengthHandler,
			PersistenceHandler persistenceHandler, UserChallengeHandler userChallengeHandler,
			SingleSignOnHandler ssoHandler, Map<String, Class<PrivilegePolicy>> policyMap) {

		if (this.initialized)
			throw new PrivilegeModelException("Already initialized!");

		this.executorService = executorService;
		this.lockingHandler = new ElementLockingHandler<>(executorService, TimeUnit.SECONDS, 10L);
		this.policyMap = Map.copyOf(policyMap);
		this.encryptionHandler = encryptionHandler;
		this.passwordStrengthHandler = passwordStrengthHandler;
		this.persistenceHandler = persistenceHandler;
		this.userChallengeHandler = userChallengeHandler;
		this.ssoHandler = ssoHandler;

		handleAutoPersistOnUserDataChange(parameterMap);
		handlePersistSessionsParam(parameterMap);
		handleConflictResolutionParam(parameterMap);
		handleSecretParams(parameterMap);

		this.allowSessionRefresh = Boolean.parseBoolean(parameterMap.get(PARAM_ALLOW_SESSION_REFRESH));
		this.allowPasswordReset = Boolean.parseBoolean(parameterMap.get(PARAM_ALLOW_PASSWORD_RESET));
		this.disallowSourceChange = Boolean.parseBoolean(parameterMap.get(PARAM_DISALLOW_SOURCE_CHANGE));

		this.crudHandler = new PrivilegeCrudHandler(this, this.policyMap, this.privilegeConflictResolution);

		// validate policies on privileges of Roles
		for (Role role : persistenceHandler.getAllRoles()) {
			this.crudHandler.validatePolicies(role);
		}

		// validate privilege conflicts
		validatePrivilegeConflicts();

		this.privilegeContextMap = new ConcurrentHashMap<>();
		this.personalAccessTokenCache = new ConcurrentHashMap<>();

		loadSessions();

		this.parameterMap = parameterMap;
		this.initialized = true;
	}

	protected void handleAutoPersistOnUserDataChange(Map<String, String> parameterMap) {
		String autoPersistS = parameterMap.get(PARAM_AUTO_PERSIST_ON_USER_CHANGES_DATA);
		if (isEmpty(autoPersistS) || autoPersistS.equals(Boolean.FALSE.toString())) {
			this.autoPersistOnUserChangesData = false;
		} else if (autoPersistS.equals(Boolean.TRUE.toString())) {
			this.autoPersistOnUserChangesData = true;
			logger.info("Enabling automatic persistence when user changes their data.");
		} else {
			String msg = "Parameter {0} has illegal value {1}. Overriding with {2}";
			msg = format(msg, PARAM_AUTO_PERSIST_ON_USER_CHANGES_DATA, autoPersistS, Boolean.FALSE);
			logger.error(msg);
			this.autoPersistOnUserChangesData = false;
		}
	}

	protected void handlePersistSessionsParam(Map<String, String> parameterMap) {
		String persistSessionsS = parameterMap.get(PARAM_PERSIST_SESSIONS);
		if (isEmpty(persistSessionsS) || persistSessionsS.equals(Boolean.FALSE.toString())) {
			this.persistSessions = false;
		} else if (persistSessionsS.equals(Boolean.TRUE.toString())) {
			this.persistSessions = true;

			String persistSessionsPathS = parameterMap.get(PARAM_PERSIST_SESSIONS_PATH);
			if (isEmpty(persistSessionsPathS)) {
				String msg = "Parameter {0} has illegal value {1}.";
				msg = format(msg, PARAM_PERSIST_SESSIONS_PATH, persistSessionsPathS);
				throw new PrivilegeModelException(msg);
			}

			this.persistSessionsPath = getPersistSessionFile(persistSessionsPathS);
			logger.info("Enabling persistence of sessions to {}", this.persistSessionsPath.getAbsolutePath());
		} else {
			String msg = "Parameter {0} has illegal value {1}. Overriding with {2}";
			msg = format(msg, PARAM_PERSIST_SESSIONS, persistSessionsS, Boolean.FALSE);
			logger.error(msg);
			this.persistSessions = false;
		}
	}

	protected File getPersistSessionFile(String persistSessionsPathS) {
		File persistSessionsPath = new File(persistSessionsPathS);
		if (!persistSessionsPath.getParentFile().isDirectory()) {
			String msg = "Path for param {0} is invalid as parent does not exist or is not a directory. Value: {1}";
			msg = format(msg, PARAM_PERSIST_SESSIONS_PATH, persistSessionsPath.getAbsolutePath());
			throw new PrivilegeModelException(msg);
		}

		if (persistSessionsPath.exists() && (!persistSessionsPath.isFile() || !persistSessionsPath.canWrite())) {
			String msg = "Path for param {0} is invalid as file exists but is not a file or not writeable. Value: {1}";
			msg = format(msg, PARAM_PERSIST_SESSIONS_PATH, persistSessionsPath.getAbsolutePath());
			throw new PrivilegeModelException(msg);
		}
		return persistSessionsPath;
	}

	private void handleConflictResolutionParam(Map<String, String> parameterMap) {
		String privilegeConflictResolutionS = parameterMap.get(PARAM_PRIVILEGE_CONFLICT_RESOLUTION);
		if (privilegeConflictResolutionS == null) {
			this.privilegeConflictResolution = PrivilegeConflictResolution.MERGE;
			if (logger.isDebugEnabled()) {
				String msg = "No {0} parameter defined. Using {1}";
				msg = format(msg, PARAM_PRIVILEGE_CONFLICT_RESOLUTION, this.privilegeConflictResolution);
				logger.info(msg);
			}
		} else {
			try {
				this.privilegeConflictResolution = PrivilegeConflictResolution.valueOf(privilegeConflictResolutionS);
			} catch (Exception e) {
				String msg = "Parameter {0} has illegal value {1}.";
				msg = format(msg, PARAM_PRIVILEGE_CONFLICT_RESOLUTION, privilegeConflictResolutionS);
				throw new PrivilegeModelException(msg);
			}
		}
		logger.info("Privilege conflict resolution set to {}", this.privilegeConflictResolution);
	}

	private void handleSecretParams(Map<String, String> parameterMap) {

		String secretKeyS = parameterMap.get(PARAM_SECRET_KEY);
		if (isEmpty(secretKeyS)) {
			String msg = "Parameter {0} may not be empty";
			msg = format(msg, PARAM_SECRET_KEY);
			throw new PrivilegeModelException(msg);
		}

		String secretSaltS = parameterMap.get(PARAM_SECRET_SALT);
		if (isEmpty(secretSaltS)) {
			String msg = "Parameter {0} may not be empty";
			msg = format(msg, PARAM_SECRET_SALT);
			throw new PrivilegeModelException(msg);
		}

		this.secretKey = AesCryptoHelper.buildSecret(secretKeyS.toCharArray(), secretSaltS.getBytes());

		// remove secrets
		parameterMap.remove(PARAM_SECRET_KEY);
		parameterMap.remove(PARAM_SECRET_SALT);
	}

	private void validatePrivilegeConflicts() {
		if (!this.privilegeConflictResolution.isStrict()) {
			return;
		}

		List<String> conflicts = new ArrayList<>();
		List<User> users = this.persistenceHandler.getAllUsers();
		for (User user : users) {
			Map<String, String> privilegeNames = new HashMap<>();
			conflicts.addAll(this.crudHandler.detectPrivilegeConflicts(privilegeNames, user));
		}

		if (!conflicts.isEmpty()) {
			for (String conflict : conflicts) {
				logger.error(conflict);
			}
			throw new PrivilegeModelException("There are " + conflicts.size() + " privilege conflicts!");
		}
	}

	/**
	 * Invalidates all the sessions for the given user and persists the sessions async
	 *
	 * @param user the user for which to invalidate the sessions
	 */
	void invalidateSessionsFor(User user) {
		List<PrivilegeContext> contexts = new ArrayList<>(this.privilegeContextMap.values());
		for (PrivilegeContext ctx : contexts) {
			if (ctx.getUsername().equals(user.getUsername()))
				invalidate(ctx.getCertificate());
		}

		persistSessionsAsync();
	}

	/**
	 * Replaces any existing {@link PrivilegeContext} for the given user by updating with the new user object
	 *
	 * @param newUser the new user to update with
	 */
	void updateExistingSessionsForUser(User newUser, boolean persistSessions) {
		List<PrivilegeContext> contexts = new ArrayList<>(this.privilegeContextMap.values());
		for (PrivilegeContext ctx : contexts) {
			if (!ctx.getUsername().equals(newUser.getUsername()))
				continue;
			replacePrivilegeContextForCert(newUser, ctx.getCertificate());
		}

		if (persistSessions)
			persistSessionsAsync();
	}

	/**
	 * Replaces any existing {@link PrivilegeContext} for users with the given role
	 *
	 * @param role the role to update with
	 */
	void updateExistingSessionsWithNewRole(Role role) {
		List<PrivilegeContext> contexts = new ArrayList<>(this.privilegeContextMap.values());
		for (PrivilegeContext ctx : contexts) {
			if (!ctx.getCertificate().hasRole(role.getName()))
				continue;
			User user = this.persistenceHandler.getUser(ctx.getUsername());
			if (user == null)
				continue;
			replacePrivilegeContextForCert(user, ctx.getCertificate());
		}

		persistSessionsAsync();
	}

	/**
	 * Replaces any existing {@link PrivilegeContext} for users with the given group
	 *
	 * @param group the group to update with
	 */
	void updateExistingSessionsWithNewGroup(Group group) {
		List<PrivilegeContext> contexts = new ArrayList<>(this.privilegeContextMap.values());
		for (PrivilegeContext ctx : contexts) {
			if (!ctx.getCertificate().hasGroup(group.name()))
				continue;
			User user = this.persistenceHandler.getUser(ctx.getUsername());
			if (user == null)
				continue;
			replacePrivilegeContextForCert(user, ctx.getCertificate());
		}

		persistSessionsAsync();
	}

	@Override
	public void runAs(String username, SystemAction action) throws Exception {
		PrivilegeContext systemUserPrivilegeContext = initiateSystemPrivilege(username, action);
		String sessionId = systemUserPrivilegeContext.getCertificate().getSessionId();
		try {
			// perform the action
			action.execute(systemUserPrivilegeContext);
		} finally {
			this.privilegeContextMap.remove(sessionId);
		}
	}

	@Override
	public <T> T runWithResult(String username, SystemActionWithResult<T> action) throws Exception {
		PrivilegeContext systemUserPrivilegeContext = initiateSystemPrivilege(username, action);
		String sessionId = systemUserPrivilegeContext.getCertificate().getSessionId();
		try {
			// perform the action
			return action.execute(systemUserPrivilegeContext);
		} finally {
			this.privilegeContextMap.remove(sessionId);
		}
	}

	@Override
	public PrivilegeContext openSystemUserContext(String username) throws PrivilegeException {
		return buildSystemUserPrivilegeContext(username);
	}

	protected PrivilegeContext initiateSystemPrivilege(String username, Restrictable restrictable) {
		if (username == null)
			throw new PrivilegeException("systemUsername may not be null!");
		if (restrictable == null)
			throw new PrivilegeException("action may not be null!");

		PrivilegeContext systemUserPrivilegeContext = buildSystemUserPrivilegeContext(username);
		systemUserPrivilegeContext.validateAction(restrictable);
		return systemUserPrivilegeContext;
	}

	/**
	 * Returns the {@link Certificate} for the given system username. If it does not yet exist, then it is created by
	 * authenticating the system user
	 *
	 * @param systemUsername the name of the system user
	 *
	 * @return the {@link Certificate} for this system user
	 */
	protected PrivilegeContext buildSystemUserPrivilegeContext(String systemUsername) {

		// get user object
		User user = this.persistenceHandler.getUser(systemUsername);

		// no user means no authentication
		if (user == null) {
			String msg = format("The system user with username {0} does not exist!", systemUsername);
			throw new AccessDeniedException(msg);
		}

		// validate password
		if (user.getPasswordCrypt() != null) {
			String msg = format("System users must not have a password: {0}", user.getUsername());
			throw new AccessDeniedException(msg);
		}

		// validate user state is system
		if (user.getUserState() != UserState.SYSTEM) {
			String msg = "The system {0} user does not have expected user state {1}";
			msg = format(msg, user.getUsername(), UserState.SYSTEM);
			throw new PrivilegeException(msg);
		}

		// validate user has at least one role
		if (user.getRoles().isEmpty()) {
			String msg = format("The system user {0} does not have any roles defined!", user.getUsername());
			throw new PrivilegeException(msg);
		}

		// initialize a new privilege context
		PrivilegeContext privilegeContext = buildPrivilegeContext(Usage.ANY, user, "internal", ZonedDateTime.now(),
				false);

		// log
		if (logger.isDebugEnabled()) {
			String msg = "The system user ''{0}'' is logged in with session {1}";
			msg = format(msg, user.getUsername(), privilegeContext.getCertificate().getSessionId());
			logger.info(msg);
		}

		return privilegeContext;
	}

	protected void buildPrivilegeContext(User user, CertificateStub stub) {
		PrivilegeContext privilegeContext = getPrivilegeContextBuilder().buildPrivilegeContext(stub.getUsage(), user,
				stub.getAuthToken(), stub.getSessionId(), stub.getSource(), stub.getLoginTime(), stub.isKeepAlive());
		Certificate certificate = privilegeContext.getCertificate();
		certificate.setLocale(stub.getLocale());
		certificate.setLastAccess(stub.getLastAccess());
		this.privilegeContextMap.put(certificate.getSessionId(), privilegeContext);
	}

	protected void replacePrivilegeContextForCert(User user, Certificate cert) {
		PrivilegeContext ctx = getPrivilegeContextBuilder().buildPrivilegeContext(cert.getUsage(), user,
				cert.getAuthToken(), cert.getSessionId(), cert.getSource(), cert.getLoginTime(), cert.isKeepAlive());
		this.privilegeContextMap.put(ctx.getCertificate().getSessionId(), ctx);
	}

	protected PrivilegeContext buildPrivilegeContext(Usage usage, User user, String source, ZonedDateTime loginTime,
			boolean keepAlive) {
		PrivilegeContext ctx = getPrivilegeContextBuilder().buildPrivilegeContext(usage, user, source, loginTime,
				keepAlive);
		this.privilegeContextMap.put(ctx.getCertificate().getSessionId(), ctx);
		return ctx;
	}

	protected PrivilegeContextBuilder getPrivilegeContextBuilder() {
		return new PrivilegeContextBuilder(this);
	}

	protected static class PersonalAccessTokenCacheEntry {
		public final PrivilegeContext context;
		public long lastAccess;

		public PersonalAccessTokenCacheEntry(PrivilegeContext context) {
			this.context = context;
			this.lastAccess = System.currentTimeMillis();
		}
	}
}
