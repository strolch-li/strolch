/*
 * Copyright (c) 2013-2026 Robert von Burg <eitch@eitchnet.ch>
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

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.Group;
import li.strolch.privilege.model.Usage;
import li.strolch.privilege.model.internal.PersonalAccessToken;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.model.internal.UserHistory;
import li.strolch.privilege.xml.*;
import li.strolch.privilege.xml.CertificateStubsSaxReader.CertificateStub;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.FileHelper;
import li.strolch.utils.helper.XmlHelper;
import li.strolch.utils.iso8601.ISO8601;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.stream.XMLStreamException;
import java.io.*;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.time.ZonedDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

import static java.lang.Boolean.parseBoolean;
import static java.text.MessageFormat.format;
import static li.strolch.privilege.handler.PrivilegeHandler.PARAM_CASE_INSENSITIVE_USERNAME;
import static li.strolch.privilege.helper.XmlConstants.*;
import static li.strolch.utils.helper.StringHelper.isEmpty;
import static li.strolch.utils.iso8601.ISO8601.EMPTY_VALUE_ZONED_DATE;

/**
 * {@link PersistenceHandler} implementation which stores each authorization entity (User, Role, Group,
 * PersonalAccessToken) as an individual XML file under a model directory, and dynamic runtime state (UserHistory, token
 * lastUsed) in separate per-element .properties state files under a state directory.
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class PerElementXmlPersistenceHandler implements PersistenceHandler {

	protected static final Logger logger = LoggerFactory.getLogger(PerElementXmlPersistenceHandler.class);

	private final Map<String, User> usersByUsername;
	private final Map<String, User> usersById;
	private final Map<String, Group> groups;
	private final Map<String, Role> roles;
	private final Map<String, PersonalAccessToken> tokens;

	private final ConcurrentHashMap<String, ReentrantLock> locks;

	private Map<String, String> parameterMap;
	private boolean verbose;
	private boolean caseInsensitiveUsername;
	private boolean autoMigrateMonolithic;

	private File basePath;
	private File modelDir;
	private File stateDir;

	private File modelUsersDir;
	private File modelRolesDir;
	private File modelGroupsDir;
	private File modelTokensDir;

	private File stateUsersDir;
	private File stateTokensDir;
	private File stateSessionsDir;

	private boolean persistSessions;

	public PerElementXmlPersistenceHandler() {
		this.roles = new ConcurrentHashMap<>();
		this.groups = new ConcurrentHashMap<>();
		this.usersByUsername = new ConcurrentHashMap<>();
		this.usersById = new ConcurrentHashMap<>();
		this.tokens = new ConcurrentHashMap<>();
		this.locks = new ConcurrentHashMap<>();
	}

	@Override
	public Map<String, String> getParameterMap() {
		return this.parameterMap;
	}

	@Override
	public List<User> getAllUsers() {
		return new LinkedList<>(this.usersByUsername.values());
	}

	@Override
	public List<Group> getAllGroups() {
		return new LinkedList<>(this.groups.values());
	}

	@Override
	public List<Role> getAllRoles() {
		return new LinkedList<>(this.roles.values());
	}

	@Override
	public List<PersonalAccessToken> getAllAccessTokens() {
		return new LinkedList<>(this.tokens.values());
	}

	@Override
	public User getUser(String username) {
		return this.usersByUsername.get(evaluateUsername(username));
	}

	@Override
	public User getUserById(String userId) {
		return this.usersById.get(userId);
	}

	@Override
	public boolean hasUser(String username) {
		return this.usersByUsername.containsKey(evaluateUsername(username));
	}

	@Override
	public Group getGroup(String groupName) {
		return this.groups.get(groupName);
	}

	@Override
	public Role getRole(String roleName) {
		return this.roles.get(roleName);
	}

	@Override
	public PersonalAccessToken getAccessToken(String tokenId) {
		return this.tokens.get(tokenId);
	}

	@Override
	public List<PersonalAccessToken> getAccessTokensForUser(String username) {
		return this.tokens.values().stream().filter(t -> t.username().equals(username)).toList();
	}

	private String evaluateUsername(String username) {
		return this.caseInsensitiveUsername ? username.toLowerCase() : username;
	}

	private ReentrantLock getLock(String prefix, String id) {
		return this.locks.computeIfAbsent(prefix + ":" + id, k -> new ReentrantLock());
	}

	@Override
	public void addUser(User user) {
		DBC.PRE.assertNotNull("user may not be null", user);
		DBC.PRE.assertNotEmpty(() -> "userId must not be empty for user " + user.username(), user.userId());

		String username = evaluateUsername(user.getUsername());
		ReentrantLock lock = getLock("user", user.getUserId());
		lock.lock();
		try {
			if (this.usersByUsername.containsKey(username))
				throw new IllegalStateException(
						format("The user with username {0} already exists!", user.getUsername()));
			if (this.usersById.containsKey(user.getUserId()))
				throw new IllegalStateException(format("The user with user ID {0} already exists!", user.getUserId()));

			writeUserXml(user);
			if (!user.isHistoryEmpty())
				writeUserState(user.getUserId(), user.getHistory());

			this.usersByUsername.put(username, user);
			this.usersById.put(user.getUserId(), user);
		} catch (Exception e) {
			if (e instanceof RuntimeException re)
				throw re;
			throw new PrivilegeException("Failed to add user " + user.getUsername(), e);
		} finally {
			lock.unlock();
		}
	}

	@Override
	public void replaceUser(User user) {
		DBC.PRE.assertNotNull("user may not be null", user);
		DBC.PRE.assertNotEmpty(() -> "userId must not be empty for user " + user.username(), user.userId());

		String username = evaluateUsername(user.getUsername());
		ReentrantLock lock = getLock("user", user.getUserId());
		lock.lock();
		try {
			User currentByUsername = this.usersByUsername.get(username);
			if (currentByUsername == null)
				throw new IllegalStateException(
						format("The user with username {0} does not exist!", user.getUsername()));
			User currentById = this.usersById.get(user.getUserId());
			if (currentById == null)
				throw new IllegalStateException(format("The user with user ID {0} does not exist!", user.getUserId()));
			if (!currentByUsername.getUserId().equals(user.getUserId()) || !currentById
					.getUsername()
					.equals(user.getUsername()))
				throw new IllegalStateException(
						format("User ID or username cannot be changed for user {0}!", user.getUsername()));

			if (hasStaticUserChanges(currentById, user))
				writeUserXml(user);

			if (!Objects.equals(currentById.getHistory(), user.getHistory()))
				writeUserState(user.getUserId(), user.getHistory());

			this.usersByUsername.put(username, user);
			this.usersById.put(user.getUserId(), user);
		} catch (Exception e) {
			if (e instanceof RuntimeException re)
				throw re;
			throw new PrivilegeException("Failed to replace user " + user.getUsername(), e);
		} finally {
			lock.unlock();
		}
	}

	@Override
	public boolean updateUserState(User user) {
		DBC.PRE.assertNotNull("user may not be null", user);
		DBC.PRE.assertNotEmpty(() -> "userId must not be empty for user " + user.username(), user.userId());

		String username = evaluateUsername(user.getUsername());
		ReentrantLock lock = getLock("user", user.getUserId());
		lock.lock();
		try {
			User currentById = this.usersById.get(user.getUserId());
			if (currentById == null)
				return false;

			writeUserState(user.getUserId(), user.getHistory());
			this.usersByUsername.put(username, user);
			this.usersById.put(user.getUserId(), user);
			return true;
		} catch (Exception e) {

			if (e instanceof RuntimeException re)
				throw re;
			throw new PrivilegeException("Failed to update user state for " + user.getUsername(), e);
		} finally {
			lock.unlock();
		}
	}

	private boolean hasStaticUserChanges(User u1, User u2) {
		return !Objects.equals(u1.getUserId(), u2.getUserId())
				|| !Objects.equals(u1.getUsername(), u2.getUsername())
				|| !Objects.equals(u1.getFirstname(), u2.getFirstname())
				|| !Objects.equals(u1.getLastname(), u2.getLastname())
				|| !Objects.equals(u1.getUserState(), u2.getUserState())
				|| !Objects.equals(u1.getLocale(), u2.getLocale())
				|| !Objects.equals(u1.getPasswordCrypt(), u2.getPasswordCrypt())
				|| !Objects.equals(u1.getRoles(), u2.getRoles())
				|| !Objects.equals(u1.getGroups(), u2.getGroups())
				|| !Objects.equals(u1.getProperties(), u2.getProperties())
				|| u1.isPasswordChangeRequested() != u2.isPasswordChangeRequested();
	}

	@Override
	public User removeUserById(String userId) {
		DBC.PRE.assertNotEmpty("userId must not be empty", userId);
		ReentrantLock lock = getLock("user", userId);
		lock.lock();
		try {
			User user = this.usersById.remove(userId);
			if (user != null) {
				this.usersByUsername.remove(evaluateUsername(user.getUsername()));
				deleteUserFiles(userId);
			}
			return user;
		} finally {
			lock.unlock();
		}
	}

	@Override
	public void addRole(Role role) {
		DBC.PRE.assertNotNull("role may not be null", role);
		ReentrantLock lock = getLock("role", role.getName());
		lock.lock();
		try {
			if (this.roles.containsKey(role.getName()))
				throw new IllegalStateException(format("The role {0} already exists!", role.getName()));
			writeRoleXml(role);
			this.roles.put(role.getName(), role);
		} catch (Exception e) {
			if (e instanceof RuntimeException re)
				throw re;
			throw new PrivilegeException("Failed to add role " + role.getName(), e);
		} finally {
			lock.unlock();
		}
	}

	@Override
	public void replaceRole(Role role) {
		DBC.PRE.assertNotNull("role may not be null", role);
		ReentrantLock lock = getLock("role", role.getName());
		lock.lock();
		try {
			if (!this.roles.containsKey(role.getName()))
				throw new IllegalStateException(format("The role {0} does not exist!", role.getName()));
			writeRoleXml(role);
			this.roles.put(role.getName(), role);
		} catch (Exception e) {
			if (e instanceof RuntimeException re)
				throw re;
			throw new PrivilegeException("Failed to replace role " + role.getName(), e);
		} finally {
			lock.unlock();
		}
	}

	@Override
	public Role removeRole(String roleName) {
		DBC.PRE.assertNotEmpty("roleName must not be empty", roleName);
		ReentrantLock lock = getLock("role", roleName);
		lock.lock();
		try {
			Role role = this.roles.remove(roleName);
			if (role != null)
				deleteRoleFile(roleName);
			return role;
		} finally {
			lock.unlock();
		}
	}

	@Override
	public void addGroup(Group group) {
		DBC.PRE.assertNotNull("group may not be null", group);
		ReentrantLock lock = getLock("group", group.name());
		lock.lock();
		try {
			if (this.groups.containsKey(group.name()))
				throw new IllegalStateException(format("The group {0} already exists!", group.name()));
			writeGroupXml(group);
			this.groups.put(group.name(), group);
		} catch (Exception e) {
			if (e instanceof RuntimeException re)
				throw re;
			throw new PrivilegeException("Failed to add group " + group.name(), e);
		} finally {
			lock.unlock();
		}
	}

	@Override
	public void replaceGroup(Group group) {
		DBC.PRE.assertNotNull("group may not be null", group);
		ReentrantLock lock = getLock("group", group.name());
		lock.lock();
		try {
			if (!this.groups.containsKey(group.name()))
				throw new IllegalStateException(format("The group {0} does not exist!", group.name()));
			writeGroupXml(group);
			this.groups.put(group.name(), group);
		} catch (Exception e) {
			if (e instanceof RuntimeException re)
				throw re;
			throw new PrivilegeException("Failed to replace group " + group.name(), e);
		} finally {
			lock.unlock();
		}
	}

	@Override
	public Group removeGroup(String groupName) {
		DBC.PRE.assertNotEmpty("groupName must not be empty", groupName);
		ReentrantLock lock = getLock("group", groupName);
		lock.lock();
		try {
			Group group = this.groups.remove(groupName);
			if (group != null)
				deleteGroupFile(groupName);
			return group;
		} finally {
			lock.unlock();
		}
	}

	@Override
	public void addAccessToken(PersonalAccessToken accessToken) {
		DBC.PRE.assertNotNull("accessToken may not be null", accessToken);
		ReentrantLock lock = getLock("token", accessToken.tokenId());
		lock.lock();
		try {
			if (this.tokens.containsKey(accessToken.tokenId()))
				throw new IllegalStateException(
						format("The personal access token {0} already exists!", accessToken.tokenId()));
			writeTokenXml(accessToken);
			if (accessToken.lastUsed() != null)
				writeTokenState(accessToken.tokenId(), accessToken.lastUsed());
			this.tokens.put(accessToken.tokenId(), accessToken);
		} catch (Exception e) {
			if (e instanceof RuntimeException re)
				throw re;
			throw new PrivilegeException("Failed to add personal access token " + accessToken.tokenId(), e);
		} finally {
			lock.unlock();
		}
	}

	@Override
	public PersonalAccessToken removeAccessToken(String tokenId) {
		DBC.PRE.assertNotEmpty("tokenId must not be empty", tokenId);
		ReentrantLock lock = getLock("token", tokenId);
		lock.lock();
		try {
			PersonalAccessToken token = this.tokens.remove(tokenId);
			if (token != null)
				deleteTokenFiles(tokenId);
			return token;
		} finally {
			lock.unlock();
		}
	}

	@Override
	public boolean updateAccessTokenLastUsed(String tokenId, ZonedDateTime lastUsed) {
		DBC.PRE.assertNotEmpty("tokenId must not be empty", tokenId);
		DBC.PRE.assertNotNull("lastUsed may not be null", lastUsed);
		ReentrantLock lock = getLock("token", tokenId);
		lock.lock();
		try {
			PersonalAccessToken token = this.tokens.get(tokenId);
			if (token == null)
				return false;
			writeTokenState(tokenId, lastUsed);
			this.tokens.put(tokenId, token.withLastUsed(lastUsed));
			return true;
		} catch (Exception e) {
			if (e instanceof RuntimeException re)
				throw re;
			throw new PrivilegeException("Failed to update token last used for " + tokenId, e);
		} finally {
			lock.unlock();
		}
	}

	@Override
	public List<CertificateStub> getAllSessions() {
		if (!this.persistSessions)
			return List.of();
		if (this.stateSessionsDir == null || !this.stateSessionsDir.exists())
			return List.of();
		File[] files = this.stateSessionsDir.listFiles((d, name) -> name.endsWith(".properties"));
		if (files == null || files.length == 0)
			return List.of();

		List<CertificateStub> sessions = new ArrayList<>();
		for (File file : files) {
			CertificateStub stub = readSessionState(file);
			if (stub != null)
				sessions.add(stub);
		}
		return sessions;
	}

	@Override
	public void addSession(Certificate certificate) {
		if (!this.persistSessions)
			return;
		DBC.PRE.assertNotNull("certificate may not be null", certificate);
		ReentrantLock lock = getLock("session", certificate.getSessionId());
		lock.lock();
		try {
			writeSessionState(certificate);
		} finally {
			lock.unlock();
		}
	}

	@Override
	public void updateSession(Certificate certificate) {
		if (!this.persistSessions)
			return;
		DBC.PRE.assertNotNull("certificate may not be null", certificate);
		ReentrantLock lock = getLock("session", certificate.getSessionId());
		lock.lock();
		try {
			writeSessionState(certificate);
		} finally {
			lock.unlock();
		}
	}

	@Override
	public void removeSession(String sessionId) {
		if (!this.persistSessions)
			return;
		DBC.PRE.assertNotEmpty("sessionId must not be empty", sessionId);
		ReentrantLock lock = getLock("session", sessionId);
		lock.lock();
		try {
			deleteSessionFile(sessionId);
		} finally {
			lock.unlock();
		}
	}

	@Override
	public boolean persist() throws XMLStreamException, IOException {
		return false;
	}

	@Override
	public void initialize(Map<String, String> paramsMap) {
		this.parameterMap = Map.copyOf(paramsMap);
		this.verbose = parseBoolean(paramsMap.getOrDefault(PARAM_VERBOSE, "false"));
		this.caseInsensitiveUsername = parseBoolean(
				this.parameterMap.getOrDefault(PARAM_CASE_INSENSITIVE_USERNAME, "true"));
		this.autoMigrateMonolithic = parseBoolean(
				this.parameterMap.getOrDefault(PARAM_AUTO_MIGRATE_MONOLITHIC, "true"));
		this.persistSessions = parseBoolean(
				this.parameterMap.getOrDefault(PARAM_PERSIST_SESSIONS, PARAM_PERSIST_SESSIONS_DEF));

		String basePathStr = this.parameterMap.get(PARAM_BASE_PATH);
		if (isEmpty(basePathStr)) {
			String msg = "[{0}] Defined parameter {1} is missing!";
			msg = format(msg, PersistenceHandler.class.getName(), PARAM_BASE_PATH);
			throw new PrivilegeException(msg);
		}

		this.basePath = new File(basePathStr);
		if (!this.basePath.exists() && !this.basePath.mkdirs()) {
			String msg = "[{0}] Could not create base path directory at {1}";
			msg = format(msg, PersistenceHandler.class.getName(), this.basePath.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		String modelDirName = this.parameterMap.getOrDefault(PARAM_MODEL_DIR, PARAM_MODEL_DIR_DEF);
		String stateDirName = this.parameterMap.getOrDefault(PARAM_STATE_DIR, PARAM_STATE_DIR_DEF);

		this.modelDir = new File(this.basePath, modelDirName);
		this.stateDir = new File(this.basePath, stateDirName);

		this.modelUsersDir = new File(this.modelDir, "users");
		this.modelRolesDir = new File(this.modelDir, "roles");
		this.modelGroupsDir = new File(this.modelDir, "groups");
		this.modelTokensDir = new File(this.modelDir, "tokens");

		this.stateUsersDir = new File(this.stateDir, "users");
		this.stateTokensDir = new File(this.stateDir, "tokens");
		this.stateSessionsDir = new File(this.stateDir, "sessions");

		reload();
	}

	private void ensureDirectories() {
		ensureDir(this.modelUsersDir);
		ensureDir(this.modelRolesDir);
		ensureDir(this.modelGroupsDir);
		ensureDir(this.modelTokensDir);
		ensureDir(this.stateUsersDir);
		ensureDir(this.stateTokensDir);
		ensureDir(this.stateSessionsDir);
	}

	private void ensureDir(File dir) {
		if (!dir.exists() && !dir.mkdirs())
			throw new PrivilegeException("Could not create directory at " + dir.getAbsolutePath());
	}

	@Override
	public boolean reload() {
		ensureDirectories();

		if (this.autoMigrateMonolithic && isModelEmpty())
			migrateMonolithicFiles();

		// ROLES
		Map<String, Role> loadedRoles = new HashMap<>();
		for (File file : listXmlFiles(this.modelRolesDir)) {
			PrivilegeRolesSaxReader reader = new PrivilegeRolesSaxReader(this.verbose);
			XmlHelper.parseDocument(file, reader, this.verbose);
			for (Role role : reader.getRoles().values()) {
				if (loadedRoles.containsKey(role.getName()))
					throw new IllegalStateException(format("The role {0} already exists!", role.getName()));
				loadedRoles.put(role.getName(), role);
			}
		}

		// GROUPS
		Map<String, Group> loadedGroups = new HashMap<>();
		for (File file : listXmlFiles(this.modelGroupsDir)) {
			PrivilegeGroupsSaxReader reader = new PrivilegeGroupsSaxReader(this.verbose);
			XmlHelper.parseDocument(file, reader, this.verbose);
			for (Group group : reader.getGroups().values()) {
				if (loadedGroups.containsKey(group.name()))
					throw new IllegalStateException(format("The group {0} already exists!", group.name()));
				loadedGroups.put(group.name(), group);
			}
		}

		// TOKENS
		Map<String, PersonalAccessToken> loadedTokens = new HashMap<>();
		for (File file : listXmlFiles(this.modelTokensDir)) {
			PrivilegeTokensSaxReader reader = new PrivilegeTokensSaxReader(this.verbose);
			XmlHelper.parseDocument(file, reader, this.verbose);
			for (PersonalAccessToken token : reader.getTokens().values()) {
				if (loadedTokens.containsKey(token.tokenId()))
					throw new IllegalStateException(
							format("The personal access token {0} already exists!", token.tokenId()));
				loadedTokens.put(token.tokenId(), token);
			}
		}

		// USERS
		Map<String, User> loadedUsersByUsername = new HashMap<>();
		Map<String, User> loadedUsersById = new HashMap<>();
		for (File file : listXmlFiles(this.modelUsersDir)) {
			PrivilegeUsersSaxReader reader = new PrivilegeUsersSaxReader(this.caseInsensitiveUsername, this.verbose);
			XmlHelper.parseDocument(file, reader, this.verbose);
			for (User user : reader.getUsersByUsername().values()) {
				String username = evaluateUsername(user.getUsername());
				if (loadedUsersByUsername.containsKey(username))
					throw new IllegalStateException(
							format("The user with username {0} already exists!", user.getUsername()));
				if (loadedUsersById.containsKey(user.getUserId()))
					throw new IllegalStateException(
							format("The user with user ID {0} already exists!", user.getUserId()));
				loadedUsersByUsername.put(username, user);
				loadedUsersById.put(user.getUserId(), user);
			}
		}

		// STATE HYDRATION - USERS
		for (Map.Entry<String, User> entry : loadedUsersById.entrySet()) {
			User user = entry.getValue();
			UserHistory stateHistory = readUserState(user.getUserId());
			if (stateHistory != null && !stateHistory.isEmpty()) {
				User hydratedUser = user.withHistory(stateHistory);
				loadedUsersById.put(hydratedUser.getUserId(), hydratedUser);
				loadedUsersByUsername.put(evaluateUsername(hydratedUser.getUsername()), hydratedUser);
			}
		}

		// STATE HYDRATION - TOKENS
		for (Map.Entry<String, PersonalAccessToken> entry : loadedTokens.entrySet()) {
			PersonalAccessToken token = entry.getValue();
			ZonedDateTime lastUsed = readTokenState(token.tokenId());
			if (lastUsed != null) {
				PersonalAccessToken hydratedToken = token.withLastUsed(lastUsed);
				loadedTokens.put(hydratedToken.tokenId(), hydratedToken);
			}
		}

		this.roles.clear();
		this.roles.putAll(loadedRoles);

		this.groups.clear();
		this.groups.putAll(loadedGroups);

		this.tokens.clear();
		this.tokens.putAll(loadedTokens);

		this.usersByUsername.clear();
		this.usersByUsername.putAll(loadedUsersByUsername);

		this.usersById.clear();
		this.usersById.putAll(loadedUsersById);

		logger.info("Read {} Users, {} Groups, {} Roles, {} Tokens", this.usersByUsername.size(), this.groups.size(),
				this.roles.size(), this.tokens.size());

		// validate referenced elements exist
		for (User user : this.usersByUsername.values()) {
			for (String roleName : user.getRoles()) {
				if (getRole(roleName) == null)
					logger.error("Role {} does not exist referenced by user {}", roleName, user.getUsername());
			}

			for (String groupName : user.getGroups()) {
				if (getGroup(groupName) == null)
					logger.error("Group {} does not exist referenced by user {}", groupName, user.getUsername());
			}
		}

		// validate referenced roles exist on groups
		for (Group group : this.groups.values()) {
			for (String roleName : group.roles()) {
				if (getRole(roleName) == null)
					logger.error("Role {} does not exist referenced by group {}", roleName, group.name());
			}
		}

		// validate users exist for tokens
		for (Iterator<PersonalAccessToken> iterator = this.tokens.values().iterator(); iterator.hasNext(); ) {
			PersonalAccessToken token = iterator.next();
			if (getUser(token.username()) == null) {
				logger.error("User {} does not exist referenced by token {}", token.username(), token.tokenId());
				iterator.remove();
			}
		}

		return true;
	}

	private boolean isModelEmpty() {
		return !hasXmlFiles(this.modelUsersDir)
				&& !hasXmlFiles(this.modelRolesDir)
				&& !hasXmlFiles(this.modelGroupsDir)
				&& !hasXmlFiles(this.modelTokensDir);
	}

	private boolean hasXmlFiles(File dir) {
		if (!dir.exists() || !dir.isDirectory())
			return false;
		File[] files = dir.listFiles((d, name) -> name.endsWith(".xml"));
		return files != null && files.length > 0;
	}

	private List<File> listXmlFiles(File dir) {
		if (!dir.exists() || !dir.isDirectory())
			return List.of();
		File[] files = dir.listFiles((d, name) -> name.endsWith(".xml"));
		if (files == null || files.length == 0)
			return List.of();
		List<File> list = new ArrayList<>(Arrays.asList(files));
		list.sort(Comparator.comparing(File::getName));
		return list;
	}

	private void migrateMonolithicFiles() {
		File usersXml = new File(this.basePath, this.parameterMap.getOrDefault(PARAM_USERS_FILE, PARAM_USERS_FILE_DEF));
		File rolesXml = new File(this.basePath, this.parameterMap.getOrDefault(PARAM_ROLES_FILE, PARAM_ROLES_FILE_DEF));
		File groupsXml = new File(this.basePath,
				this.parameterMap.getOrDefault(PARAM_GROUPS_FILE, PARAM_GROUPS_FILE_DEF));
		File tokensXml = new File(this.basePath,
				this.parameterMap.getOrDefault(PARAM_TOKENS_FILE, PARAM_TOKENS_FILE_DEF));
		File sessionsXml = new File(this.basePath,
				this.parameterMap.getOrDefault(PARAM_SESSIONS_FILE, PARAM_SESSIONS_FILE_DEF));

		if (!usersXml.exists() && !rolesXml.exists() && !groupsXml.exists() && !tokensXml.exists()
				&& (!this.persistSessions || !sessionsXml.exists()))
			return;

		logger.info("Auto-migrating monolithic privilege configuration from {}", this.basePath.getAbsolutePath());

		try {
			if (rolesXml.exists()) {
				PrivilegeRolesSaxReader reader = new PrivilegeRolesSaxReader(this.verbose);
				XmlHelper.parseDocument(rolesXml, reader, this.verbose);
				for (Role role : reader.getRoles().values()) {
					writeRoleXml(role);
				}
			}

			if (groupsXml.exists()) {
				PrivilegeGroupsSaxReader reader = new PrivilegeGroupsSaxReader(this.verbose);
				XmlHelper.parseDocument(groupsXml, reader, this.verbose);
				for (Group group : reader.getGroups().values()) {
					writeGroupXml(group);
				}
			}

			if (tokensXml.exists()) {
				PrivilegeTokensSaxReader reader = new PrivilegeTokensSaxReader(this.verbose);
				XmlHelper.parseDocument(tokensXml, reader, this.verbose);
				for (PersonalAccessToken token : reader.getTokens().values()) {
					writeTokenXml(token);
					if (token.lastUsed() != null)
						writeTokenState(token.tokenId(), token.lastUsed());
				}
			}

			if (usersXml.exists()) {
				PrivilegeUsersSaxReader reader = new PrivilegeUsersSaxReader(this.caseInsensitiveUsername,
						this.verbose);
				XmlHelper.parseDocument(usersXml, reader, this.verbose);
				for (User user : reader.getUsersByUsername().values()) {
					writeUserXml(user);
					if (!user.isHistoryEmpty())
						writeUserState(user.getUserId(), user.getHistory());
				}
			}

			if (this.persistSessions && sessionsXml.exists()) {
				CertificateStubsSaxReader reader = new CertificateStubsSaxReader(sessionsXml);
				List<CertificateStub> stubs = reader.read();
				for (CertificateStub stub : stubs) {
					writeSessionState(stub);
				}
			}
		} catch (Exception e) {
			throw new PrivilegeException("Failed to auto-migrate monolithic configuration", e);
		}
	}

	@FunctionalInterface
	private interface ThrowingConsumer<T> {
		void accept(T t) throws Exception;
	}

	private void writeAtomically(File targetFile, ThrowingConsumer<File> writer) throws Exception {
		File parent = targetFile.getParentFile();
		if (!parent.exists() && !parent.mkdirs())
			throw new IOException("Failed to create directory " + parent.getAbsolutePath());

		File tmpFile = new File(parent, targetFile.getName() + ".tmp");
		try {
			writer.accept(tmpFile);
			try {
				Files.move(tmpFile.toPath(), targetFile.toPath(), StandardCopyOption.ATOMIC_MOVE,
						StandardCopyOption.REPLACE_EXISTING);
			} catch (AtomicMoveNotSupportedException e) {
				Files.move(tmpFile.toPath(), targetFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
			}
		} catch (Exception e) {
			if (tmpFile.exists())
				tmpFile.delete();
			throw e;
		}
	}

	private void writeUserXml(User user) throws Exception {
		File targetFile = new File(this.modelUsersDir, FileHelper.toSafeFilename(user.getUserId()) + ".xml");
		User staticUser = user.withHistory(UserHistory.EMPTY);
		writeAtomically(targetFile, tmpFile -> new PrivilegeUsersSaxWriter(List.of(staticUser), tmpFile).write());
	}

	private void writeTokenXml(PersonalAccessToken token) throws Exception {
		File targetFile = new File(this.modelTokensDir, FileHelper.toSafeFilename(token.tokenId()) + ".xml");
		PersonalAccessToken staticToken = token.withLastUsed(null);
		writeAtomically(targetFile, tmpFile -> new PrivilegeTokensSaxWriter(List.of(staticToken), tmpFile).write());
	}

	private void writeRoleXml(Role role) throws Exception {
		File targetFile = new File(this.modelRolesDir, FileHelper.toSafeFilename(role.getName()) + ".xml");
		writeAtomically(targetFile, tmpFile -> new PrivilegeRolesSaxWriter(List.of(role), tmpFile).write());
	}

	private void writeGroupXml(Group group) throws Exception {
		File targetFile = new File(this.modelGroupsDir, FileHelper.toSafeFilename(group.name()) + ".xml");
		writeAtomically(targetFile, tmpFile -> new PrivilegeGroupsSaxWriter(List.of(group), tmpFile).write());
	}

	private void writeUserState(String userId, UserHistory history) throws Exception {
		File targetFile = new File(this.stateUsersDir, FileHelper.toSafeFilename(userId) + ".properties");
		if (history == null || history.isEmpty()) {
			if (targetFile.exists())
				if (!targetFile.delete())
					logger.warn("Failed to delete user state file: {}", targetFile.getAbsolutePath());
			return;
		}

		writeAtomically(targetFile, tmpFile -> {
			Properties props = new Properties();
			if (!history.isFirstLoginEmpty())
				props.setProperty(PROP_FIRST_LOGIN, ISO8601.toString(history.getFirstLogin()));
			if (!history.isLastLoginEmpty())
				props.setProperty(PROP_LAST_LOGIN, ISO8601.toString(history.getLastLogin()));
			if (!history.isLastPasswordChangeEmpty())
				props.setProperty(PROP_LAST_PASSWORD_CHANGE, ISO8601.toString(history.getLastPasswordChange()));

			try (OutputStream out = new BufferedOutputStream(Files.newOutputStream(tmpFile.toPath()))) {
				props.store(out, "Strolch User Runtime State");
			}
		});
	}

	private void writeTokenState(String tokenId, ZonedDateTime lastUsed) throws Exception {
		File targetFile = new File(this.stateTokensDir, FileHelper.toSafeFilename(tokenId) + ".properties");
		if (lastUsed == null) {
			if (targetFile.exists())
				targetFile.delete();
			return;
		}

		writeAtomically(targetFile, tmpFile -> {
			Properties props = new Properties();
			props.setProperty(PROP_LAST_USED, ISO8601.toString(lastUsed));
			try (OutputStream out = new BufferedOutputStream(Files.newOutputStream(tmpFile.toPath()))) {
				props.store(out, "Strolch Token Runtime State");
			}
		});
	}

	private void deleteUserFiles(String userId) {
		File userXml = new File(this.modelUsersDir, FileHelper.toSafeFilename(userId) + ".xml");
		if (userXml.exists())
			userXml.delete();
		File userState = new File(this.stateUsersDir, FileHelper.toSafeFilename(userId) + ".properties");
		if (userState.exists())
			userState.delete();
	}

	private void deleteRoleFile(String roleName) {
		File roleXml = new File(this.modelRolesDir, FileHelper.toSafeFilename(roleName) + ".xml");
		if (roleXml.exists())
			roleXml.delete();
	}

	private void deleteGroupFile(String groupName) {
		File groupXml = new File(this.modelGroupsDir, FileHelper.toSafeFilename(groupName) + ".xml");
		if (groupXml.exists())
			groupXml.delete();
	}

	private void deleteTokenFiles(String tokenId) {
		File tokenXml = new File(this.modelTokensDir, FileHelper.toSafeFilename(tokenId) + ".xml");
		if (tokenXml.exists())
			tokenXml.delete();
		File tokenState = new File(this.stateTokensDir, FileHelper.toSafeFilename(tokenId) + ".properties");
		if (tokenState.exists())
			tokenState.delete();
	}

	private UserHistory readUserState(String userId) {
		File targetFile = new File(this.stateUsersDir, FileHelper.toSafeFilename(userId) + ".properties");
		if (!targetFile.exists())
			return null;

		Properties props = new Properties();
		try (InputStream in = new BufferedInputStream(Files.newInputStream(targetFile.toPath()))) {
			props.load(in);
		} catch (IOException e) {
			throw new PrivilegeException("Failed to read user state from " + targetFile.getAbsolutePath(), e);
		}

		String firstLoginS = props.getProperty(PROP_FIRST_LOGIN);
		String lastLoginS = props.getProperty(PROP_LAST_LOGIN);
		String lastPasswordChangeS = props.getProperty(PROP_LAST_PASSWORD_CHANGE);

		ZonedDateTime firstLogin = isEmpty(firstLoginS) ? EMPTY_VALUE_ZONED_DATE : ISO8601.parseToZdt(firstLoginS);
		ZonedDateTime lastLogin = isEmpty(lastLoginS) ? EMPTY_VALUE_ZONED_DATE : ISO8601.parseToZdt(lastLoginS);
		ZonedDateTime lastPasswordChange = isEmpty(lastPasswordChangeS) ? EMPTY_VALUE_ZONED_DATE :
				ISO8601.parseToZdt(lastPasswordChangeS);

		return new UserHistory(firstLogin, lastLogin, lastPasswordChange);
	}

	private ZonedDateTime readTokenState(String tokenId) {
		File targetFile = new File(this.stateTokensDir, FileHelper.toSafeFilename(tokenId) + ".properties");
		if (!targetFile.exists())
			return null;

		Properties props = new Properties();
		try (InputStream in = new BufferedInputStream(Files.newInputStream(targetFile.toPath()))) {
			props.load(in);
		} catch (IOException e) {
			throw new PrivilegeException("Failed to read token state from " + targetFile.getAbsolutePath(), e);
		}

		String lastUsedS = props.getProperty(PROP_LAST_USED);
		return isEmpty(lastUsedS) ? null : ISO8601.parseToZdt(lastUsedS);
	}

	private void writeSessionState(Certificate cert) {
		writeSessionState(new CertificateStub(cert));
	}

	private void writeSessionState(CertificateStub cert) {
		File targetFile = new File(this.stateSessionsDir,
				FileHelper.toSafeFilename(cert.getSessionId()) + ".properties");
		try {
			writeAtomically(targetFile, tmpFile -> {
				Properties props = new Properties();
				props.setProperty(PROP_SESSION_ID, cert.getSessionId());
				props.setProperty(PROP_USERNAME, cert.getUsername());
				props.setProperty(PROP_USAGE, cert.getUsage().name());
				props.setProperty(PROP_AUTH_TOKEN, cert.getAuthToken());
				props.setProperty(PROP_SOURCE, cert.getSource());
				props.setProperty(PROP_LOCALE, cert.getLocale().toLanguageTag());
				props.setProperty(PROP_LOGIN_TIME, ISO8601.toString(cert.getLoginTime()));
				props.setProperty(PROP_LAST_ACCESS, ISO8601.toString(cert.getLastAccess()));
				props.setProperty(PROP_KEEP_ALIVE, String.valueOf(cert.isKeepAlive()));

				try (OutputStream out = new BufferedOutputStream(Files.newOutputStream(tmpFile.toPath()))) {
					props.store(out, "Strolch Session Runtime State");
				}
			});
		} catch (Exception e) {
			if (e instanceof RuntimeException re)
				throw re;
			throw new PrivilegeException("Failed to write session state for " + cert.getSessionId(), e);
		}
	}

	private void deleteSessionFile(String sessionId) {
		File sessionState = new File(this.stateSessionsDir, FileHelper.toSafeFilename(sessionId) + ".properties");
		if (sessionState.exists())
			if (!sessionState.delete())
				logger.warn("Failed to delete session state file: {}", sessionState.getAbsolutePath());
	}

	private CertificateStub readSessionState(File targetFile) {
		Properties props = new Properties();
		try (InputStream in = new BufferedInputStream(Files.newInputStream(targetFile.toPath()))) {
			props.load(in);
		} catch (IOException e) {
			logger.error("Failed to read session state from " + targetFile.getAbsolutePath(), e);
			return null;
		}

		String sessionId = props.getProperty(PROP_SESSION_ID);
		String username = props.getProperty(PROP_USERNAME);
		String usageS = props.getProperty(PROP_USAGE);
		String authToken = props.getProperty(PROP_AUTH_TOKEN);
		String source = props.getProperty(PROP_SOURCE);
		String localeS = props.getProperty(PROP_LOCALE);
		String loginTimeS = props.getProperty(PROP_LOGIN_TIME);
		String lastAccessS = props.getProperty(PROP_LAST_ACCESS);
		String keepAliveS = props.getProperty(PROP_KEEP_ALIVE);

		if (isEmpty(sessionId) || isEmpty(username) || isEmpty(authToken) || isEmpty(loginTimeS) || isEmpty(
				lastAccessS)) {
			logger.warn("Corrupted session state file at {}", targetFile.getAbsolutePath());
			return null;
		}

		Usage usage = isEmpty(usageS) ? Usage.SET_PASSWORD : Usage.valueOf(usageS);
		Locale locale = isEmpty(localeS) ? Locale.getDefault() : Locale.forLanguageTag(localeS);
		ZonedDateTime loginTime = ISO8601.parseToZdt(loginTimeS);
		ZonedDateTime lastAccess = ISO8601.parseToZdt(lastAccessS);
		boolean keepAlive = Boolean.parseBoolean(keepAliveS);

		return new CertificateStub(usage, sessionId, username, authToken, source, locale, loginTime, lastAccess,
				keepAlive);
	}
}
