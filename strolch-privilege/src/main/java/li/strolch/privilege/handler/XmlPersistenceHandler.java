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

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.helper.XmlConstants;
import li.strolch.privilege.model.Group;
import li.strolch.privilege.model.internal.AccessToken;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.xml.*;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.XmlHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import static java.lang.Boolean.parseBoolean;
import static java.text.MessageFormat.format;
import static li.strolch.privilege.handler.PrivilegeHandler.PARAM_CASE_INSENSITIVE_USERNAME;
import static li.strolch.privilege.helper.XmlConstants.*;
import static li.strolch.utils.helper.StringHelper.formatNanoDuration;
import static li.strolch.utils.helper.StringHelper.isEmpty;

/**
 * {@link PersistenceHandler} implementation which reads the configuration from XML files. These configuration is passed
 * in {@link #initialize(Map)}
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlPersistenceHandler implements PersistenceHandler {

	protected static final Logger logger = LoggerFactory.getLogger(XmlPersistenceHandler.class);

	private final Map<String, User> usersByUsername;
	private final Map<String, User> usersById;
	private final Map<String, Group> groups;
	private final Map<String, Role> roles;
	private final Map<String, AccessToken> tokens;

	private boolean usersDirty;
	private boolean groupsDirty;
	private boolean rolesDirty;
	private boolean tokensDirty;

	private Map<String, String> parameterMap;
	private boolean verbose;

	private File usersPath;
	private File groupsPath;
	private File rolesPath;
	private File tokensPath;

	private boolean caseInsensitiveUsername;

	public XmlPersistenceHandler() {
		this.roles = new ConcurrentHashMap<>();
		this.groups = new ConcurrentHashMap<>();
		this.usersByUsername = new ConcurrentHashMap<>();
		this.usersById = new ConcurrentHashMap<>();
		this.tokens = new ConcurrentHashMap<>();
	}

	@Override
	public Map<String, String> getParameterMap() {
		return this.parameterMap;
	}

	@Override
	public List<User> getAllUsers() {
		synchronized (this.usersByUsername) {
			return new LinkedList<>(this.usersByUsername.values());
		}
	}

	@Override
	public List<Group> getAllGroups() {
		synchronized (this.groups) {
			return new LinkedList<>(this.groups.values());
		}
	}

	@Override
	public List<Role> getAllRoles() {
		synchronized (this.roles) {
			return new LinkedList<>(this.roles.values());
		}
	}

	@Override
	public List<AccessToken> getAllAccessTokens() {
		synchronized (this.tokens) {
			return new LinkedList<>(this.tokens.values());
		}
	}

	@Override
	public boolean hasUser(String username) {
		return this.usersByUsername.containsKey(evaluateUsername(username));
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
	public Group getGroup(String groupName) {
		return this.groups.get(groupName);
	}

	@Override
	public Role getRole(String roleName) {
		return this.roles.get(roleName);
	}

	@Override
	public synchronized User removeUserById(String userId) {
		User user = this.usersById.remove(userId);
		if (user != null) {
			this.usersByUsername.remove(evaluateUsername(user.getUsername()));
			this.usersDirty = true;
		}
		return user;
	}

	@Override
	public Group removeGroup(String groupName) {
		Group group = this.groups.remove(groupName);
		this.groupsDirty = group != null;
		return group;
	}

	@Override
	public Role removeRole(String roleName) {
		Role role = this.roles.remove(roleName);
		this.rolesDirty = role != null;
		return role;
	}

	@Override
	public synchronized void addUser(User user) {
		DBC.PRE.assertNotEmpty(() -> "userId must not be empty for user " + user.username(), user.userId());
		String username = evaluateUsername(user.getUsername());
		if (this.usersByUsername.containsKey(username))
			throw new IllegalStateException(format("The user with username {0} already exists!", user.getUsername()));
		if (this.usersById.containsKey(user.getUserId()))
			throw new IllegalStateException(format("The user with user ID {0} already exists!", user.getUsername()));
		this.usersByUsername.put(username, user);
		this.usersById.put(user.getUserId(), user);
		this.usersDirty = true;
	}

	@Override
	public synchronized void replaceUser(User user) {
		DBC.PRE.assertNotEmpty(() -> "userId must not be empty for user " + user.username(), user.userId());
		String username = evaluateUsername(user.getUsername());
		if (!this.usersByUsername.containsKey(username))
			throw new IllegalStateException(
					format("The user with username {0} can not be replaced as it does not exist!", user.getUsername()));
		if (!this.usersById.containsKey(user.getUserId()))
			throw new IllegalStateException(
					format("The user with user ID {0} can not be replaced as it does not exist!", user.getUserId()));
		User existingUser = this.usersByUsername.put(username, user);
		if (existingUser != null && !existingUser.getUserId().equals(user.getUserId()))
			throw new IllegalStateException(
					format("Existing user ID {0} differs from new user ID {1}. User ID change is not possible!",
							existingUser.getUserId(), user.getUserId()));
		this.usersById.put(user.getUserId(), user);
		this.usersDirty = true;
	}

	@Override
	public void addGroup(Group group) {
		if (this.groups.containsKey(group.name()))
			throw new IllegalStateException(format("The group {0} already exists!", group.name()));
		this.groups.put(group.name(), group);
		this.groupsDirty = true;
	}

	@Override
	public void replaceGroup(Group group) {
		if (!this.groups.containsKey(group.name()))
			throw new IllegalStateException(
					format("The group {0} can not be replaced as it does not exist!", group.name()));
		this.groups.put(group.name(), group);
		this.groupsDirty = true;
	}

	@Override
	public void addRole(Role role) {
		if (this.roles.containsKey(role.getName()))
			throw new IllegalStateException(format("The role {0} already exists!", role.getName()));
		this.roles.put(role.getName(), role);
		this.rolesDirty = true;
	}

	@Override
	public void replaceRole(Role role) {
		if (!this.roles.containsKey(role.getName()))
			throw new IllegalStateException(
					format("The role {0} can not be replaced as it does not exist!", role.getName()));
		this.roles.put(role.getName(), role);
		this.rolesDirty = true;
	}

	@Override
	public AccessToken getAccessToken(String tokenId) {
		return this.tokens.get(tokenId);
	}

	@Override
	public void addAccessToken(AccessToken accessToken) {
		if (this.tokens.containsKey(accessToken.tokenId()))
			throw new IllegalStateException(format("The access token {0} already exists!", accessToken.tokenId()));
		this.tokens.put(accessToken.tokenId(), accessToken);
		this.tokensDirty = true;
	}

	@Override
	public AccessToken removeAccessToken(String tokenId) {
		AccessToken token = this.tokens.remove(tokenId);
		this.tokensDirty = token != null;
		return token;
	}

	@Override
	public List<AccessToken> getAccessTokensForUser(String username) {
		synchronized (this.tokens) {
			return this.tokens.values().stream().filter(t -> t.username().equals(username)).toList();
		}
	}

	/**
	 * Initializes this {@link XmlPersistenceHandler} by reading the following parameters:
	 * <ul>
	 * <li>{@link XmlConstants#PARAM_BASE_PATH}</li>
	 * <li>{@link XmlConstants#PARAM_USERS_FILE}</li>
	 * <li>{@link XmlConstants#PARAM_GROUPS_FILE}</li>
	 * <li>{@link XmlConstants#PARAM_ROLES_FILE}</li>
	 * <li>{@link XmlConstants#PARAM_TOKENS_FILE}</li>
	 * </ul>
	 */
	@Override
	public void initialize(Map<String, String> paramsMap) {
		this.parameterMap = Map.copyOf(paramsMap);
		this.verbose = parseBoolean(paramsMap.getOrDefault(PARAM_VERBOSE, "false"));

		// get and validate base bath
		String basePath = this.parameterMap.get(PARAM_BASE_PATH);
		File basePathF = new File(basePath);
		if (!basePathF.exists() && !basePathF.isDirectory()) {
			String msg = "[{0}] Defined parameter {1} does not point to a valid path at {2}";
			msg = format(msg, PersistenceHandler.class.getName(), PARAM_BASE_PATH, basePathF.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		File usersPath = getFile(basePath, PARAM_USERS_FILE, PARAM_USERS_FILE_DEF, true);
		File groupsPath = getFile(basePath, PARAM_GROUPS_FILE, PARAM_GROUPS_FILE_DEF, false);
		File rolesPath = getFile(basePath, PARAM_ROLES_FILE, PARAM_ROLES_FILE_DEF, true);
		File tokensPath = getFile(basePath, PARAM_ROLES_FILE, PARAM_TOKENS_FILE, false);

		// save path to model
		this.usersPath = usersPath;
		this.groupsPath = groupsPath;
		this.rolesPath = rolesPath;
		this.tokensPath = tokensPath;

		this.caseInsensitiveUsername = parseBoolean(
				this.parameterMap.getOrDefault(PARAM_CASE_INSENSITIVE_USERNAME, "true"));

		if (reload())
			logger.info("Privilege Data loaded.");
	}

	private File getFile(String basePath, String param, String defaultValue, boolean required) {
		String fileName = this.parameterMap.get(param);
		if (isEmpty(fileName)) {
			fileName = defaultValue;
			if (logger.isDebugEnabled()) {
				String msg = "[{0}] Parameter {1} is not defined, using default {2}!";
				msg = format(msg, PersistenceHandler.class.getName(), param, defaultValue);
				logger.debug(msg);
			}
		}

		String path = basePath + "/" + fileName;
		File file = new File(path);
		if (required && !file.exists()) {
			String msg = "[{0}] Defined parameter {1} is invalid as file does not exist at path {2}";
			msg = format(msg, PersistenceHandler.class.getName(), param, file.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		return file;
	}

	/**
	 * Reads the XML configuration files which contain the model. Which configuration files are parsed was defined in
	 * the while calling {@link #initialize(Map)}
	 *
	 * @see #initialize(Map)
	 */
	@Override
	public boolean reload() {

		// parse models xml file to XML document
		PrivilegeUsersSaxReader usersXmlHandler = new PrivilegeUsersSaxReader(this.caseInsensitiveUsername,
				this.verbose);
		XmlHelper.parseDocument(this.usersPath, usersXmlHandler, this.verbose);

		PrivilegeGroupsSaxReader groupsXmlHandler = new PrivilegeGroupsSaxReader(this.verbose);
		if (this.groupsPath.exists())
			XmlHelper.parseDocument(this.groupsPath, groupsXmlHandler, this.verbose);

		PrivilegeTokensSaxReader tokensXmlHandler = new PrivilegeTokensSaxReader(this.verbose);
		if (this.tokensPath.exists())
			XmlHelper.parseDocument(this.tokensPath, tokensXmlHandler, this.verbose);

		PrivilegeRolesSaxReader rolesXmlHandler = new PrivilegeRolesSaxReader(this.verbose);
		XmlHelper.parseDocument(this.rolesPath, rolesXmlHandler, this.verbose);

		// ROLES
		synchronized (this.roles) {
			this.roles.clear();
			this.roles.putAll(rolesXmlHandler.getRoles());
		}

		// GROUPS
		synchronized (this.groups) {
			this.groups.clear();
			this.groups.putAll(groupsXmlHandler.getGroups());
		}

		// USERS
		synchronized (this) {
			this.usersByUsername.clear();
			usersXmlHandler.getUsers().forEach((username, user) -> {
				this.usersByUsername.put(username, user);
				this.usersById.put(user.getUserId(), user);
			});
		}

		// TOKENS
		synchronized (this.tokens) {
			this.tokens.clear();
			this.tokens.putAll(tokensXmlHandler.getTokens());
		}

		this.usersDirty = false;
		this.groupsDirty = false;
		this.rolesDirty = false;
		this.tokensDirty = false;

		logger.info("Read {} Users", this.usersByUsername.size());
		logger.info("Read {} Groups", this.groups.size());
		logger.info("Read {} Roles", this.roles.size());
		logger.info("Read {} Tokens", this.tokens.size());

		// validate referenced elements exist
		for (User user : this.usersByUsername.values()) {
			for (String roleName : user.getRoles()) {
				// validate that role exists
				if (getRole(roleName) == null)
					logger.error("Role {} does not exist referenced by user {}", roleName, user.getUsername());
			}

			for (String groupName : user.getGroups()) {
				// validate that group exists
				if (getGroup(groupName) == null)
					logger.error("Group {} does not exist referenced by user {}", groupName, user.getUsername());
			}
		}

		// validate referenced roles exist on groups
		for (Group group : this.groups.values()) {
			for (String roleName : group.roles()) {
				// validate that role exists
				if (getRole(roleName) == null)
					logger.error("Role {} does not exist referenced by group {}", roleName, group.name());
			}
		}

		// validate users exist for tokens
		for (Iterator<AccessToken> iterator = this.tokens.values().iterator(); iterator.hasNext(); ) {
			AccessToken token = iterator.next();
			if (getUser(token.username()) == null) {
				logger.error("User {} does not exist referenced by token {}", token.username(), token.tokenId());
				iterator.remove();
			}
		}

		return true;
	}

	/**
	 * Writes the model to the XML files. Where the files are written to was defined in the {@link #initialize(Map)}
	 */
	@Override
	public boolean persist() throws XMLStreamException, IOException {
		long start = System.nanoTime();
		boolean saved = false;

		// write users file
		if (this.usersDirty) {
			new PrivilegeUsersSaxWriter(getAllUsers(), this.usersPath).write();
			this.usersDirty = false;
			saved = true;
		}

		// write groups file
		if (this.groupsDirty) {
			new PrivilegeGroupsSaxWriter(getAllGroups(), this.groupsPath).write();
			this.groupsDirty = false;
			saved = true;
		}

		// write roles file
		if (this.rolesDirty) {
			new PrivilegeRolesSaxWriter(getAllRoles(), this.rolesPath).write();
			this.rolesDirty = false;
			saved = true;
		}

		// write tokens file
		if (this.tokensDirty) {
			new PrivilegeTokensSaxWriter(getAllAccessTokens(), this.tokensPath).write();
			this.tokensDirty = false;
			saved = true;
		}

		long tookNanos = System.nanoTime() - start;
		if (TimeUnit.NANOSECONDS.toMillis(tookNanos) > 100)
			logger.warn("Persist took {}", formatNanoDuration(tookNanos));
		return saved;
	}

	protected String evaluateUsername(String username) {
		return this.caseInsensitiveUsername ? username.toLowerCase() : username;
	}
}
