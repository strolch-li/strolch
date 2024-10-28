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

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.helper.XmlConstants;
import li.strolch.privilege.model.Group;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.xml.*;
import li.strolch.utils.helper.XmlHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.stream.XMLStreamException;
import java.io.File;
import java.io.IOException;
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

	private final Map<String, User> userMap;
	private final Map<String, Group> groupMap;
	private final Map<String, Role> roleMap;

	private boolean userMapDirty;
	private boolean groupMapDirty;
	private boolean roleMapDirty;

	private Map<String, String> parameterMap;

	private File usersPath;
	private File groupsPath;
	private File rolesPath;

	private boolean caseInsensitiveUsername;

	public XmlPersistenceHandler() {
		this.roleMap = new ConcurrentHashMap<>();
		this.groupMap = new ConcurrentHashMap<>();
		this.userMap = new ConcurrentHashMap<>();
	}

	@Override
	public Map<String, String> getParameterMap() {
		return this.parameterMap;
	}

	@Override
	public List<User> getAllUsers() {
		synchronized (this.userMap) {
			return new LinkedList<>(this.userMap.values());
		}
	}

	@Override
	public List<Group> getAllGroups() {
		synchronized (this.groupMap) {
			return new LinkedList<>(this.groupMap.values());
		}
	}

	@Override
	public List<Role> getAllRoles() {
		synchronized (this.roleMap) {
			return new LinkedList<>(this.roleMap.values());
		}
	}

	@Override
	public boolean hasUser(String username) {
		return this.userMap.containsKey(evaluateUsername(username));
	}

	@Override
	public User getUser(String username) {
		return this.userMap.get(evaluateUsername(username));
	}

	@Override
	public Group getGroup(String groupName) {
		return this.groupMap.get(groupName);
	}

	@Override
	public Role getRole(String roleName) {
		return this.roleMap.get(roleName);
	}

	@Override
	public User removeUser(String username) {
		User user = this.userMap.remove(evaluateUsername(username));
		this.userMapDirty = user != null;
		return user;
	}

	@Override
	public Group removeGroup(String groupName) {
		Group group = this.groupMap.remove(groupName);
		this.groupMapDirty = group != null;
		return group;
	}

	@Override
	public Role removeRole(String roleName) {
		Role role = this.roleMap.remove(roleName);
		this.roleMapDirty = role != null;
		return role;
	}

	@Override
	public void addUser(User user) {
		String username = evaluateUsername(user.getUsername());
		if (this.userMap.containsKey(username))
			throw new IllegalStateException(format("The user {0} already exists!", user.getUsername()));
		this.userMap.put(username, user);
		this.userMapDirty = true;
	}

	@Override
	public void replaceUser(User user) {
		String username = evaluateUsername(user.getUsername());
		if (!this.userMap.containsKey(username))
			throw new IllegalStateException(
					format("The user {0} can not be replaced as it does not exist!", user.getUsername()));
		this.userMap.put(username, user);
		this.userMapDirty = true;
	}

	@Override
	public void addGroup(Group group) {
		if (this.groupMap.containsKey(group.name()))
			throw new IllegalStateException(format("The group {0} already exists!", group.name()));
		this.groupMap.put(group.name(), group);
		this.groupMapDirty = true;
	}

	@Override
	public void replaceGroup(Group group) {
		if (!this.groupMap.containsKey(group.name()))
			throw new IllegalStateException(
					format("The group {0} can not be replaced as it does not exist!", group.name()));
		this.groupMap.put(group.name(), group);
		this.groupMapDirty = true;
	}

	@Override
	public void addRole(Role role) {
		if (this.roleMap.containsKey(role.getName()))
			throw new IllegalStateException(format("The role {0} already exists!", role.getName()));
		this.roleMap.put(role.getName(), role);
		this.roleMapDirty = true;
	}

	@Override
	public void replaceRole(Role role) {
		if (!this.roleMap.containsKey(role.getName()))
			throw new IllegalStateException(
					format("The role {0} can not be replaced as it does not exist!", role.getName()));
		this.roleMap.put(role.getName(), role);
		this.roleMapDirty = true;
	}

	/**
	 * Initializes this {@link XmlPersistenceHandler} by reading the following parameters:
	 * <ul>
	 * <li>{@link XmlConstants#PARAM_BASE_PATH}</li>
	 * <li>{@link XmlConstants#PARAM_USERS_FILE}</li>
	 * <li>{@link XmlConstants#PARAM_GROUPS_FILE}</li>
	 * <li>{@link XmlConstants#PARAM_ROLES_FILE}</li>
	 * </ul>
	 */
	@Override
	public void initialize(Map<String, String> paramsMap) {
		this.parameterMap = Map.copyOf(paramsMap);

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

		// save path to model
		this.usersPath = usersPath;
		this.groupsPath = groupsPath;
		this.rolesPath = rolesPath;

		this.caseInsensitiveUsername = parseBoolean(
				this.parameterMap.getOrDefault(PARAM_CASE_INSENSITIVE_USERNAME, "true"));

		if (reload())
			logger.info("Privilege Data loaded.");
	}

	private File getFile(String basePath, String param, String defaultValue, boolean required) {
		String fileName = this.parameterMap.get(param);
		if (isEmpty(fileName)) {
			fileName = defaultValue;
			String msg = "[{0}] Parameter {1} is not defined, using default {2}!";
			msg = format(msg, PersistenceHandler.class.getName(), param, defaultValue);
			logger.warn(msg);
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
		PrivilegeUsersSaxReader usersXmlHandler = new PrivilegeUsersSaxReader(this.caseInsensitiveUsername);
		XmlHelper.parseDocument(this.usersPath, usersXmlHandler);

		PrivilegeGroupsSaxReader groupsXmlHandler = new PrivilegeGroupsSaxReader();
		if (this.groupsPath.exists())
			XmlHelper.parseDocument(this.groupsPath, groupsXmlHandler);

		PrivilegeRolesSaxReader rolesXmlHandler = new PrivilegeRolesSaxReader();
		XmlHelper.parseDocument(this.rolesPath, rolesXmlHandler);

		// ROLES
		synchronized (this.roleMap) {
			this.roleMap.clear();
			this.roleMap.putAll(rolesXmlHandler.getRoles());
		}

		// GROUPS
		synchronized (this.groupMap) {
			this.groupMap.clear();
			this.groupMap.putAll(groupsXmlHandler.getGroups());
		}

		// USERS
		synchronized (this.userMap) {
			this.userMap.clear();
			this.userMap.putAll(usersXmlHandler.getUsers());
		}

		this.userMapDirty = false;
		this.groupMapDirty = false;
		this.roleMapDirty = false;

		logger.info("Read {} Users", this.userMap.size());
		logger.info("Read {} Groups", this.groupMap.size());
		logger.info("Read {} Roles", this.roleMap.size());

		// validate referenced elements exist
		for (User user : this.userMap.values()) {
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
		for (Group group : this.groupMap.values()) {
			for (String roleName : group.roles()) {
				// validate that role exists
				if (getRole(roleName) == null)
					logger.error("Role {} does not exist referenced by group {}", roleName, group.name());
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
		if (this.userMapDirty) {
			new PrivilegeUsersSaxWriter(getAllUsers(), this.usersPath).write();
			this.userMapDirty = false;
			saved = true;
		}

		// write groups file
		if (this.groupMapDirty) {
			new PrivilegeGroupsSaxWriter(getAllGroups(), this.groupsPath).write();
			this.groupMapDirty = false;
			saved = true;
		}

		// write roles file
		if (this.roleMapDirty) {
			new PrivilegeRolesSaxWriter(getAllRoles(), this.rolesPath).write();
			this.roleMapDirty = false;
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
