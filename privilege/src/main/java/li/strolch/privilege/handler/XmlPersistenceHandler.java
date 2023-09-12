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
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.xml.PrivilegeRolesDomWriter;
import li.strolch.privilege.xml.PrivilegeRolesSaxReader;
import li.strolch.privilege.xml.PrivilegeUsersDomWriter;
import li.strolch.privilege.xml.PrivilegeUsersSaxReader;
import li.strolch.utils.helper.XmlHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

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
	private final Map<String, Role> roleMap;

	private boolean userMapDirty;
	private boolean roleMapDirty;

	private Map<String, String> parameterMap;

	private File usersPath;
	private File rolesPath;

	private boolean caseInsensitiveUsername;

	public XmlPersistenceHandler() {
		this.roleMap = new ConcurrentHashMap<>();
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
	public List<Role> getAllRoles() {
		synchronized (this.roleMap) {
			return new LinkedList<>(this.roleMap.values());
		}
	}

	@Override
	public User getUser(String username) {
		return this.userMap.get(this.caseInsensitiveUsername ? username.toLowerCase() : username);
	}

	@Override
	public Role getRole(String roleName) {
		return this.roleMap.get(roleName);
	}

	@Override
	public User removeUser(String username) {
		User user = this.userMap.remove(this.caseInsensitiveUsername ? username.toLowerCase() : username);
		this.userMapDirty = user != null;
		return user;
	}

	@Override
	public Role removeRole(String roleName) {
		Role role = this.roleMap.remove(roleName);
		this.roleMapDirty = role != null;
		return role;
	}

	@Override
	public void addUser(User user) {
		String username = this.caseInsensitiveUsername ? user.getUsername().toLowerCase() : user.getUsername();
		if (this.userMap.containsKey(username))
			throw new IllegalStateException(format("The user {0} already exists!", user.getUsername()));
		this.userMap.put(username, user);
		this.userMapDirty = true;
	}

	@Override
	public void replaceUser(User user) {
		String username = this.caseInsensitiveUsername ? user.getUsername().toLowerCase() : user.getUsername();
		if (!this.userMap.containsKey(username))
			throw new IllegalStateException(
					format("The user {0} can not be replaced as it does not exist!", user.getUsername()));
		this.userMap.put(username, user);
		this.userMapDirty = true;
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
	 * <li>{@link XmlConstants#XML_PARAM_BASE_PATH}</li>
	 * <li>{@link XmlConstants#XML_PARAM_USERS_FILE}</li>
	 * <li>{@link XmlConstants#XML_PARAM_ROLES_FILE}</li>
	 * </ul>
	 */
	@Override
	public void initialize(Map<String, String> paramsMap) {
		this.parameterMap = Map.copyOf(paramsMap);

		// get and validate base bath
		String basePath = this.parameterMap.get(XML_PARAM_BASE_PATH);
		File basePathF = new File(basePath);
		if (!basePathF.exists() && !basePathF.isDirectory()) {
			String msg = "[{0}] Defined parameter {1} does not point to a valid path at {2}";
			msg = format(msg, PersistenceHandler.class.getName(), XML_PARAM_BASE_PATH, basePathF.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		// get users file path
		File usersPath = getFile(basePath, XML_PARAM_USERS_FILE);

		// get roles file path
		File rolesPath = getFile(basePath, XML_PARAM_ROLES_FILE);

		// save path to model
		this.usersPath = usersPath;
		this.rolesPath = rolesPath;

		this.caseInsensitiveUsername = Boolean.parseBoolean(this.parameterMap.get(PARAM_CASE_INSENSITIVE_USERNAME));

		if (reload())
			logger.info("Privilege Data loaded.");
	}

	private File getFile(String basePath, String param) {
		String fileName = this.parameterMap.get(param);
		if (isEmpty(fileName)) {
			String msg = "[{0}] Defined parameter {1} is not valid as it is empty!";
			msg = format(msg, PersistenceHandler.class.getName(), param);
			throw new PrivilegeException(msg);
		}

		String path = basePath + "/" + fileName;
		File file = new File(path);
		if (!file.exists()) {
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

		PrivilegeRolesSaxReader rolesXmlHandler = new PrivilegeRolesSaxReader();
		XmlHelper.parseDocument(this.rolesPath, rolesXmlHandler);

		// ROLES
		synchronized (this.roleMap) {
			this.roleMap.clear();
			this.roleMap.putAll(rolesXmlHandler.getRoles());
		}

		// USERS
		synchronized (this.userMap) {
			this.userMap.clear();
			this.userMap.putAll(usersXmlHandler.getUsers());
		}

		this.userMapDirty = false;
		this.roleMapDirty = false;

		logger.info(format("Read {0} Users", this.userMap.size()));
		logger.info(format("Read {0} Roles", this.roleMap.size()));

		// validate referenced roles exist
		for (User user : this.userMap.values()) {
			for (String roleName : user.getRoles()) {

				// validate that role exists
				if (getRole(roleName) == null) {
					logger.error(
							format("Role {0} does not exist referenced by user {1}", roleName, user.getUsername()));
				}
			}
		}

		return true;
	}

	/**
	 * Writes the model to the XML files. Where the files are written to was defined in the {@link #initialize(Map)}
	 */
	@Override
	public boolean persist() {

		long start = System.nanoTime();

		// get users file name
		String usersFileName = this.parameterMap.get(XML_PARAM_USERS_FILE);
		if (usersFileName == null || usersFileName.isEmpty()) {
			String msg = "[{0}] Defined parameter {1} is invalid";
			msg = format(msg, PersistenceHandler.class.getName(), XML_PARAM_USERS_FILE);
			throw new PrivilegeException(msg);
		}

		// get roles file name
		String rolesFileName = this.parameterMap.get(XML_PARAM_ROLES_FILE);
		if (rolesFileName == null || rolesFileName.isEmpty()) {
			String msg = "[{0}] Defined parameter {1} is invalid";
			msg = format(msg, PersistenceHandler.class.getName(), XML_PARAM_ROLES_FILE);
			throw new PrivilegeException(msg);
		}

		boolean saved = false;

		// get users file
		if (this.userMapDirty) {
			// delegate writing
			PrivilegeUsersDomWriter modelWriter = new PrivilegeUsersDomWriter(getAllUsers(), this.usersPath);
			modelWriter.write();

			this.userMapDirty = false;
			saved = true;
		}

		// get roles file
		if (this.roleMapDirty) {
			// delegate writing
			PrivilegeRolesDomWriter modelWriter = new PrivilegeRolesDomWriter(getAllRoles(), this.rolesPath);
			modelWriter.write();

			this.roleMapDirty = false;
			saved = true;
		}

		logger.info("Persist took " + (formatNanoDuration(System.nanoTime() - start)));
		return saved;
	}
}
