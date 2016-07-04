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

import java.io.File;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.helper.XmlConstants;
import li.strolch.privilege.model.internal.Role;
import li.strolch.privilege.model.internal.User;
import li.strolch.privilege.xml.PrivilegeRolesDomWriter;
import li.strolch.privilege.xml.PrivilegeRolesSaxReader;
import li.strolch.privilege.xml.PrivilegeUsersDomWriter;
import li.strolch.privilege.xml.PrivilegeUsersSaxReader;
import li.strolch.utils.helper.StringHelper;
import li.strolch.utils.helper.XmlHelper;

/**
 * {@link PersistenceHandler} implementation which reads the configuration from XML files. These configuration is passed
 * in {@link #initialize(Map)}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class XmlPersistenceHandler implements PersistenceHandler {

	protected static final Logger logger = LoggerFactory.getLogger(XmlPersistenceHandler.class);

	private Map<String, User> userMap;
	private Map<String, Role> roleMap;

	private boolean userMapDirty;
	private boolean roleMapDirty;

	private Map<String, String> parameterMap;

	private long usersFileDate;
	private long rolesFileDate;
	private File usersPath;
	private File rolesPath;

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
		return this.userMap.get(username);
	}

	@Override
	public Role getRole(String roleName) {
		return this.roleMap.get(roleName);
	}

	@Override
	public User removeUser(String username) {
		User user = this.userMap.remove(username);
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
		if (this.userMap.containsKey(user.getUsername()))
			throw new IllegalStateException(MessageFormat.format("The user {0} already exists!", user.getUsername()));
		this.userMap.put(user.getUsername(), user);
		this.userMapDirty = true;
	}

	@Override
	public void replaceUser(User user) {
		if (!this.userMap.containsKey(user.getUsername()))
			throw new IllegalStateException(MessageFormat
					.format("The user {0} can not be replaced as it does not exiset!", user.getUsername()));
		this.userMap.put(user.getUsername(), user);
		this.userMapDirty = true;
	}

	@Override
	public void addRole(Role role) {
		if (this.roleMap.containsKey(role.getName()))
			throw new IllegalStateException(MessageFormat.format("The role {0} already exists!", role.getName()));
		this.roleMap.put(role.getName(), role);
		this.roleMapDirty = true;
	}

	@Override
	public void replaceRole(Role role) {
		if (!this.roleMap.containsKey(role.getName()))
			throw new IllegalStateException(
					MessageFormat.format("The role {0} can not be replaced as it does not exist!", role.getName()));
		this.roleMap.put(role.getName(), role);
		this.roleMapDirty = true;
	}

	/**
	 * Initializes this {@link XmlPersistenceHandler} by reading the following parameters:
	 * <ul>
	 * <li>{@link XmlConstants#XML_PARAM_BASE_PATH}</li>
	 * <li>{@link XmlConstants#XML_PARAM_MODEL_FILE}</li>
	 * </ul>
	 */
	@Override
	public void initialize(Map<String, String> paramsMap) {

		// copy parameter map
		this.parameterMap = Collections.unmodifiableMap(new HashMap<>(paramsMap));

		// get and validate base bath
		String basePath = this.parameterMap.get(XmlConstants.XML_PARAM_BASE_PATH);
		File basePathF = new File(basePath);
		if (!basePathF.exists() && !basePathF.isDirectory()) {
			String msg = "[{0}] Defined parameter {1} does not point to a valid path at {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PersistenceHandler.class.getName(), XmlConstants.XML_PARAM_BASE_PATH,
					basePathF.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		// get users file name
		String usersFileName = this.parameterMap.get(XmlConstants.XML_PARAM_USERS_FILE);
		if (StringHelper.isEmpty(usersFileName)) {
			String msg = "[{0}] Defined parameter {1} is not valid as it is empty!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PersistenceHandler.class.getName(), XmlConstants.XML_PARAM_USERS_FILE);
			throw new PrivilegeException(msg);
		}

		// get roles file name
		String rolesFileName = this.parameterMap.get(XmlConstants.XML_PARAM_ROLES_FILE);
		if (StringHelper.isEmpty(rolesFileName)) {
			String msg = "[{0}] Defined parameter {1} is not valid as it is empty!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PersistenceHandler.class.getName(), XmlConstants.XML_PARAM_ROLES_FILE);
			throw new PrivilegeException(msg);
		}

		// validate users file exists
		String usersPathS = basePath + "/" + usersFileName; //$NON-NLS-1$
		File usersPath = new File(usersPathS);
		if (!usersPath.exists()) {
			String msg = "[{0}] Defined parameter {1} is invalid as users file does not exist at path {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PersistenceHandler.class.getName(), XmlConstants.XML_PARAM_USERS_FILE,
					usersPath.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		// validate roles file exists
		String rolesPathS = basePath + "/" + rolesFileName; //$NON-NLS-1$
		File rolesPath = new File(rolesPathS);
		if (!rolesPath.exists()) {
			String msg = "[{0}] Defined parameter {1} is invalid as roles file does not exist at path {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PersistenceHandler.class.getName(), XmlConstants.XML_PARAM_ROLES_FILE,
					rolesPath.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		// save path to model
		this.usersPath = usersPath;
		this.rolesPath = rolesPath;

		if (reload())
			logger.info("Privilege Data loaded."); //$NON-NLS-1$
	}

	/**
	 * Reads the XML configuration files which contain the model. Which configuration files are parsed was defined in
	 * the while calling {@link #initialize(Map)}
	 * 
	 * @see #initialize(Map)
	 */
	@Override
	public boolean reload() {

		this.roleMap = Collections.synchronizedMap(new HashMap<String, Role>());
		this.userMap = Collections.synchronizedMap(new HashMap<String, User>());

		// parse models xml file to XML document
		PrivilegeUsersSaxReader usersXmlHandler = new PrivilegeUsersSaxReader();
		XmlHelper.parseDocument(this.usersPath, usersXmlHandler);

		PrivilegeRolesSaxReader rolesXmlHandler = new PrivilegeRolesSaxReader();
		XmlHelper.parseDocument(this.rolesPath, rolesXmlHandler);

		this.usersFileDate = this.usersPath.lastModified();
		this.rolesFileDate = this.rolesPath.lastModified();

		// ROLES
		List<Role> roles = rolesXmlHandler.getRoles();
		for (Role role : roles) {
			this.roleMap.put(role.getName(), role);
		}

		// USERS
		List<User> users = usersXmlHandler.getUsers();
		for (User user : users) {
			this.userMap.put(user.getUsername(), user);
		}

		this.userMapDirty = false;
		this.roleMapDirty = false;

		logger.info(MessageFormat.format("Read {0} Users", this.userMap.size())); //$NON-NLS-1$
		logger.info(MessageFormat.format("Read {0} Roles", this.roleMap.size())); //$NON-NLS-1$

		// validate referenced roles exist
		for (User user : users) {
			for (String roleName : user.getRoles()) {

				// validate that role exists
				if (getRole(roleName) == null) {
					String msg = "Role {0} does not exist referenced by user {1}";
					msg = MessageFormat.format(msg, roleName, user.getUsername());
					throw new PrivilegeException(msg);
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

		// get users file name
		String usersFileName = this.parameterMap.get(XmlConstants.XML_PARAM_USERS_FILE);
		if (usersFileName == null || usersFileName.isEmpty()) {
			String msg = "[{0}] Defined parameter {1} is invalid"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PersistenceHandler.class.getName(), XmlConstants.XML_PARAM_USERS_FILE);
			throw new PrivilegeException(msg);
		}

		// get roles file name
		String rolesFileName = this.parameterMap.get(XmlConstants.XML_PARAM_ROLES_FILE);
		if (rolesFileName == null || rolesFileName.isEmpty()) {
			String msg = "[{0}] Defined parameter {1} is invalid"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PersistenceHandler.class.getName(), XmlConstants.XML_PARAM_ROLES_FILE);
			throw new PrivilegeException(msg);
		}

		boolean saved = false;

		// get users file
		boolean usersFileUnchanged = this.usersPath.exists() && this.usersPath.lastModified() == this.usersFileDate;
		if (usersFileUnchanged && !this.userMapDirty) {
			logger.warn("Not persisting of users as current file is unchanged and users data is not dirty"); //$NON-NLS-1$
		} else {

			// delegate writing
			PrivilegeUsersDomWriter modelWriter = new PrivilegeUsersDomWriter(getAllUsers(), this.usersPath);
			modelWriter.write();

			this.userMapDirty = false;
			saved = true;
		}

		// get roles file
		boolean rolesFileUnchanged = this.rolesPath.exists() && this.rolesPath.lastModified() == this.rolesFileDate;
		if (rolesFileUnchanged && !this.roleMapDirty) {
			logger.warn("Not persisting of roles as current file is unchanged and roles data is not dirty"); //$NON-NLS-1$
		} else {

			// delegate writing
			PrivilegeRolesDomWriter modelWriter = new PrivilegeRolesDomWriter(getAllRoles(), this.rolesPath);
			modelWriter.write();

			this.roleMapDirty = false;
			saved = true;
		}

		// reset dirty states

		return saved;
	}
}
