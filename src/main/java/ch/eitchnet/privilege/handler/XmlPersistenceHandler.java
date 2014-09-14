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

import java.io.File;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.helper.XmlConstants;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.privilege.xml.PrivilegeModelDomWriter;
import ch.eitchnet.privilege.xml.PrivilegeModelSaxReader;
import ch.eitchnet.utils.helper.XmlHelper;

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

	private long modelsFileDate;
	private boolean userMapDirty;
	private boolean roleMapDirty;

	private Map<String, String> parameterMap;

	private File modelPath;

	@Override
	public List<User> getAllUsers() {
		synchronized (this.userMap) {
			return new LinkedList<User>(this.userMap.values());
		}
	}

	@Override
	public List<Role> getAllRoles() {
		synchronized (this.roleMap) {
			return new LinkedList<Role>(this.roleMap.values());
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
	public void addOrReplaceUser(User user) {
		this.userMap.put(user.getUsername(), user);
		this.userMapDirty = true;
	}

	@Override
	public void addOrReplaceRole(Role role) {
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
		this.parameterMap = Collections.unmodifiableMap(new HashMap<String, String>(paramsMap));

		// get and validate base bath
		String basePath = this.parameterMap.get(XmlConstants.XML_PARAM_BASE_PATH);
		File basePathF = new File(basePath);
		if (!basePathF.exists() && !basePathF.isDirectory()) {
			String msg = "[{0}] Defined parameter {1} does not point to a valid path at {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PersistenceHandler.class.getName(), XmlConstants.XML_PARAM_BASE_PATH,
					basePathF.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		// get model file name
		String modelFileName = this.parameterMap.get(XmlConstants.XML_PARAM_MODEL_FILE);
		if (modelFileName == null || modelFileName.isEmpty()) {
			String msg = "[{0}] Defined parameter {1} is not valid as it is empty!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PersistenceHandler.class.getName(), XmlConstants.XML_PARAM_MODEL_FILE);
			throw new PrivilegeException(msg);
		}

		// validate file exists
		String modelPathS = basePath + "/" + modelFileName; //$NON-NLS-1$
		File modelPath = new File(modelPathS);
		if (!modelPath.exists()) {
			String msg = "[{0}] Defined parameter {1} is invalid as model file does not exist at path {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PersistenceHandler.class.getName(), XmlConstants.XML_PARAM_MODEL_FILE,
					modelPath.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		// save path to model
		this.modelPath = modelPath;

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
		PrivilegeModelSaxReader xmlHandler = new PrivilegeModelSaxReader();
		XmlHelper.parseDocument(this.modelPath, xmlHandler);

		this.modelsFileDate = this.modelPath.lastModified();

		// ROLES
		List<Role> roles = xmlHandler.getRoles();
		for (Role role : roles) {
			this.roleMap.put(role.getName(), role);
		}

		// USERS
		List<User> users = xmlHandler.getUsers();
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

		// validate we have a user with PrivilegeAdmin access
		boolean privilegeAdminExists = false;
		for (String username : this.userMap.keySet()) {
			User user = this.userMap.get(username);
			if (user.hasRole(PrivilegeHandler.PRIVILEGE_ADMIN_ROLE)) {
				privilegeAdminExists = true;
				break;
			}
		}

		if (!privilegeAdminExists) {
			String msg = "No User with role ''{0}'' exists. Privilege modifications will not be possible!"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PrivilegeHandler.PRIVILEGE_ADMIN_ROLE);
			logger.warn(msg);
		}

		return true;
	}

	/**
	 * Writes the model to the XML files. Where the files are written to was defined in the {@link #initialize(Map)}
	 */
	@Override
	public boolean persist() {

		// get models file name
		String modelFileName = this.parameterMap.get(XmlConstants.XML_PARAM_MODEL_FILE);
		if (modelFileName == null || modelFileName.isEmpty()) {
			String msg = "[{0}] Defined parameter {1} is invalid"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, PersistenceHandler.class.getName(), XmlConstants.XML_PARAM_MODEL_FILE);
			throw new PrivilegeException(msg);
		}

		// get model file
		boolean modelFileUnchanged = this.modelPath.exists() && this.modelPath.lastModified() == this.modelsFileDate;
		if (modelFileUnchanged && !this.roleMapDirty && !this.userMapDirty) {
			logger.warn("Not persisting as current file is unchanged and model data is not dirty"); //$NON-NLS-1$
			return false;
		}

		// delegate writing
		PrivilegeModelDomWriter modelWriter = new PrivilegeModelDomWriter(getAllUsers(), getAllRoles(), this.modelPath);
		modelWriter.write();

		// reset dirty states
		this.userMapDirty = false;
		this.roleMapDirty = false;

		return true;
	}
}
