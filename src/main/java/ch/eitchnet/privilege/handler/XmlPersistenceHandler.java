/*
 * Copyright (c) 2010 - 2012
 * 
 * This file is part of Privilege.
 *
 * Privilege is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Privilege.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.privilege.handler;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.helper.XmlConstants;
import ch.eitchnet.privilege.helper.XmlHelper;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;
import ch.eitchnet.privilege.xml.PrivilegeModelDomWriter;
import ch.eitchnet.privilege.xml.PrivilegeModelSaxReader;

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

	private String modelPath;

	/**
	 * @see ch.eitchnet.privilege.handler.PersistenceHandler#getAllUsers()
	 */
	@Override
	public List<User> getAllUsers() {
		synchronized (this.userMap) {
			return new LinkedList<User>(this.userMap.values());
		}
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PersistenceHandler#getAllRoles()
	 */
	@Override
	public List<Role> getAllRoles() {
		synchronized (this.roleMap) {
			return new LinkedList<Role>(this.roleMap.values());
		}
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PersistenceHandler#getUser(java.lang.String)
	 */
	@Override
	public User getUser(String username) {
		return this.userMap.get(username);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PersistenceHandler#getRole(java.lang.String)
	 */
	@Override
	public Role getRole(String roleName) {
		return this.roleMap.get(roleName);
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PersistenceHandler#removeUser(java.lang.String)
	 */
	@Override
	public User removeUser(String username) {
		User user = this.userMap.remove(username);
		this.userMapDirty = user != null;
		return user;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PersistenceHandler#removeRole(java.lang.String)
	 */
	@Override
	public Role removeRole(String roleName) {
		Role role = this.roleMap.remove(roleName);
		this.roleMapDirty = role != null;
		return role;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PersistenceHandler#addOrReplaceUser(ch.eitchnet.privilege.model.internal.User)
	 */
	@Override
	public void addOrReplaceUser(User user) {
		this.userMap.put(user.getUsername(), user);
		this.userMapDirty = true;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PersistenceHandler#addOrReplaceRole(ch.eitchnet.privilege.model.internal.Role)
	 */
	@Override
	public void addOrReplaceRole(Role role) {
		this.roleMap.put(role.getName(), role);
		this.roleMapDirty = true;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PersistenceHandler#initialize(java.util.Map)
	 */
	@Override
	public void initialize(Map<String, String> paramsMap) {

		// copy parameter map
		this.parameterMap = Collections.unmodifiableMap(new HashMap<String, String>(paramsMap));

		// get and validate base bath
		String basePath = this.parameterMap.get(XmlConstants.XML_PARAM_BASE_PATH);
		File basePathF = new File(basePath);
		if (!basePathF.exists() && !basePathF.isDirectory()) {
			throw new PrivilegeException("[" + PersistenceHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_BASE_PATH + " is invalid");
		}

		// get model file name
		String modelFileName = this.parameterMap.get(XmlConstants.XML_PARAM_MODEL_FILE);
		if (modelFileName == null || modelFileName.isEmpty()) {
			throw new PrivilegeException("[" + PersistenceHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_MODEL_FILE + " is invalid");
		}

		// save path to model
		this.modelPath = basePath + "/" + modelFileName;

		if (reload())
			XmlPersistenceHandler.logger.info("Privilege Data loaded.");
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PersistenceHandler#reload()
	 */
	@Override
	public boolean reload() {

		// validate file exists
		File modelsFile = new File(this.modelPath);
		if (!modelsFile.exists()) {
			throw new PrivilegeException("[" + PersistenceHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_MODEL_FILE + " is invalid as models file does not exist at path "
					+ modelsFile.getAbsolutePath());
		}

		this.roleMap = Collections.synchronizedMap(new HashMap<String, Role>());
		this.userMap = Collections.synchronizedMap(new HashMap<String, User>());

		// parse models xml file to XML document
		PrivilegeModelSaxReader xmlHandler = new PrivilegeModelSaxReader();
		XmlHelper.parseDocument(modelsFile, xmlHandler);

		this.modelsFileDate = modelsFile.lastModified();

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

		XmlPersistenceHandler.logger.info("Read " + this.userMap.size() + " Users");
		XmlPersistenceHandler.logger.info("Read " + this.roleMap.size() + " Roles");

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
			XmlPersistenceHandler.logger.warn("No User with role '" + PrivilegeHandler.PRIVILEGE_ADMIN_ROLE
					+ "' exists. Privilege modifications will not be possible!");
		}

		return true;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PersistenceHandler#persist()
	 */
	@Override
	public boolean persist() {

		// get models file name
		String modelFileName = this.parameterMap.get(XmlConstants.XML_PARAM_MODEL_FILE);
		if (modelFileName == null || modelFileName.isEmpty()) {
			throw new PrivilegeException("[" + PersistenceHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_MODEL_FILE + " is invalid");
		}

		// get model file
		File modelFile = new File(this.modelPath);
		boolean modelFileUnchanged = modelFile.exists() && modelFile.lastModified() == this.modelsFileDate;
		if (modelFileUnchanged && !this.roleMapDirty && !this.userMapDirty) {
			XmlPersistenceHandler.logger
					.warn("Not persisting as current file is unchanged and model data is not dirty");
			return false;
		}

		// delegate writing
		PrivilegeModelDomWriter modelWriter = new PrivilegeModelDomWriter(getAllUsers(), getAllRoles(), modelFile);
		modelWriter.write();

		// reset dirty states
		this.userMapDirty = false;
		this.roleMapDirty = false;

		return true;
	}
}
