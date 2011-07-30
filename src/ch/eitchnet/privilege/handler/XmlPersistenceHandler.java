/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.handler;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.dom4j.DocumentFactory;
import org.dom4j.Element;

import ch.eitchnet.privilege.helper.XmlConstants;
import ch.eitchnet.privilege.helper.XmlHelper;
import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.User;

/**
 * {@link PersistenceHandler} implementation which reads the configuration from XML files. These configuration is passed
 * in {@link #initialize(Map)}
 * 
 * @author rvonburg
 */
public class XmlPersistenceHandler implements PersistenceHandler {

	private static final Logger logger = Logger.getLogger(XmlPersistenceHandler.class);

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
		if (!(modelFileUnchanged && this.roleMapDirty && this.userMapDirty)) {
			logger.warn("Not persisting as current file is unchanged and model data is not dirty");
			return false;
		}

		DocumentFactory docFactory = DocumentFactory.getInstance();

		// create root element
		Element rootElement = docFactory.createElement(XmlConstants.XML_ROOT_PRIVILEGE_USERS_AND_ROLES);

		// USERS
		// build XML DOM of users
		List<Element> users = toDomUsers();
		Element usersElement = docFactory.createElement(XmlConstants.XML_USERS);
		for (Element userElement : users) {
			usersElement.add(userElement);
		}
		rootElement.add(usersElement);

		// ROLES
		// build XML DOM of roles
		List<Element> roles = toDomRoles();
		Element rolesElement = docFactory.createElement(XmlConstants.XML_ROLES);
		for (Element roleElement : roles) {
			rolesElement.add(roleElement);
		}
		rootElement.add(rolesElement);

		// reset dirty states and return if something was dirty, false otherwise
		if (this.userMapDirty || this.roleMapDirty) {
			this.userMapDirty = false;
			this.roleMapDirty = false;

			return true;

		}

		this.userMapDirty = false;
		this.roleMapDirty = false;

		return false;
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
		Element modelsRootElement = XmlHelper.parseDocument(modelsFile).getRootElement();
		this.modelsFileDate = modelsFile.lastModified();

		// ROLES
		// get roles element
		Element rolesElement = modelsRootElement.element(XmlConstants.XML_ROLES);
		// read roles
		readRoles(rolesElement);

		// USERS
		// get users element
		Element usersElement = modelsRootElement.element(XmlConstants.XML_USERS);
		// read users
		readUsers(usersElement);

		this.userMapDirty = false;
		this.roleMapDirty = false;

		logger.info("Read " + this.userMap.size() + " Users");
		logger.info("Read " + this.roleMap.size() + " Roles");

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
			logger.warn("No User with role '" + PrivilegeHandler.PRIVILEGE_ADMIN_ROLE
					+ "' exists. Privilege modifications will not be possible!");
		}

		return true;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.PersistenceHandler#initialize(java.util.Map)
	 */
	@Override
	public void initialize(Map<String, String> parameterMap) {

		// get and validate base bath
		String basePath = parameterMap.get(XmlConstants.XML_PARAM_BASE_PATH);
		File basePathF = new File(basePath);
		if (!basePathF.exists() && !basePathF.isDirectory()) {
			throw new PrivilegeException("[" + PersistenceHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_BASE_PATH + " is invalid");
		}

		// get model file name
		String modelFileName = parameterMap.get(XmlConstants.XML_PARAM_MODEL_FILE);
		if (modelFileName == null || modelFileName.isEmpty()) {
			throw new PrivilegeException("[" + PersistenceHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_MODEL_FILE + " is invalid");
		}

		// save path to model
		this.modelPath = basePath + "/" + modelFileName;

		if (reload())
			logger.info("Privilege Data loaded.");
	}

	/**
	 * @param usersRootElement
	 */
	private void readUsers(Element usersRootElement) {

		@SuppressWarnings("unchecked")
		List<Element> userElements = usersRootElement.elements(XmlConstants.XML_USER);
		for (Element userElement : userElements) {

			String userId = userElement.attributeValue(XmlConstants.XML_ATTR_USER_ID);

			String username = userElement.attributeValue(XmlConstants.XML_ATTR_USERNAME);
			String password = userElement.attributeValue(XmlConstants.XML_ATTR_PASSWORD);

			String firstname = userElement.element(XmlConstants.XML_FIRSTNAME).getTextTrim();
			String surname = userElement.element(XmlConstants.XML_SURNAME).getTextTrim();

			UserState userState = UserState.valueOf(userElement.element(XmlConstants.XML_STATE).getTextTrim());

			// TODO better parsing needed
			String localeName = userElement.element(XmlConstants.XML_LOCALE).getTextTrim();
			Locale locale = new Locale(localeName);

			Element rolesElement = userElement.element(XmlConstants.XML_ROLES);
			@SuppressWarnings("unchecked")
			List<Element> rolesElementList = rolesElement.elements(XmlConstants.XML_ROLE);
			Set<String> roles = new HashSet<String>();
			for (Element roleElement : rolesElementList) {
				String roleName = roleElement.getTextTrim();
				if (roleName.isEmpty()) {
					logger.error("User " + username + " has a role defined with no name, Skipped.");
				} else {
					roles.add(roleName);
				}
			}

			// create user
			User user = new User(userId, username, password, firstname, surname, userState,
					Collections.unmodifiableSet(roles), locale);
			logger.info("Added user " + user);

			// put user in map
			this.userMap.put(username, user);
		}
	}

	/**
	 * @param rolesRootElement
	 */
	private void readRoles(Element rolesRootElement) {

		@SuppressWarnings("unchecked")
		List<Element> roleElements = rolesRootElement.elements(XmlConstants.XML_ROLE);
		for (Element roleElement : roleElements) {

			String roleName = roleElement.attributeValue(XmlConstants.XML_ATTR_NAME);

			Map<String, Privilege> privilegeMap = readPrivileges(roleElement);

			Role role = new Role(roleName, privilegeMap);
			this.roleMap.put(roleName, role);
		}
	}

	/**
	 * @param roleParentElement
	 * @return
	 */
	private Map<String, Privilege> readPrivileges(Element roleParentElement) {

		Map<String, Privilege> privilegeMap = new HashMap<String, Privilege>();

		@SuppressWarnings("unchecked")
		List<Element> privilegeElements = roleParentElement.elements(XmlConstants.XML_PRIVILEGE);
		for (Element privilegeElement : privilegeElements) {

			String privilegeName = privilegeElement.attributeValue(XmlConstants.XML_ATTR_NAME);
			String privilegePolicy = privilegeElement.attributeValue(XmlConstants.XML_ATTR_POLICY);

			String allAllowedS = privilegeElement.element(XmlConstants.XML_ALL_ALLOWED).getTextTrim();
			boolean allAllowed = Boolean.valueOf(allAllowedS).booleanValue();

			@SuppressWarnings("unchecked")
			List<Element> denyElements = privilegeElement.elements(XmlConstants.XML_DENY);
			Set<String> denyList = new HashSet<String>(denyElements.size());
			for (Element denyElement : denyElements) {
				String denyValue = denyElement.getTextTrim();
				if (!denyValue.isEmpty())
					denyList.add(denyValue);
			}

			@SuppressWarnings("unchecked")
			List<Element> allowElements = privilegeElement.elements(XmlConstants.XML_ALLOW);
			Set<String> allowList = new HashSet<String>(allowElements.size());
			for (Element allowElement : allowElements) {
				String allowValue = allowElement.getTextTrim();
				if (!allowValue.isEmpty())
					allowList.add(allowValue);
			}

			Privilege privilege = new Privilege(privilegeName, privilegePolicy, allAllowed, denyList, allowList);
			privilegeMap.put(privilegeName, privilege);
		}

		return privilegeMap;
	}

	private List<Element> toDomUsers() {

		List<Element> usersAsElements = new ArrayList<Element>(this.userMap.size());

		DocumentFactory documentFactory = DocumentFactory.getInstance();

		synchronized (this.userMap) {
			for (String userName : this.userMap.keySet()) {

				// get the user object
				User user = this.userMap.get(userName);

				// create the user element
				Element userElement = documentFactory.createElement(XmlConstants.XML_USER);
				userElement.addAttribute(XmlConstants.XML_ATTR_USER_ID, user.getUserId());
				userElement.addAttribute(XmlConstants.XML_ATTR_USERNAME, user.getUsername());
				userElement.addAttribute(XmlConstants.XML_ATTR_PASSWORD, user.getPassword());

				// add first name element
				Element firstnameElement = documentFactory.createElement(XmlConstants.XML_FIRSTNAME);
				firstnameElement.setText(user.getFirstname());
				userElement.add(firstnameElement);

				// add surname element
				Element surnameElement = documentFactory.createElement(XmlConstants.XML_SURNAME);
				surnameElement.setText(user.getSurname());
				userElement.add(surnameElement);

				// add state element
				Element stateElement = documentFactory.createElement(XmlConstants.XML_STATE);
				stateElement.setText(user.getUserState().toString());
				userElement.add(stateElement);

				// add locale element
				Element localeElement = documentFactory.createElement(XmlConstants.XML_LOCALE);
				localeElement.setText(user.getLocale().toString());
				userElement.add(localeElement);

				// add all the role elements
				Element rolesElement = documentFactory.createElement(XmlConstants.XML_ROLES);
				userElement.add(rolesElement);
				for (String roleName : user.getRoles()) {
					Element roleElement = documentFactory.createElement(XmlConstants.XML_ROLE);
					roleElement.setText(roleName);
					rolesElement.add(roleElement);
				}

				// add element to return list
				usersAsElements.add(userElement);
			}
		}

		return usersAsElements;
	}

	private List<Element> toDomRoles() {

		List<Element> rolesAsElements = new ArrayList<Element>(this.roleMap.size());

		DocumentFactory documentFactory = DocumentFactory.getInstance();

		synchronized (this.roleMap) {
			for (String roleName : this.roleMap.keySet()) {

				// get the role object
				Role role = this.roleMap.get(roleName);

				// create the role element
				Element roleElement = documentFactory.createElement(XmlConstants.XML_ROLE);
				roleElement.addAttribute(XmlConstants.XML_ATTR_NAME, role.getName());

				// add all the privileges
				toDomPrivileges(roleElement, role.getPrivilegeMap());

				// add element to return list
				rolesAsElements.add(roleElement);
			}
		}

		return rolesAsElements;
	}

	private void toDomPrivileges(Element roleParentElement, Map<String, Privilege> privilegeMap) {

		DocumentFactory documentFactory = DocumentFactory.getInstance();

		for (Privilege privilege : privilegeMap.values()) {

			// create the privilege element
			Element privilegeElement = documentFactory.createElement(XmlConstants.XML_PRIVILEGE);
			privilegeElement.addAttribute(XmlConstants.XML_ATTR_NAME, privilege.getName());
			privilegeElement.addAttribute(XmlConstants.XML_ATTR_POLICY, privilege.getPolicy());

			// add the all allowed element
			Element allAllowedElement = documentFactory.createElement(XmlConstants.XML_ALL_ALLOWED);
			allAllowedElement.setText(Boolean.toString(privilege.isAllAllowed()));
			privilegeElement.add(allAllowedElement);

			// add all the deny values
			for (String denyValue : privilege.getDenyList()) {
				Element denyValueElement = documentFactory.createElement(XmlConstants.XML_DENY);
				denyValueElement.setText(denyValue);
				privilegeElement.add(denyValueElement);
			}

			// add all the allow values
			for (String allowValue : privilege.getAllowList()) {
				Element allowValueElement = documentFactory.createElement(XmlConstants.XML_ALLOW);
				allowValueElement.setText(allowValue);
				privilegeElement.add(allowValueElement);
			}

			// add element to parent
			roleParentElement.add(privilegeElement);
		}
	}
}
