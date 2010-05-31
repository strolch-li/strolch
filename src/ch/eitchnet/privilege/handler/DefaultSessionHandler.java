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
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.log4j.Logger;
import org.dom4j.Element;

import ch.eitchnet.privilege.base.PrivilegeContainer;
import ch.eitchnet.privilege.base.XmlConstants;
import ch.eitchnet.privilege.helper.ConfigurationHelper;
import ch.eitchnet.privilege.helper.XmlHelper;
import ch.eitchnet.privilege.i18n.AccessDeniedException;
import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.Restrictable;
import ch.eitchnet.privilege.model.UserState;
import ch.eitchnet.privilege.model.internal.Privilege;
import ch.eitchnet.privilege.model.internal.Role;
import ch.eitchnet.privilege.model.internal.Session;
import ch.eitchnet.privilege.model.internal.User;

/**
 * @author rvonburg
 * 
 */
public class DefaultSessionHandler implements SessionHandler {

	private static final Logger logger = Logger.getLogger(DefaultSessionHandler.class);

	private static long lastSessionId;

	private Map<String, User> userMap;
	private Map<String, Role> roleMap;
	private Map<String, CertificateSessionPair> sessionMap;

	/**
	 * @see ch.eitchnet.privilege.handler.SessionHandler#actionAllowed(ch.eitchnet.privilege.model.Certificate,
	 *      ch.eitchnet.privilege.model.Restrictable)
	 * 
	 * @throws AccessDeniedException
	 *             if the {@link Certificate} is not for a currently logged in {@link User} or if the user may not
	 *             perform the action defined by the {@link Restrictable} implementation
	 */
	@Override
	public boolean actionAllowed(Certificate certificate, Restrictable restrictable) {

		// certificate and restrictable must not be null
		if (certificate == null)
			throw new PrivilegeException("Certificate may not be null!");
		else if (restrictable == null)
			throw new PrivilegeException("Restrictable may not be null!");

		// first see if a session exists for this certificate
		CertificateSessionPair certificateSessionPair = sessionMap.get(certificate.getSessionId());
		if (certificateSessionPair == null)
			throw new AccessDeniedException("There is no session information for " + certificate.toString());

		// validate certificate has not been tampered with
		Certificate sessionCertificate = certificateSessionPair.certificate;
		if (!sessionCertificate.equals(certificate))
			throw new PrivilegeException("Received illegal certificate for session id " + certificate.getSessionId());

		// TODO is validating authToken overkill since the two certificates have already been checked on equality?
		// validate authToken from certificate using the sessions authPassword
		String authToken = certificate.getAuthToken(certificateSessionPair.session.getAuthPassword());
		if (authToken == null || !authToken.equals(certificateSessionPair.session.getAuthToken()))
			throw new PrivilegeException("Received illegal certificate data for session id "
					+ certificate.getSessionId());

		// get user object
		User user = userMap.get(certificateSessionPair.session.getUsername());
		if (user == null) {
			throw new PrivilegeException(
					"Oh boy, how did this happen: No User in user map although the certificate is valid!");
		}

		// default is to not allow the action
		// TODO should default deny/allow policy be configurable?
		boolean actionAllowed = false;

		// now iterate roles and validate on policy handler
		PolicyHandler policyHandler = PrivilegeContainer.getInstance().getPolicyHandler();
		for (String roleName : user.getRoleList()) {

			Role role = roleMap.get(roleName);
			if (role == null) {
				logger.error("No role is defined with name " + roleName + " which is configured for user " + user);
				continue;
			}

			actionAllowed = policyHandler.actionAllowed(role, restrictable);

			// if action is allowed, then break iteration as a privilege match has been made
			if (actionAllowed)
				break;
		}

		return actionAllowed;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.SessionHandler#authenticate(java.lang.String, java.lang.String)
	 * 
	 * @throws AccessDeniedException
	 *             if the user credentials are not valid
	 */
	@Override
	public Certificate authenticate(String username, String password) {

		// both username and password must at least have 3 characters in length
		if (username == null || username.length() < 3)
			throw new PrivilegeException("The given username is shorter than 3 characters");
		else if (password == null || password.length() < 3)
			throw new PrivilegeException("The given password is shorter than 3 characters");

		EncryptionHandler encryptionHandler = PrivilegeContainer.getInstance().getEncryptionHandler();

		// we only work with hashed passwords
		String passwordHash = encryptionHandler.convertToHash(password);

		// get user object
		User user = userMap.get(username);
		// no user means no authentication
		if (user == null)
			throw new AccessDeniedException("There is no user defined with the credentials: " + username + " / ***...");

		// validate password
		if (!user.getPassword().equals(passwordHash))
			throw new AccessDeniedException("Password is incorrect for " + username + " / ***...");

		// validate if user is allowed to login
		if (user.getState() != UserState.ENABLED)
			throw new AccessDeniedException("User " + username + " is not ENABLED. State is: " + user.getState());

		// validate user has at least one role
		if (user.getRoleList().isEmpty()) {
			throw new PrivilegeException("User " + username + " does not have any roles defined!");
		}

		// get 2 auth tokens
		String authToken = encryptionHandler.nextToken();
		String authPassword = encryptionHandler.nextToken();

		// get next session id
		String sessionId = nextSessionId();

		// create certificate
		Certificate certificate = new Certificate(sessionId, username, authToken, authPassword, user.getLocale());

		// create and save a new session
		Session session = new Session(sessionId, authToken, authPassword, user.getUsername(), System
				.currentTimeMillis());
		sessionMap.put(sessionId, new CertificateSessionPair(session, certificate));

		// log
		logger.info("Authenticated: " + session);

		// return the certificate
		return certificate;
	}

	private synchronized String nextSessionId() {
		return Long.toString(++lastSessionId % Long.MAX_VALUE);
	}

	/**
	 * @see ch.eitchnet.privilege.base.PrivilegeContainerObject#initialize(org.dom4j.Element)
	 */
	public void initialize(Element element) {

		lastSessionId = 0l;
		roleMap = new HashMap<String, Role>();
		userMap = new HashMap<String, User>();
		sessionMap = new HashMap<String, CertificateSessionPair>();

		// get parameters
		Element parameterElement = element.element(XmlConstants.XML_PARAMETERS);
		Map<String, String> parameterMap = ConfigurationHelper.convertToParameterMap(parameterElement);

		// get roles file name
		String rolesFileName = parameterMap.get(XmlConstants.XML_PARAM_ROLES_FILE);
		if (rolesFileName == null || rolesFileName.isEmpty()) {
			throw new PrivilegeException("[" + SessionHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_ROLES_FILE + " is invalid");
		}

		// get roles file
		File rolesFile = new File(PrivilegeContainer.getInstance().getBasePath() + "/" + rolesFileName);
		if (!rolesFile.exists()) {
			throw new PrivilegeException("[" + SessionHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_ROLES_FILE + " is invalid as roles file does not exist at path "
					+ rolesFile.getAbsolutePath());
		}

		// parse roles xml file to XML document
		Element rolesRootElement = XmlHelper.parseDocument(rolesFile).getRootElement();

		// read roles
		readRoles(rolesRootElement);

		// get users file name
		String usersFileName = parameterMap.get(XmlConstants.XML_PARAM_USERS_FILE);
		if (usersFileName == null || usersFileName.isEmpty()) {
			throw new PrivilegeException("[" + SessionHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_USERS_FILE + " is invalid");
		}

		// get users file
		File usersFile = new File(PrivilegeContainer.getInstance().getBasePath() + "/" + usersFileName);
		if (!usersFile.exists()) {
			throw new PrivilegeException("[" + SessionHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_USERS_FILE + " is invalid as users file does not exist at path "
					+ usersFile.getAbsolutePath());
		}

		// parse users xml file to XML document
		Element usersRootElement = XmlHelper.parseDocument(usersFile).getRootElement();

		// read users
		readUsers(usersRootElement);

		logger.info("Read " + userMap.size() + " Users");
		logger.info("Read " + roleMap.size() + " Roles");
	}

	/**
	 * @param usersRootElement
	 */
	private void readUsers(Element usersRootElement) {

		List<Element> userElements = usersRootElement.elements(XmlConstants.XML_USER);
		for (Element userElement : userElements) {

			String username = userElement.attributeValue(XmlConstants.XML_ATTR_USERNAME);
			String password = userElement.attributeValue(XmlConstants.XML_ATTR_PASSWORD);

			String firstname = userElement.element(XmlConstants.XML_FIRSTNAME).getTextTrim();
			String surname = userElement.element(XmlConstants.XML_SURNAME).getTextTrim();

			UserState userState = UserState.valueOf(userElement.element(XmlConstants.XML_STATE).getTextTrim());

			// TODO better handling needed
			String localeName = userElement.element(XmlConstants.XML_LOCALE).getTextTrim();
			Locale locale = new Locale(localeName);

			Element rolesElement = userElement.element(XmlConstants.XML_ROLES);
			List<Element> rolesElementList = rolesElement.elements(XmlConstants.XML_ROLE);
			List<String> roleList = new LinkedList<String>();
			for (Element roleElement : rolesElementList) {
				String roleName = roleElement.getTextTrim();
				if (roleName.isEmpty()) {
					logger.warn("User " + username + " has a role defined with no name, Skipped.");
				} else {
					roleList.add(roleName);
				}
			}

			// create user
			User user = User.buildUser(username, password, firstname, surname, userState, roleList, locale);

			// put user in map
			userMap.put(username, user);
		}
	}

	/**
	 * @param rolesRootElement
	 */
	private void readRoles(Element rolesRootElement) {

		List<Element> roleElements = rolesRootElement.elements(XmlConstants.XML_ROLE);
		for (Element roleElement : roleElements) {

			String roleName = roleElement.attributeValue(XmlConstants.XML_ATTR_NAME);

			List<Element> privilegeElements = roleElement.elements(XmlConstants.XML_PRIVILEGE);
			Map<String, Privilege> privilegeMap = new HashMap<String, Privilege>();
			for (Element privilegeElement : privilegeElements) {

				String privilegeName = privilegeElement.attributeValue(XmlConstants.XML_ATTR_NAME);
				String privilegePolicy = privilegeElement.attributeValue(XmlConstants.XML_ATTR_POLICY);

				String allAllowedS = privilegeElement.element(XmlConstants.XML_ALL_ALLOWED).getTextTrim();
				boolean allAllowed = Boolean.valueOf(allAllowedS);

				List<Element> denyElements = privilegeElement.elements(XmlConstants.XML_DENY);
				List<String> denyList = new ArrayList<String>(denyElements.size());
				for (Element denyElement : denyElements) {
					String denyValue = denyElement.getTextTrim();
					if (denyValue.isEmpty()) {
						logger.error("Role " + roleName + " has privilege " + privilegeName
								+ " with an empty deny value!");
					} else {
						denyList.add(denyValue);
					}
				}

				List<Element> allowElements = privilegeElement.elements(XmlConstants.XML_ALLOW);
				List<String> allowList = new ArrayList<String>(allowElements.size());
				for (Element allowElement : allowElements) {
					String allowValue = allowElement.getTextTrim();
					if (allowValue.isEmpty()) {
						logger.error("Role " + roleName + " has privilege " + privilegeName
								+ " with an empty allow value!");
					} else {
						allowList.add(allowValue);
					}
				}

				Privilege privilege = new Privilege(privilegeName, privilegePolicy, allAllowed, denyList, allowList);
				privilegeMap.put(privilegeName, privilege);
			}

			Role role = new Role(roleName, privilegeMap);
			roleMap.put(roleName, role);
		}
	}

	private class CertificateSessionPair {
		private Session session;
		private Certificate certificate;

		public CertificateSessionPair(Session session, Certificate certificate) {
			this.session = session;
			this.certificate = certificate;
		}
	}
}
