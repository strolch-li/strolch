/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.helper;

import java.io.File;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.dom4j.Document;
import org.dom4j.DocumentFactory;
import org.dom4j.Element;


/**
 * <p>
 * This class is a simple application which can be used to bootstrap a new configuration for the
 * {@link PrivilegeContainer}
 * </p>
 * 
 * <p>
 * Simple execute the application and it will ask a few questions and then write a new set of configuration files which
 * can be used to run the {@link PrivilegeContainer}
 * </p>
 * 
 * @author rvonburg
 * 
 */
public class BootstrapConfigurationHelper {
//	private static final Logger logger = Logger.getLogger(BootstrapConfigurationHelper.class);

	private static String path;

	private static String defaultPrivilegeContainerXmlFile = "PrivilegeContainer.xml";

	private static String usersFileName = "PrivilegeUsers.xml";
	private static String rolesFileName = "PrivilegeRoles.xml";
	private static String privilegesFileName = "Privileges.xml";

	private static String hashAlgorithm = "SHA-256";

	private static String policyXmlFile = "PrivilegePolicies.xml";

	private static String defaultPrivilegeHandler = "ch.eitchnet.privilege.handler.DefaultPrivilegeHandler";
	private static String defaultPersistenceHandler = "ch.eitchnet.privilege.handler.DefaultPersistenceHandler";
	private static String defaultEncryptionHandler = "ch.eitchnet.privilege.handler.DefaultEncryptionHandler";

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		BasicConfigurator.resetConfiguration();
		BasicConfigurator.configure(new ConsoleAppender(new PatternLayout("%d %5p [%t] %C{1} %M - %m%n")));
		Logger.getRootLogger().setLevel(Level.INFO);

		// get current directory
		path = System.getProperty("user.dir") + "/newConfig";

		// TODO ask user where to save configuration, default is pwd/newConfig/....

		// see if path already exists
		File pathF = new File(path);
		if (pathF.exists()) {
			throw new RuntimeException("Path already exists: " + pathF.getAbsolutePath());
		} else {
			if (!pathF.mkdirs()) {
				throw new RuntimeException("Could not create path " + pathF.getAbsolutePath());
			}
		}

		// TODO ask other questions...

		// now perform work:
		createXmlPrivilegeContainer();
		createPolicyConfiguration();
		createModel();
	}

	/**
	 * 
	 */
	private static void createModel() {
		// TODO Auto-generated method stub

	}

	/**
	 * 
	 */
	private static void createPolicyConfiguration() {
		// TODO Auto-generated method stub

	}

	/**
	 * 
	 */
	private static void createXmlPrivilegeContainer() {

		// create document root
		DocumentFactory factory = DocumentFactory.getInstance();
		Document doc = factory.createDocument(XmlHelper.DEFAULT_ENCODING);
		doc.setName(XmlConstants.XML_ROOT_PRIVILEGE_CONTAINER);
		Element rootElement = factory.createElement(XmlConstants.XML_ROOT_PRIVILEGE_CONTAINER);
		doc.setRootElement(rootElement);

		Element parameterElement;
		Element parametersElement;

		// create PersistenceHandler
		Element persistenceHandlerElem = factory.createElement(XmlConstants.XML_HANDLER_PERSISTENCE);
		rootElement.add(persistenceHandlerElem);
		persistenceHandlerElem.addAttribute(XmlConstants.XML_ATTR_CLASS, defaultPersistenceHandler);
		parametersElement = factory.createElement(XmlConstants.XML_PARAMETERS);
		persistenceHandlerElem.add(parametersElement);
		// Parameter usersXmlFile
		parameterElement = factory.createElement(XmlConstants.XML_PARAMETER);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_NAME, XmlConstants.XML_PARAM_USERS_FILE);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_VALUE, usersFileName);
		parametersElement.add(parameterElement);
		// Parameter rolesXmlFile
		parameterElement = factory.createElement(XmlConstants.XML_PARAMETER);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_NAME, XmlConstants.XML_PARAM_ROLES_FILE);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_VALUE, rolesFileName);
		parametersElement.add(parameterElement);
		// Parameter privilegesXmlFile
		parameterElement = factory.createElement(XmlConstants.XML_PARAMETER);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_NAME, XmlConstants.XML_PARAM_PRIVILEGES_FILE);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_VALUE, privilegesFileName);
		parametersElement.add(parameterElement);
		// Parameter policyXmlFile
		parameterElement = factory.createElement(XmlConstants.XML_PARAMETER);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_NAME, XmlConstants.XML_PARAM_POLICY_FILE);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_VALUE, policyXmlFile);
		parametersElement.add(parameterElement);

		// create PrivilegeHandler
		Element privilegeHandlerElem = factory.createElement(XmlConstants.XML_HANDLER_PRIVILEGE);
		rootElement.add(privilegeHandlerElem);
		privilegeHandlerElem.addAttribute(XmlConstants.XML_ATTR_CLASS, defaultPrivilegeHandler);

		// create EncryptionHandler
		Element encryptionHandlerElem = factory.createElement(XmlConstants.XML_HANDLER_ENCRYPTION);
		rootElement.add(encryptionHandlerElem);
		encryptionHandlerElem.addAttribute(XmlConstants.XML_ATTR_CLASS, defaultEncryptionHandler);
		parametersElement = factory.createElement(XmlConstants.XML_PARAMETERS);
		encryptionHandlerElem.add(parametersElement);
		// Parameter hashAlgorithm
		parameterElement = factory.createElement(XmlConstants.XML_PARAMETER);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_NAME, XmlConstants.XML_PARAM_HASH_ALGORITHM);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_VALUE, hashAlgorithm);
		parametersElement.add(parameterElement);

		// write the container file to disk
		File privilegeContainerFile = new File(path + "/" + defaultPrivilegeContainerXmlFile);
		XmlHelper.writeDocument(doc, privilegeContainerFile);
	}
}
