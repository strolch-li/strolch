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

import ch.eitchnet.privilege.base.PrivilegeContainer;
import ch.eitchnet.privilege.base.XmlConstants;

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

	private static String usersFileName = "PrivilegeUsers.xml";
	private static String rolesFileName = "PrivilegeRoles.xml";
	private static String privilegesFileName = "Privileges.xml";

	private static String hashAlgorithm = "SHA-256";

	private static String policyXmlFile = "RestrictionPolicies.xml";

	private static String defaultPersistenceHandler = "ch.eitchnet.privilege.handler.DefaultPersistenceHandler";
	private static String defaultSessionHandler = "ch.eitchnet.privilege.handler.DefaultSessionHandler";
	private static String defaultEncryptionHandler = "ch.eitchnet.privilege.handler.DefaultEncryptionHandler";
	private static String defaultPolicyHandler = "ch.eitchnet.privilege.handler.DefaultPolicyHandler";

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		BasicConfigurator.resetConfiguration();
		BasicConfigurator.configure(new ConsoleAppender(new PatternLayout("%d %5p [%t] %C{1} %M - %m%n")));
		Logger.getRootLogger().setLevel(Level.INFO);

		// get current directory
		path = System.getProperty("user.dir") + "/newConfig";

		// ask user where to save configuration, default is pwd/newConfig/....

		// see if path already exists
		File pathF = new File(path);
		if (pathF.exists()) {
			throw new RuntimeException("Path already exists: " + pathF.getAbsolutePath());
		} else {
			if (!pathF.mkdirs()) {
				throw new RuntimeException("Could not create path " + pathF.getAbsolutePath());
			}
		}

		// ask other questions...

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

		// create SessionHandler
		Element sessionHandlerElem = factory.createElement(XmlConstants.XML_HANDLER_SESSION);
		sessionHandlerElem.addAttribute(XmlConstants.XML_ATTR_CLASS, defaultSessionHandler);

		// create ModelHandler
		Element modelHandlerElem = factory.createElement(XmlConstants.XML_HANDLER_MODEL);
		rootElement.add(modelHandlerElem);
		modelHandlerElem.addAttribute(XmlConstants.XML_ATTR_CLASS, "ch.eitchnet.privilege.handler.DefaultModelHandler");

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

		// create PolicyHandler
		Element policyHandlerElem = factory.createElement(XmlConstants.XML_HANDLER_POLICY);
		rootElement.add(policyHandlerElem);
		policyHandlerElem.addAttribute(XmlConstants.XML_ATTR_CLASS, defaultPolicyHandler);
		parametersElement = factory.createElement(XmlConstants.XML_PARAMETERS);
		policyHandlerElem.add(parametersElement);
		// Parameter policyXmlFile
		parameterElement = factory.createElement(XmlConstants.XML_PARAMETER);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_NAME, XmlConstants.XML_PARAM_POLICY_FILE);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_VALUE, policyXmlFile);
		parametersElement.add(parameterElement);

		File privilegeContainerFile = new File(path + "/" + PrivilegeContainer.PRIVILEGE_CONTAINER_FILE);
		XmlHelper.writeDocument(doc, privilegeContainerFile);
	}
}
