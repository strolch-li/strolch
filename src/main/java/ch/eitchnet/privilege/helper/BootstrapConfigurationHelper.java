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

import ch.eitchnet.privilege.handler.PrivilegeHandler;

/**
 * <p>
 * This class is a simple application which can be used to bootstrap a new configuration for the
 * {@link PrivilegeHandler}
 * </p>
 * 
 * <p>
 * Simple execute the application and it will ask a few questions and then write a new set of configuration files which
 * can be used to run the {@link PrivilegeHandler}
 * </p>
 * 
 * <p>
 * <b>Note:</b>This class is not yet implemented!
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class BootstrapConfigurationHelper {

	// private static final Logger logger = Logger.getLogger(BootstrapConfigurationHelper.class);

	private static String path;

	private static String defaultPrivilegeContainerXmlFile = "Privilege.xml";

	private static String basePath = "";
	private static String modelFileName = "PrivilegeUsers.xml";

	private static String hashAlgorithm = "SHA-256";

	private static String defaultPersistenceHandler = "ch.eitchnet.privilege.handler.DefaultPersistenceHandler";
	private static String defaultEncryptionHandler = "ch.eitchnet.privilege.handler.DefaultEncryptionHandler";

	/**
	 * @param args
	 *            the args from the command line
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
		}

		if (!pathF.mkdirs()) {
			throw new RuntimeException("Could not create path " + pathF.getAbsolutePath());
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
		doc.setName(XmlConstants.XML_ROOT_PRIVILEGE);
		Element rootElement = factory.createElement(XmlConstants.XML_ROOT_PRIVILEGE);
		doc.setRootElement(rootElement);

		Element containerElement = factory.createElement(XmlConstants.XML_CONTAINER);

		Element parameterElement;
		Element parametersElement;

		// create PersistenceHandler
		Element persistenceHandlerElem = factory.createElement(XmlConstants.XML_HANDLER_PERSISTENCE);
		containerElement.add(persistenceHandlerElem);
		persistenceHandlerElem.addAttribute(XmlConstants.XML_ATTR_CLASS, defaultPersistenceHandler);
		parametersElement = factory.createElement(XmlConstants.XML_PARAMETERS);
		persistenceHandlerElem.add(parametersElement);
		// Parameter basePath
		parameterElement = factory.createElement(XmlConstants.XML_PARAMETER);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_NAME, XmlConstants.XML_PARAM_BASE_PATH);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_VALUE, basePath);
		parametersElement.add(parameterElement);
		// Parameter modelXmlFile
		parameterElement = factory.createElement(XmlConstants.XML_PARAMETER);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_NAME, XmlConstants.XML_PARAM_MODEL_FILE);
		parameterElement.addAttribute(XmlConstants.XML_ATTR_VALUE, modelFileName);
		parametersElement.add(parameterElement);

		// create EncryptionHandler
		Element encryptionHandlerElem = factory.createElement(XmlConstants.XML_HANDLER_ENCRYPTION);
		containerElement.add(encryptionHandlerElem);
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
