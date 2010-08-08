/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.base;

import java.io.File;

import org.apache.log4j.Logger;
import org.dom4j.Element;

import ch.eitchnet.privilege.handler.EncryptionHandler;
import ch.eitchnet.privilege.handler.PersistenceHandler;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.helper.ClassHelper;
import ch.eitchnet.privilege.helper.XmlHelper;
import ch.eitchnet.privilege.i18n.PrivilegeException;

/**
 * @author rvonburg
 */
public class PrivilegeContainer {

	/**
	 * This is the role users must have, if they can modify the {@link PrivilegeContainer} and its objects
	 */
	public static final String PRIVILEGE_ADMIN_ROLE = "PrivilegeAdmin";
	public static final String PRIVILEGE_CONTAINER_FILE = "PrivilegeContainer.xml";

	private static final Logger logger = Logger.getLogger(PrivilegeContainer.class);

	private static final PrivilegeContainer instance;

	static {
		instance = new PrivilegeContainer();
	}

	private EncryptionHandler encryptionHandler;
	private PrivilegeHandler modelHandler;

	private String basePath;

	public static PrivilegeContainer getInstance() {
		return instance;
	}

	/**
	 * private constructor to force singleton
	 */
	private PrivilegeContainer() {
		// private constructor
	}

	/**
	 * @return the encryptionHandler
	 */
	public EncryptionHandler getEncryptionHandler() {
		return encryptionHandler;
	}

	/**
	 * @return the modelHandler
	 */
	public PrivilegeHandler getModelHandler() {
		return modelHandler;
	}

	/**
	 * @return the basePath
	 */
	public String getBasePath() {
		return basePath;
	}

	public void initialize(File privilegeContainerXml) {

		// make sure file exists
		if (!privilegeContainerXml.exists()) {
			throw new PrivilegeException("Privilige file does not exist at path "
					+ privilegeContainerXml.getAbsolutePath());
		}

		// set base path from privilege container xml
		basePath = privilegeContainerXml.getParentFile().getAbsolutePath();

		// parse container xml file to XML document
		Element containerRootElement = XmlHelper.parseDocument(privilegeContainerXml).getRootElement();

		// instantiate persistence handler
		Element persistenceHandlerElement = containerRootElement.element(XmlConstants.XML_HANDLER_PERSISTENCE);
		String persistenceHandlerClassName = persistenceHandlerElement.attributeValue(XmlConstants.XML_ATTR_CLASS);
		PersistenceHandler persistenceHandler = ClassHelper.instantiateClass(persistenceHandlerClassName);

		// instantiate encryption handler
		Element encryptionHandlerElement = containerRootElement.element(XmlConstants.XML_HANDLER_ENCRYPTION);
		String encryptionHandlerClassName = encryptionHandlerElement.attributeValue(XmlConstants.XML_ATTR_CLASS);
		EncryptionHandler encryptionHandler = ClassHelper.instantiateClass(encryptionHandlerClassName);

		// instantiate privilege handler
		Element modelHandlerElement = containerRootElement.element(XmlConstants.XML_HANDLER_MODEL);
		String modelHandlerClassName = modelHandlerElement.attributeValue(XmlConstants.XML_ATTR_CLASS);
		PrivilegeHandler modelHandler = ClassHelper.instantiateClass(modelHandlerClassName);

		try {
			persistenceHandler.initialize(persistenceHandlerElement);
		} catch (Exception e) {
			logger.error(e, e);
			throw new PrivilegeException("PersistenceHandler " + persistenceHandlerElement
					+ " could not be initialized");
		}
		try {
			encryptionHandler.initialize(encryptionHandlerElement);
		} catch (Exception e) {
			logger.error(e, e);
			throw new PrivilegeException("EncryptionHandler " + encryptionHandlerClassName
					+ " could not be initialized");
		}
		try {
			modelHandler.initialize(modelHandlerElement);
		} catch (Exception e) {
			logger.error(e, e);
			throw new PrivilegeException("ModificationHandler " + modelHandlerClassName + " could not be initialized");
		}

		// keep references to the handlers
		this.modelHandler = modelHandler;
		this.encryptionHandler = encryptionHandler;
	}
}
