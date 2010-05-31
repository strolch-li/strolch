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
import ch.eitchnet.privilege.handler.PolicyHandler;
import ch.eitchnet.privilege.handler.SessionHandler;
import ch.eitchnet.privilege.helper.ClassHelper;
import ch.eitchnet.privilege.helper.XmlHelper;
import ch.eitchnet.privilege.i18n.PrivilegeException;

/**
 * @author rvonburg
 */
public class PrivilegeContainer {
	private static final Logger logger = Logger.getLogger(PrivilegeContainer.class);

	private static final PrivilegeContainer instance;

	static {
		instance = new PrivilegeContainer();
	}

	private SessionHandler sessionHandler;
	private PolicyHandler policyHandler;
	private EncryptionHandler encryptionHandler;

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
	 * @return the sessionHandler
	 */
	public SessionHandler getSessionHandler() {
		return sessionHandler;
	}

	/**
	 * @return the policyHandler
	 */
	public PolicyHandler getPolicyHandler() {
		return policyHandler;
	}

	/**
	 * @return the encryptionHandler
	 */
	public EncryptionHandler getEncryptionHandler() {
		return encryptionHandler;
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

		// instantiate session handler
		Element sessionHandlerElement = containerRootElement.element(XmlConstants.XML_HANDLER_SESSION);
		String sessionHandlerClassName = sessionHandlerElement.attributeValue(XmlConstants.XML_ATTR_CLASS);
		SessionHandler sessionHandler = ClassHelper.instantiateClass(sessionHandlerClassName);

		// instantiate encryption handler
		Element encryptionHandlerElement = containerRootElement.element(XmlConstants.XML_HANDLER_ENCRYPTION);
		String encryptionHandlerClassName = encryptionHandlerElement.attributeValue(XmlConstants.XML_ATTR_CLASS);
		EncryptionHandler encryptionHandler = ClassHelper.instantiateClass(encryptionHandlerClassName);

		// instantiate policy handler
		Element policyHandlerElement = containerRootElement.element(XmlConstants.XML_HANDLER_POLICY);
		String policyHandlerClassName = policyHandlerElement.attributeValue(XmlConstants.XML_ATTR_CLASS);
		PolicyHandler policyHandler = ClassHelper.instantiateClass(policyHandlerClassName);

		try {
			sessionHandler.initialize(sessionHandlerElement);
		} catch (Exception e) {
			logger.error(e, e);
			throw new PrivilegeException("SessionHandler " + sessionHandlerClassName + " could not be initialized");
		}
		try {
			encryptionHandler.initialize(encryptionHandlerElement);
		} catch (Exception e) {
			logger.error(e, e);
			throw new PrivilegeException("EncryptionHandler " + encryptionHandlerClassName
					+ " could not be initialized");
		}
		try {
			policyHandler.initialize(policyHandlerElement);
		} catch (Exception e) {
			logger.error(e, e);
			throw new PrivilegeException("PolicyHandler " + policyHandlerClassName + " could not be initialized");
		}

		// keep references to the handlers
		this.sessionHandler = sessionHandler;
		this.encryptionHandler = encryptionHandler;
		this.policyHandler = policyHandler;
	}
}
