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

import org.dom4j.Element;

import ch.eitchnet.privilege.handler.EncryptionHandler;
import ch.eitchnet.privilege.handler.PolicyHandler;
import ch.eitchnet.privilege.handler.SessionHandler;
import ch.eitchnet.privilege.helper.ClassHelper;
import ch.eitchnet.privilege.helper.XmlHelper;

/**
 * 
 * 
 * @author rvonburg
 */
public class PrivilegeContainer {

	private static final PrivilegeContainer instance;

	static {
		instance = new PrivilegeContainer();
	}

	private SessionHandler sessionHandler;
	private PolicyHandler policyHandler;
	private EncryptionHandler encryptionHandler;

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

	public void initialize(File privilegeContainerXml) {

		// parse container xml file to XML document
		Element containerRootElement = XmlHelper.parseDocument(privilegeContainerXml).getRootElement();

		// instantiate session handler
		Element sessionHandlerElement = containerRootElement.element(XmlConstants.XML_HANDLER_SESSION);
		String sessionHandlerClassName = sessionHandlerElement.attributeValue(XmlConstants.XML_ATTR_CLASS);
		SessionHandler sessionHandler = ClassHelper.instantiateClass(sessionHandlerClassName);
		sessionHandler.initialize(sessionHandlerElement);

		// instantiate encryption handler
		Element encryptionHandlerElement = containerRootElement.element(XmlConstants.XML_HANDLER_ENCRYPTION);
		String encryptionHandlerClassName = encryptionHandlerElement.attributeValue(XmlConstants.XML_ATTR_CLASS);
		EncryptionHandler encryptionHandler = ClassHelper.instantiateClass(encryptionHandlerClassName);
		encryptionHandler.initialize(encryptionHandlerElement);

		// instantiate policy handler
		Element policyHandlerElement = containerRootElement.element(XmlConstants.XML_HANDLER_POLICY);
		String policyHandlerClassName = policyHandlerElement.attributeValue(XmlConstants.XML_ATTR_CLASS);
		PolicyHandler policyHandler = ClassHelper.instantiateClass(policyHandlerClassName);
		policyHandler.initialize(policyHandlerElement);

		this.sessionHandler = sessionHandler;
		this.encryptionHandler = encryptionHandler;
		this.policyHandler = policyHandler;
	}
}
