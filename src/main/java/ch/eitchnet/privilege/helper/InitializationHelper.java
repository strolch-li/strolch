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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;
import org.dom4j.Element;

import ch.eitchnet.privilege.handler.DefaultPrivilegeHandler;
import ch.eitchnet.privilege.handler.EncryptionHandler;
import ch.eitchnet.privilege.handler.PersistenceHandler;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.i18n.PrivilegeException;
import ch.eitchnet.privilege.policy.PrivilegePolicy;
import ch.eitchnet.utils.helper.StringHelper;

/**
 * This class implements the initializing of the {@link PrivilegeHandler} by loading an XML file containing the
 * configuration
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InitializationHelper {

	private static final Logger logger = Logger.getLogger(InitializationHelper.class);

	/**
	 * Initializes the {@link DefaultPrivilegeHandler} from the configuration file
	 * 
	 * @param privilegeXmlFile
	 *            a {@link File} reference to the XML file containing the configuration for Privilege
	 * 
	 * @return the {@link PrivilegeHandler} instance loaded from the configuration file
	 */
	public static PrivilegeHandler initializeFromXml(File privilegeXmlFile) {

		// make sure file exists
		if (!privilegeXmlFile.exists()) {
			throw new PrivilegeException("Privilege file does not exist at path " + privilegeXmlFile.getAbsolutePath());
		}

		// parse container xml file to XML document
		Element rootElement = XmlHelper.parseDocument(privilegeXmlFile).getRootElement();
		Element containerElement = rootElement.element(XmlConstants.XML_CONTAINER);

		// instantiate encryption handler
		Element encryptionHandlerElement = containerElement.element(XmlConstants.XML_HANDLER_ENCRYPTION);
		String encryptionHandlerClassName = encryptionHandlerElement.attributeValue(XmlConstants.XML_ATTR_CLASS);
		EncryptionHandler encryptionHandler = ClassHelper.instantiateClass(encryptionHandlerClassName);

		// instantiate persistence handler
		Element persistenceHandlerElement = containerElement.element(XmlConstants.XML_HANDLER_PERSISTENCE);
		String persistenceHandlerClassName = persistenceHandlerElement.attributeValue(XmlConstants.XML_ATTR_CLASS);
		PersistenceHandler persistenceHandler = ClassHelper.instantiateClass(persistenceHandlerClassName);

		// instantiate privilege handler
		DefaultPrivilegeHandler privilegeHandler = new DefaultPrivilegeHandler();

		// get policies
		Element policiesElement = rootElement.element(XmlConstants.XML_POLICIES);
		Map<String, Class<PrivilegePolicy>> policyMap = InitializationHelper.convertToPolicyMap(policiesElement);

		try {

			// get parameters
			Element parameterElement = encryptionHandlerElement.element(XmlConstants.XML_PARAMETERS);
			Map<String, String> parameterMap = InitializationHelper.convertToParameterMap(parameterElement);

			// initialize encryption handler
			encryptionHandler.initialize(parameterMap);

		} catch (Exception e) {
			InitializationHelper.logger.error(e, e);
			throw new PrivilegeException("EncryptionHandler " + encryptionHandlerClassName
					+ " could not be initialized");
		}

		try {

			// get parameters
			Element parameterElement = persistenceHandlerElement.element(XmlConstants.XML_PARAMETERS);
			Map<String, String> parameterMap = InitializationHelper.convertToParameterMap(parameterElement);

			// initialize persistence handler
			persistenceHandler.initialize(parameterMap);

		} catch (Exception e) {
			InitializationHelper.logger.error(e, e);
			throw new PrivilegeException("PersistenceHandler " + persistenceHandlerElement
					+ " could not be initialized");
		}

		try {

			// get parameters
			Element parameterElement = containerElement.element(XmlConstants.XML_PARAMETERS);
			Map<String, String> parameterMap = InitializationHelper.convertToParameterMap(parameterElement);

			// initialize privilege handler
			privilegeHandler.initialize(parameterMap, encryptionHandler, persistenceHandler, policyMap);

		} catch (Exception e) {
			InitializationHelper.logger.error(e, e);
			throw new PrivilegeException("PrivilegeHandler " + privilegeHandler.getClass().getName()
					+ " could not be initialized");
		}

		return privilegeHandler;
	}

	/**
	 * Converts an {@link XmlConstants#XML_PARAMETERS} element containing {@link XmlConstants#XML_PARAMETER} elements to
	 * a {@link Map} of String key/value pairs
	 * 
	 * @param element
	 *            the XML {@link Element} with name {@link XmlConstants#XML_PARAMETERS} containing
	 *            {@link XmlConstants#XML_PARAMETER} elements
	 * 
	 * @return the {@link Map} of the parameter name/value combinations from the given {@link Element}
	 */
	@SuppressWarnings("unchecked")
	public static Map<String, String> convertToParameterMap(Element element) {

		// if element is null then there are no parameters, so return empty map
		if (element == null)
			return Collections.emptyMap();

		List<Element> elements = element.elements(XmlConstants.XML_PARAMETER);

		// if elements is null or empty then there are no parameters, so return empty map
		if (elements == null || elements.isEmpty())
			return Collections.emptyMap();

		Map<String, String> parameterMap = new HashMap<String, String>();

		for (Element parameter : elements) {
			String name = parameter.attributeValue(XmlConstants.XML_ATTR_NAME);
			String value = parameter.attributeValue(XmlConstants.XML_ATTR_VALUE);
			
			// replace any defined system properties
			value = StringHelper.replaceSystemPropertiesIn(value);
			
			parameterMap.put(name, value);
		}

		return parameterMap;
	}

	/**
	 * Converts an {@link XmlConstants#XML_POLICIES} element containing {@link XmlConstants#XML_POLICY} elements to a
	 * {@link Map} of String/Class pairs
	 * 
	 * @param element
	 *            the XML {@link Element} with name {@link XmlConstants#XML_POLICIES} containing
	 *            {@link XmlConstants#XML_POLICY} elements
	 * 
	 * @return the {@link Map} of the policy name/class combinations from the given {@link Element}
	 */
	@SuppressWarnings("unchecked")
	public static Map<String, Class<PrivilegePolicy>> convertToPolicyMap(Element element) {

		Map<String, Class<PrivilegePolicy>> policyMap = new HashMap<String, Class<PrivilegePolicy>>();

		List<Element> policyElements = element.elements(XmlConstants.XML_POLICY);
		for (Element policyElement : policyElements) {
			String policyName = policyElement.attributeValue(XmlConstants.XML_ATTR_NAME);
			String policyClass = policyElement.attributeValue(XmlConstants.XML_ATTR_CLASS);

			Class<PrivilegePolicy> clazz;
			try {
				clazz = ClassHelper.loadClass(policyClass);
			} catch (PrivilegeException e) {
				throw new PrivilegeException("The Policy with name " + policyName + " does not exist", e);
			}

			policyMap.put(policyName, clazz);
		}

		return policyMap;
	}
}
