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
package ch.eitchnet.privilege.xml;

import java.io.File;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.DefaultPrivilegeHandler;
import ch.eitchnet.privilege.handler.EncryptionHandler;
import ch.eitchnet.privilege.handler.PersistenceHandler;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.helper.ClassHelper;
import ch.eitchnet.privilege.helper.XmlHelper;
import ch.eitchnet.privilege.model.internal.PrivilegeContainerModel;
import ch.eitchnet.privilege.policy.PrivilegePolicy;

/**
 * This class implements the initializing of the {@link PrivilegeHandler} by loading an XML file containing the
 * configuration
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class InitializationHelper {

	private static final Logger logger = LoggerFactory.getLogger(InitializationHelper.class);

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

		// parse configuration file
		PrivilegeContainerModel containerModel = new PrivilegeContainerModel();
		PrivilegeConfigSaxReader xmlHandler = new PrivilegeConfigSaxReader(containerModel);
		XmlHelper.parseDocument(privilegeXmlFile, xmlHandler);

		// initialize encryption handler
		String encryptionHandlerClassName = containerModel.getEncryptionHandlerClassName();
		EncryptionHandler encryptionHandler = ClassHelper.instantiateClass(encryptionHandlerClassName);
		Map<String, String> parameterMap = containerModel.getEncryptionHandlerParameterMap();
		try {
			encryptionHandler.initialize(parameterMap);
		} catch (Exception e) {
			InitializationHelper.logger.error(e.getMessage(), e);
			throw new PrivilegeException("EncryptionHandler " + encryptionHandlerClassName
					+ " could not be initialized");
		}

		// initialize persistence handler
		String persistenceHandlerClassName = containerModel.getPersistenceHandlerClassName();
		PersistenceHandler persistenceHandler = ClassHelper.instantiateClass(persistenceHandlerClassName);
		parameterMap = containerModel.getPersistenceHandlerParameterMap();
		try {
			persistenceHandler.initialize(parameterMap);
		} catch (Exception e) {
			InitializationHelper.logger.error(e.getMessage(), e);
			throw new PrivilegeException("PersistenceHandler " + persistenceHandlerClassName
					+ " could not be initialized");
		}

		// initialize privilege handler
		DefaultPrivilegeHandler privilegeHandler = new DefaultPrivilegeHandler();
		parameterMap = containerModel.getParameterMap();
		Map<String, Class<PrivilegePolicy>> policyMap = containerModel.getPolicies();
		try {
			privilegeHandler.initialize(parameterMap, encryptionHandler, persistenceHandler, policyMap);
		} catch (Exception e) {
			InitializationHelper.logger.error(e.getMessage(), e);
			throw new PrivilegeException("PrivilegeHandler " + privilegeHandler.getClass().getName()
					+ " could not be initialized");
		}

		return privilegeHandler;
	}
}
