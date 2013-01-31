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
import java.util.HashMap;
import java.util.Map;

import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.model.internal.PrivilegeContainerModel;
import ch.eitchnet.privilege.xml.PrivilegeConfigDomWriter;

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

	// private static final Logger logger = Loggerdoc.getLogger(BootstrapConfigurationHelper.class);

	private static String path;

	private static String defaultPrivilegeContainerXmlFile = "Privilege.xml";

	//private static String basePath = "";
	//private static String modelFileName = "PrivilegeUsers.xml";
	//private static String hashAlgorithm = "SHA-256";

	private static String defaultPersistenceHandler = "ch.eitchnet.privilege.handler.DefaultPersistenceHandler";
	private static String defaultEncryptionHandler = "ch.eitchnet.privilege.handler.DefaultEncryptionHandler";

	/**
	 * @param args
	 *            the args from the command line
	 */
	public static void main(String[] args) {

		// get current directory
		BootstrapConfigurationHelper.path = System.getProperty("user.dir") + "/newConfig";

		// TODO ask user where to save configuration, default is pwd/newConfig/....

		// see if path already exists
		File pathF = new File(BootstrapConfigurationHelper.path);
		if (pathF.exists()) {
			throw new RuntimeException("Path already exists: " + pathF.getAbsolutePath());
		}

		if (!pathF.mkdirs()) {
			throw new RuntimeException("Could not create path " + pathF.getAbsolutePath());
		}

		Map<String, String> parameterMap = new HashMap<String, String>();
		Map<String, String> encryptionHandlerParameterMap = new HashMap<String, String>();
		Map<String, String> persistenceHandlerParameterMap = new HashMap<String, String>();

		// TODO ask other questions...
		parameterMap.put("autoPersistOnPasswordChange", "true");
		encryptionHandlerParameterMap.put("hashAlgorithm", "SHA-256");
		persistenceHandlerParameterMap.put("basePath", "./target/test");
		persistenceHandlerParameterMap.put("modelXmlFile", "PrivilegeModel.xml");

		PrivilegeContainerModel containerModel = new PrivilegeContainerModel();
		containerModel.setParameterMap(parameterMap);
		containerModel.setEncryptionHandlerClassName(defaultEncryptionHandler);
		containerModel.setEncryptionHandlerParameterMap(encryptionHandlerParameterMap);
		containerModel.setPersistenceHandlerClassName(defaultPersistenceHandler);
		containerModel.setPersistenceHandlerParameterMap(persistenceHandlerParameterMap);

		containerModel.addPolicy("DefaultPrivilege", "ch.eitchnet.privilege.policy.DefaultPrivilege");

		// now perform work:
		File configFile = new File(BootstrapConfigurationHelper.path + "/"
				+ BootstrapConfigurationHelper.defaultPrivilegeContainerXmlFile);
		PrivilegeConfigDomWriter configSaxWriter = new PrivilegeConfigDomWriter(containerModel, configFile);
		configSaxWriter.write();
	}
}
