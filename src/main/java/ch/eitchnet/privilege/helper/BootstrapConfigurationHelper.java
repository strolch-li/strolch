/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
@SuppressWarnings("nls")
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
