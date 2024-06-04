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
package li.strolch.privilege.helper;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.*;
import li.strolch.privilege.model.internal.PrivilegeContainerModel;
import li.strolch.privilege.policy.PrivilegePolicy;
import li.strolch.privilege.xml.PrivilegeConfigSaxReader;
import li.strolch.utils.dbc.DBC;
import li.strolch.utils.helper.XmlHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.text.MessageFormat;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;

import static li.strolch.privilege.helper.XmlConstants.PARAM_BASE_PATH;
import static li.strolch.utils.helper.ClassHelper.instantiateClass;
import static li.strolch.utils.helper.StringHelper.isEmpty;

/**
 * This class implements the initializing of the {@link PrivilegeHandler} by loading an XML file containing the
 * configuration
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class PrivilegeInitializer {

	private static final Logger logger = LoggerFactory.getLogger(PrivilegeInitializer.class);

	private final ScheduledExecutorService executorService;

	private PrivilegeContainerModel containerModel;
	private EncryptionHandler encryptionHandler;
	private PasswordStrengthHandler passwordStrengthHandler;
	private PersistenceHandler persistenceHandler;
	private UserChallengeHandler challengeHandler;
	private SingleSignOnHandler ssoHandler;
	private PrivilegeHandler privilegeHandler;

	public PrivilegeInitializer(ScheduledExecutorService executorService) {
		DBC.PRE.assertNotNull("executorService may not be null", executorService);
		this.executorService = executorService;
	}

	/**
	 * Initializes the {@link PrivilegeHandler} from the configuration file
	 *
	 * @param privilegeXmlFile a {@link File} reference to the XML file containing the configuration for Privilege
	 *
	 * @return the initialized {@link PrivilegeHandler} where the {@link EncryptionHandler} and
	 * {@link PersistenceHandler} are set and initialized as well
	 */
	public PrivilegeHandler initializeFromXml(File privilegeXmlFile) {

		// make sure file exists
		if (!privilegeXmlFile.exists()) {
			String msg = "Privilege file does not exist at path {0}";
			msg = MessageFormat.format(msg, privilegeXmlFile.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		// delegate using input stream
		try (InputStream fin = Files.newInputStream(privilegeXmlFile.toPath())) {
			this.containerModel = parseXmlConfiguration(privilegeXmlFile.getParentFile(), fin);
		} catch (Exception e) {
			String msg = "Failed to load configuration from {0}";
			msg = MessageFormat.format(msg, privilegeXmlFile.getAbsolutePath());
			throw new PrivilegeException(msg, e);
		}

		initializeComponents();
		this.privilegeHandler = initializedPrivilegeHandler();
		return this.privilegeHandler;
	}

	public PrivilegeHandler initializeFromXml(PrivilegeContainerModel containerModel) {
		this.containerModel = containerModel;
		initializeComponents();
		this.privilegeHandler = initializedPrivilegeHandler();
		return this.privilegeHandler;
	}

	/**
	 * Parses the privilege XML configuration from the given stream
	 *
	 * @param privilegeConfigInputStream the XML stream containing the privilege configuration
	 */
	public PrivilegeContainerModel parseXmlConfiguration(File basePath, InputStream privilegeConfigInputStream) {

		// parse configuration file
		PrivilegeContainerModel containerModel = new PrivilegeContainerModel(basePath);
		PrivilegeConfigSaxReader xmlHandler = new PrivilegeConfigSaxReader(containerModel);
		XmlHelper.parseDocument(privilegeConfigInputStream, xmlHandler);

		return containerModel;
	}

	/**
	 * Initializes the privilege handlers from the given container model
	 */
	private void initializeComponents() {

		this.encryptionHandler = initializedEncryptionHandler();
		this.passwordStrengthHandler = initializePasswordStrengthHandler();
		this.persistenceHandler = initializePersistenceHandler();
		this.challengeHandler = initializeUserChallengeHandler();
		this.ssoHandler = initializeSingleSignOnHandler();
	}

	private PrivilegeHandler initializedPrivilegeHandler() {
		DefaultPrivilegeHandler privilegeHandler;

		Map<String, String> parameterMap = this.containerModel.getParameterMap();
		parameterMap.put(PARAM_BASE_PATH, this.containerModel.getBasePath().getAbsolutePath());

		if (this.containerModel.getPrivilegeHandlerClassName() == null) {
			privilegeHandler = new DefaultPrivilegeHandler();
		} else {
			String privilegeHandlerClassName = this.containerModel.getPrivilegeHandlerClassName();
			privilegeHandler = instantiateClass(privilegeHandlerClassName);
			parameterMap.putAll(this.containerModel.getPrivilegeHandlerParameterMap());
		}

		Map<String, Class<PrivilegePolicy>> policyMap = this.containerModel.getPolicies();
		try {
			privilegeHandler.initialize(this.executorService, parameterMap, this.encryptionHandler,
					this.passwordStrengthHandler, this.persistenceHandler, this.challengeHandler, this.ssoHandler,
					policyMap);
		} catch (Exception e) {
			String msg = "PrivilegeHandler {0} could not be initialized";
			msg = MessageFormat.format(msg, privilegeHandler.getClass().getName());
			throw new PrivilegeException(msg, e);
		}

		return privilegeHandler;
	}

	private SingleSignOnHandler initializeSingleSignOnHandler() {
		if (this.containerModel.getSsoHandlerClassName() == null)
			return null;

		String ssoHandlerClassName = this.containerModel.getSsoHandlerClassName();
		SingleSignOnHandler ssoHandler = instantiateClass(ssoHandlerClassName);
		Map<String, String> parameterMap = this.containerModel.getSsoHandlerParameterMap();

		try {
			ssoHandler.initialize(parameterMap);
		} catch (Exception e) {
			String msg = "SingleSignOnHandler {0} could not be initialized";
			msg = MessageFormat.format(msg, ssoHandlerClassName);
			throw new PrivilegeException(msg, e);
		}

		return ssoHandler;
	}

	private UserChallengeHandler initializeUserChallengeHandler() {
		String challengeHandlerClassName = this.containerModel.getUserChallengeHandlerClassName();
		UserChallengeHandler challengeHandler = instantiateClass(challengeHandlerClassName);
		Map<String, String> parameterMap = this.containerModel.getUserChallengeHandlerParameterMap();

		try {
			challengeHandler.initialize(parameterMap);
		} catch (Exception e) {
			String msg = "UserChallengeHandler {0} could not be initialized";
			msg = MessageFormat.format(msg, challengeHandlerClassName);
			throw new PrivilegeException(msg, e);
		}

		return challengeHandler;
	}

	private PersistenceHandler initializePersistenceHandler() {
		String persistenceHandlerClassName = this.containerModel.getPersistenceHandlerClassName();
		PersistenceHandler persistenceHandler = instantiateClass(persistenceHandlerClassName);
		Map<String, String> parameterMap = this.containerModel.getPersistenceHandlerParameterMap();
		parameterMap.put(PARAM_BASE_PATH, this.containerModel.getBasePath().getAbsolutePath());

		try {
			persistenceHandler.initialize(parameterMap);
		} catch (Exception e) {
			String msg = "PersistenceHandler {0} could not be initialized";
			msg = MessageFormat.format(msg, persistenceHandlerClassName);
			throw new PrivilegeException(msg, e);
		}

		return persistenceHandler;
	}

	private PasswordStrengthHandler initializePasswordStrengthHandler() {
		String passwordStrengthHandlerClassName = this.containerModel.getPasswordStrengthHandlerClassName();
		if (isEmpty(passwordStrengthHandlerClassName)) {
			logger.info("No PasswordStrengthHandler defined, using {}", SimplePasswordStrengthHandler.class.getName());
			passwordStrengthHandlerClassName = SimplePasswordStrengthHandler.class.getName();
		}

		PasswordStrengthHandler passwordStrengthHandler = instantiateClass(passwordStrengthHandlerClassName);
		Map<String, String> parameterMap = this.containerModel.getPasswordStrengthHandlerParameterMap();

		try {
			passwordStrengthHandler.initialize(parameterMap);
		} catch (Exception e) {
			String msg = "PasswordStrengthHandler {0} could not be initialized";
			msg = MessageFormat.format(msg, passwordStrengthHandlerClassName);
			throw new PrivilegeException(msg, e);
		}

		return passwordStrengthHandler;
	}

	private EncryptionHandler initializedEncryptionHandler() {
		String encryptionHandlerClassName = this.containerModel.getEncryptionHandlerClassName();
		EncryptionHandler encryptionHandler = instantiateClass(encryptionHandlerClassName);
		Map<String, String> parameterMap = this.containerModel.getEncryptionHandlerParameterMap();

		try {
			encryptionHandler.initialize(parameterMap);
		} catch (Exception e) {
			String msg = "EncryptionHandler {0} could not be initialized";
			msg = MessageFormat.format(msg, encryptionHandlerClassName);
			throw new PrivilegeException(msg, e);
		}

		return encryptionHandler;
	}
}
