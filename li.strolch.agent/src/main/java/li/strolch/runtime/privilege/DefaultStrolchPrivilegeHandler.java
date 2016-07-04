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
package li.strolch.runtime.privilege;

import static li.strolch.privilege.handler.PrivilegeHandler.PARAM_PERSIST_SESSIONS;
import static li.strolch.privilege.handler.PrivilegeHandler.PARAM_PERSIST_SESSIONS_PATH;

import java.io.File;
import java.io.FileInputStream;
import java.text.MessageFormat;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.DefaultPrivilegeHandler;
import li.strolch.privilege.handler.EncryptionHandler;
import li.strolch.privilege.handler.PersistenceHandler;
import li.strolch.privilege.handler.SystemUserAction;
import li.strolch.privilege.handler.XmlPersistenceHandler;
import li.strolch.privilege.helper.PrivilegeInitializationHelper;
import li.strolch.privilege.helper.XmlConstants;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.internal.PrivilegeContainerModel;
import li.strolch.privilege.xml.PrivilegeConfigSaxReader;
import li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.utils.helper.XmlHelper;

public class DefaultStrolchPrivilegeHandler extends StrolchComponent implements PrivilegeHandler {

	public static final String PROP_PRIVILEGE_CONFIG_FILE = "privilegeConfigFile"; //$NON-NLS-1$
	public static final String PRIVILEGE_CONFIG_XML = "PrivilegeConfig.xml"; //$NON-NLS-1$

	private li.strolch.privilege.handler.PrivilegeHandler privilegeHandler;

	public DefaultStrolchPrivilegeHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {
		super.initialize(configuration);

		// initialize privilege
		RuntimeConfiguration runtimeConfiguration = configuration.getRuntimeConfiguration();
		File privilegeConfigFile = configuration.getConfigFile(PROP_PRIVILEGE_CONFIG_FILE, PRIVILEGE_CONFIG_XML,
				runtimeConfiguration);
		li.strolch.privilege.handler.PrivilegeHandler privilegeHandler = initializeFromXml(configuration,
				privilegeConfigFile);
		this.privilegeHandler = privilegeHandler;
	}

	/**
	 * Initializes the {@link DefaultPrivilegeHandler} from the configuration file
	 * 
	 * @param privilegeXmlFile
	 *            a {@link File} reference to the XML file containing the configuration for Privilege
	 * 
	 * @return the initialized {@link PrivilegeHandler} where the {@link EncryptionHandler} and
	 *         {@link PersistenceHandler} are set and initialized as well
	 */
	private li.strolch.privilege.handler.PrivilegeHandler initializeFromXml(ComponentConfiguration configuration,
			File privilegeXmlFile) {

		// make sure file exists
		if (!privilegeXmlFile.exists()) {
			String msg = "Privilege file does not exist at path {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, privilegeXmlFile.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		try (FileInputStream inputStream = new FileInputStream(privilegeXmlFile)) {

			// parse configuration file
			PrivilegeContainerModel containerModel = new PrivilegeContainerModel();
			PrivilegeConfigSaxReader xmlHandler = new PrivilegeConfigSaxReader(containerModel);
			XmlHelper.parseDocument(inputStream, xmlHandler);

			Map<String, String> parameterMap = containerModel.getParameterMap();
			RuntimeConfiguration runtimeConfig = configuration.getRuntimeConfiguration();

			// set sessions data path
			if (Boolean.valueOf(parameterMap.get(PARAM_PERSIST_SESSIONS))) {
				File dataPath = runtimeConfig.getDataPath();
				String sessionsPath = new File(dataPath, "sessions.dat").getAbsolutePath();
				parameterMap.put(PARAM_PERSIST_SESSIONS_PATH, sessionsPath);
			}

			// set base path
			if (containerModel.getPersistenceHandlerClassName().equals(XmlPersistenceHandler.class.getName())) {
				Map<String, String> xmlParams = containerModel.getPersistenceHandlerParameterMap();
				File configPath = runtimeConfig.getConfigPath();
				xmlParams.put(XmlConstants.XML_PARAM_BASE_PATH, configPath.getPath());
			}

			return PrivilegeInitializationHelper.initializeFromXml(containerModel);

		} catch (Exception e) {
			String msg = "Failed to load Privilege configuration from {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, privilegeXmlFile.getAbsolutePath());
			throw new PrivilegeException(msg, e);
		}
	}

	@Override
	public Certificate authenticate(String username, byte[] password) {
		assertContainerStarted();
		Certificate certificate = this.privilegeHandler.authenticate(username, password);
		StrolchRealm realm = getContainer().getRealm(certificate);
		try (StrolchTransaction tx = realm.openTx(certificate, StrolchPrivilegeConstants.LOGIN)) {
			tx.setSuppressDoNothingLogging(true);
			tx.setSuppressAudits(true);
			Audit audit = tx.auditFrom(AccessType.CREATE, StrolchPrivilegeConstants.PRIVILEGE,
					StrolchPrivilegeConstants.CERTIFICATE, username);
			tx.getAuditTrail().add(tx, audit);
		}
		return certificate;
	}

	@Override
	public void isCertificateValid(Certificate certificate) throws PrivilegeException {
		assertStarted();
		this.privilegeHandler.isCertificateValid(certificate);
	}

	@Override
	public void checkPassword(Certificate certificate, byte[] password) throws PrivilegeException {
		assertContainerStarted();
		this.privilegeHandler.checkPassword(certificate, password);
	}

	@Override
	public boolean invalidateSession(Certificate certificate) {
		boolean invalidateSession = this.privilegeHandler.invalidateSession(certificate);
		StrolchRealm realm = getContainer().getRealm(certificate);
		try (StrolchTransaction tx = realm.openTx(certificate, StrolchPrivilegeConstants.LOGOUT)) {
			tx.setSuppressDoNothingLogging(true);
			tx.setSuppressAudits(true);
			Audit audit = tx.auditFrom(AccessType.DELETE, StrolchPrivilegeConstants.PRIVILEGE,
					StrolchPrivilegeConstants.CERTIFICATE, certificate.getUsername());
			tx.getAuditTrail().add(tx, audit);
		}
		return invalidateSession;
	}

	@Override
	public boolean sessionTimeout(Certificate certificate) {
		assertStarted();
		boolean invalidateSession = this.privilegeHandler.invalidateSession(certificate);
		StrolchRealm realm = getContainer().getRealm(certificate);
		try (StrolchTransaction tx = realm.openTx(certificate, StrolchPrivilegeConstants.SESSION_TIME_OUT)) {
			tx.setSuppressDoNothingLogging(true);
			tx.setSuppressAudits(true);
			Audit audit = tx.auditFrom(AccessType.DELETE, StrolchPrivilegeConstants.PRIVILEGE,
					StrolchPrivilegeConstants.CERTIFICATE, certificate.getUsername());
			tx.getAuditTrail().add(tx, audit);
		}
		return invalidateSession;
	}

	@Override
	public PrivilegeContext getPrivilegeContext(Certificate certificate) throws PrivilegeException {
		return this.privilegeHandler.getPrivilegeContext(certificate);
	}

	@Override
	public <T extends SystemUserAction> T runAsSystem(String systemUsername, T action) throws PrivilegeException {
		return this.privilegeHandler.runAsSystem(systemUsername, action);
	}

	@Override
	public li.strolch.privilege.handler.PrivilegeHandler getPrivilegeHandler(Certificate certificate)
			throws PrivilegeException {
		return this.privilegeHandler;
	}
}
