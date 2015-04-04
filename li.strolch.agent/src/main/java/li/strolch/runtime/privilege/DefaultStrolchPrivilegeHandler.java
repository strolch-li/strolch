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

import java.io.File;
import java.io.FileInputStream;
import java.text.MessageFormat;
import java.util.Map;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.exception.StrolchException;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import ch.eitchnet.privilege.base.AccessDeniedException;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.DefaultPrivilegeHandler;
import ch.eitchnet.privilege.handler.EncryptionHandler;
import ch.eitchnet.privilege.handler.PersistenceHandler;
import ch.eitchnet.privilege.handler.SystemUserAction;
import ch.eitchnet.privilege.handler.XmlPersistenceHandler;
import ch.eitchnet.privilege.helper.PrivilegeInitializationHelper;
import ch.eitchnet.privilege.helper.XmlConstants;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;
import ch.eitchnet.privilege.model.internal.PrivilegeContainerModel;
import ch.eitchnet.privilege.xml.PrivilegeConfigSaxReader;
import ch.eitchnet.utils.helper.XmlHelper;

public class DefaultStrolchPrivilegeHandler extends StrolchComponent implements PrivilegeHandler {

	public static final String PROP_PRIVILEGE_CONFIG_FILE = "privilegeConfigFile"; //$NON-NLS-1$
	public static final String PRIVILEGE_CONFIG_XML = "PrivilegeConfig.xml"; //$NON-NLS-1$

	private ch.eitchnet.privilege.handler.PrivilegeHandler privilegeHandler;

	public DefaultStrolchPrivilegeHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {
		super.initialize(configuration);

		// initialize privilege
		RuntimeConfiguration runtimeConfiguration = configuration.getRuntimeConfiguration();
		File privilegeConfigFile = configuration.getConfigFile(PROP_PRIVILEGE_CONFIG_FILE, PRIVILEGE_CONFIG_XML,
				runtimeConfiguration);
		ch.eitchnet.privilege.handler.PrivilegeHandler privilegeHandler = initializeFromXml(configuration,
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
	private ch.eitchnet.privilege.handler.PrivilegeHandler initializeFromXml(ComponentConfiguration configuration,
			File privilegeXmlFile) {

		// make sure file exists
		if (!privilegeXmlFile.exists()) {
			String msg = "Privilege file does not exist at path {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, privilegeXmlFile.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		try {
			// parse configuration file
			PrivilegeContainerModel containerModel = new PrivilegeContainerModel();
			PrivilegeConfigSaxReader xmlHandler = new PrivilegeConfigSaxReader(containerModel);
			try (FileInputStream inputStream = new FileInputStream(privilegeXmlFile)) {
				XmlHelper.parseDocument(inputStream, xmlHandler);

				// set base path
				if (containerModel.getPersistenceHandlerClassName().equals(XmlPersistenceHandler.class.getName())) {
					Map<String, String> xmlParams = containerModel.getPersistenceHandlerParameterMap();
					File configPath = configuration.getRuntimeConfiguration().getConfigPath();
					xmlParams.put(XmlConstants.XML_PARAM_BASE_PATH, configPath.getPath());
				}

				return PrivilegeInitializationHelper.initializeFromXml(containerModel);
			}
		} catch (Exception e) {
			String msg = "Failed to load Privilege configuration from {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, privilegeXmlFile.getAbsolutePath());
			throw new PrivilegeException(msg, e);
		}
	}

	@Override
	public Certificate authenticate(String username, byte[] password) {
		assertContainerStarted();
		try {
			Certificate certificate = this.privilegeHandler.authenticate(username, password);
			StrolchRealm realm = getContainer().getRealm(certificate);
			try (StrolchTransaction tx = realm.openTx(certificate, getClass())) {
				tx.setSuppressDoNothingLogging(true);
				tx.setSuppressAudits(true);
				Audit audit = tx.auditFrom(AccessType.CREATE, PRIVILEGE, CERTIFICATE, username);
				tx.getAuditTrail().add(tx, audit);
			}
			return certificate;
		} catch (AccessDeniedException e) {
			throw new StrolchException("Authentication credentials are invalid", e); //$NON-NLS-1$
		}
	}

	@Override
	public void isCertificateValid(Certificate certificate) throws PrivilegeException {
		assertContainerStarted();
		this.privilegeHandler.isCertificateValid(certificate);
	}

	@Override
	public void checkPassword(Certificate certificate, byte[] password) throws PrivilegeException {
		assertContainerStarted();
		this.privilegeHandler.checkPassword(certificate, password);
	}

	@Override
	public boolean invalidateSession(Certificate certificate) {
		assertContainerStarted();
		boolean invalidateSession = this.privilegeHandler.invalidateSession(certificate);
		StrolchRealm realm = getContainer().getRealm(certificate);
		try (StrolchTransaction tx = realm.openTx(certificate, getClass())) {
			tx.setSuppressDoNothingLogging(true);
			tx.setSuppressAudits(true);
			Audit audit = tx.auditFrom(AccessType.DELETE, PRIVILEGE, CERTIFICATE, certificate.getUsername());
			tx.getAuditTrail().add(tx, audit);
		}
		return invalidateSession;
	}

	@Override
	public PrivilegeContext getPrivilegeContext(Certificate certificate) throws PrivilegeException {
		return this.privilegeHandler.getPrivilegeContext(certificate);
	}

	@Override
	public void runAsSystem(String systemUsername, SystemUserAction action) throws PrivilegeException {
		this.privilegeHandler.runAsSystem(systemUsername, action);
	}

	@Override
	public ch.eitchnet.privilege.handler.PrivilegeHandler getPrivilegeHandler(Certificate certificate)
			throws PrivilegeException {
		assertContainerStarted();
		return this.privilegeHandler;
	}
}
