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

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;
import li.strolch.agent.api.StrolchRealm;
import li.strolch.model.audit.AccessType;
import li.strolch.model.audit.Audit;
import li.strolch.persistence.api.StrolchTransaction;
import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.handler.*;
import li.strolch.privilege.helper.PrivilegeInitializer;
import li.strolch.privilege.model.Certificate;
import li.strolch.privilege.model.PrivilegeContext;
import li.strolch.privilege.model.Usage;
import li.strolch.privilege.model.internal.PrivilegeContainerModel;
import li.strolch.privilege.xml.PrivilegeConfigSaxReader;
import li.strolch.runtime.StrolchConstants;
import li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import li.strolch.utils.helper.XmlHelper;

import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.text.MessageFormat;
import java.util.Map;

import static java.lang.Boolean.parseBoolean;
import static java.util.concurrent.TimeUnit.NANOSECONDS;
import static li.strolch.persistence.api.TransactionThreadLocal.getTx;
import static li.strolch.persistence.api.TransactionThreadLocal.hasTx;
import static li.strolch.privilege.handler.PrivilegeHandler.PARAM_PERSIST_SESSIONS;
import static li.strolch.privilege.handler.PrivilegeHandler.PARAM_PERSIST_SESSIONS_PATH;
import static li.strolch.runtime.StrolchConstants.StrolchPrivilegeConstants.*;

public class DefaultStrolchPrivilegeHandler extends StrolchComponent implements PrivilegeHandler {

	public static final String PROP_PRIVILEGE_CONFIG_FILE = "privilegeConfigFile";
	public static final String PRIVILEGE_CONFIG_XML = "PrivilegeConfig.xml";

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
		this.privilegeHandler = initializeFromXml(configuration, privilegeConfigFile);
	}

	@Override
	public void reloadConfiguration() {

		try {
			runAsAgent(ctx -> this.privilegeHandler.persistSessions(ctx.getCertificate(), getClass().getName()));
		} catch (Exception e) {
			logger.error("Failed to persist sessions", e);
		}

		ComponentConfiguration configuration = getConfiguration();
		RuntimeConfiguration runtimeConfiguration = configuration.getRuntimeConfiguration();
		File privilegeConfigFile = configuration.getConfigFile(PROP_PRIVILEGE_CONFIG_FILE, PRIVILEGE_CONFIG_XML,
				runtimeConfiguration);
		this.privilegeHandler = initializeFromXml(configuration, privilegeConfigFile);
	}

	/**
	 * Initializes the {@link DefaultPrivilegeHandler} from the configuration file
	 *
	 * @param privilegeXmlFile a {@link File} reference to the XML file containing the configuration for Privilege
	 *
	 * @return the initialized {@link PrivilegeHandler} where the {@link EncryptionHandler} and
	 * {@link PersistenceHandler} are set and initialized as well
	 */
	private li.strolch.privilege.handler.PrivilegeHandler initializeFromXml(ComponentConfiguration configuration,
			File privilegeXmlFile) {

		// make sure file exists
		if (!privilegeXmlFile.exists()) {
			String msg = "Privilege file does not exist at path {0}";
			msg = MessageFormat.format(msg, privilegeXmlFile.getAbsolutePath());
			throw new PrivilegeException(msg);
		}

		RuntimeConfiguration runtimeConfig = configuration.getRuntimeConfiguration();
		File configPath = runtimeConfig.getConfigPath();

		try (InputStream inputStream = Files.newInputStream(privilegeXmlFile.toPath())) {

			// parse configuration file
			PrivilegeContainerModel containerModel = new PrivilegeContainerModel(configPath);
			PrivilegeConfigSaxReader xmlHandler = new PrivilegeConfigSaxReader(containerModel);
			XmlHelper.parseDocument(inputStream, xmlHandler);

			// set sessions data path
			Map<String, String> parameterMap = containerModel.getParameterMap();
			if (parseBoolean(parameterMap.get(PARAM_PERSIST_SESSIONS))) {
				File dataPath = runtimeConfig.getTempPath();
				String sessionsPath = new File(dataPath, "sessions.dat").getAbsolutePath();
				parameterMap.put(PARAM_PERSIST_SESSIONS_PATH, sessionsPath);
			}

			return new PrivilegeInitializer(getScheduledExecutor(getName())).initializeFromXml(containerModel);

		} catch (Exception e) {
			String msg = "Failed to load Privilege configuration from {0}";
			msg = MessageFormat.format(msg, privilegeXmlFile.getAbsolutePath());
			throw new PrivilegeException(msg, e);
		}
	}

	@Override
	public Certificate authenticate(String username, char[] password) {
		assertContainerStarted();
		Certificate certificate = this.privilegeHandler.authenticate(username, password, false);
		writeAudit(certificate, LOGIN, AccessType.CREATE, username);
		return certificate;
	}

	@Override
	public Certificate authenticate(String username, char[] password, String source, Usage usage, boolean keepAlive) {
		assertContainerStarted();
		Certificate certificate = this.privilegeHandler.authenticate(username, password, source, usage, keepAlive);
		writeAudit(certificate, LOGIN, AccessType.CREATE, username);
		return certificate;
	}

	@Override
	public Certificate authenticateSingleSignOn(Object data) {
		assertContainerStarted();
		Certificate certificate = this.privilegeHandler.authenticateSingleSignOn(data, false);
		writeAudit(certificate, LOGIN, AccessType.CREATE, certificate.getUsername());
		return certificate;
	}

	@Override
	public Certificate authenticateSingleSignOn(Object data, String source) {
		assertContainerStarted();
		Certificate certificate = this.privilegeHandler.authenticateSingleSignOn(data, source, false);
		writeAudit(certificate, LOGIN, AccessType.CREATE, certificate.getUsername());
		return certificate;
	}

	@Override
	public Certificate refreshSession(Certificate certificate, String source) {
		assertContainerStarted();
		Certificate refreshedCert = this.privilegeHandler.refresh(certificate, source);
		writeAudit(refreshedCert, LOGIN, AccessType.CREATE, refreshedCert.getUsername());
		return refreshedCert;
	}

	@Override
	public boolean isRefreshAllowed() {
		return this.privilegeHandler.isRefreshAllowed();
	}

	private void writeAudit(Certificate certificate, String login, AccessType accessType, String username) {
		if (hasTx()) {
			StrolchTransaction tx = getTx();
			tx.setSuppressAudits(true);
			Audit audit = tx.auditFrom(accessType, PRIVILEGE, CERTIFICATE, username);
			tx.getAuditTrail().add(tx, audit);
			return;
		}

		StrolchRealm realm = getContainer().getRealm(certificate);
		try (StrolchTransaction tx = hasTx() ? getTx() : openTx(certificate, login, realm)) {
			tx.setSuppressAudits(true);
			Audit audit = tx.auditFrom(accessType, PRIVILEGE, CERTIFICATE, username);
			tx.getAuditTrail().add(tx, audit);
		}
	}

	private static StrolchTransaction openTx(Certificate certificate, String login, StrolchRealm realm) {
		return realm.openTx(certificate, login, false).silentThreshold(1, NANOSECONDS);
	}

	@Override
	public PrivilegeContext validate(Certificate certificate) throws PrivilegeException {
		return this.privilegeHandler.validate(certificate);
	}

	@Override
	public PrivilegeContext validate(Certificate certificate, String source) throws PrivilegeException {
		return this.privilegeHandler.validate(certificate, source);
	}

	@Override
	public void validateSystemSession(PrivilegeContext ctx) throws PrivilegeException {
		this.privilegeHandler.validateSystemSession(ctx);
	}

	@Override
	public boolean invalidate(Certificate certificate) {
		boolean invalidateSession = this.privilegeHandler.invalidate(certificate);
		writeAudit(certificate, StrolchPrivilegeConstants.LOGOUT, AccessType.DELETE, certificate.getUsername());
		return invalidateSession;
	}

	@Override
	public boolean sessionTimeout(Certificate certificate) {
		assertStarted();
		boolean invalidateSession = this.privilegeHandler.invalidate(certificate);
		writeAudit(certificate, StrolchPrivilegeConstants.SESSION_TIME_OUT, AccessType.DELETE,
				certificate.getUsername());
		return invalidateSession;
	}

	@Override
	public void runAs(String username, SystemAction action) throws Exception {
		this.privilegeHandler.runAs(username, action);
	}

	@Override
	public <T> T runWithResult(String username, SystemActionWithResult<T> action) throws Exception {
		return this.privilegeHandler.runWithResult(username, action);
	}

	@Override
	public void runAs(String username, PrivilegedRunnable runnable) throws Exception {
		this.privilegeHandler.runAs(username, new StrolchSystemAction(runnable));
	}

	@Override
	public <T> T runWithResult(String username, PrivilegedRunnableWithResult<T> runnable) throws Exception {
		return this.privilegeHandler.runWithResult(username, new StrolchSystemActionWithResult<>(runnable));
	}

	@Override
	public void runAsAgent(SystemAction action) throws Exception {
		this.privilegeHandler.runAs(StrolchConstants.SYSTEM_USER_AGENT, action);
	}

	@Override
	public <T> T runAsAgentWithResult(SystemActionWithResult<T> action) throws Exception {
		return this.privilegeHandler.runWithResult(StrolchConstants.SYSTEM_USER_AGENT, action);
	}

	@Override
	public void runAsAgent(PrivilegedRunnable runnable) throws Exception {
		this.privilegeHandler.runAs(StrolchConstants.SYSTEM_USER_AGENT, new StrolchSystemAction(runnable));
	}

	@Override
	public <T> T runAsAgentWithResult(PrivilegedRunnableWithResult<T> runnable) throws Exception {
		return this.privilegeHandler.runWithResult(StrolchConstants.SYSTEM_USER_AGENT,
				new StrolchSystemActionWithResult<>(runnable));
	}

	@Override
	public <T> T runAsWithResult(String username, PrivilegedRunnableWithResult<T> runnable) throws Exception {
		return this.privilegeHandler.runWithResult(username, new StrolchSystemActionWithResult<>(runnable));
	}

	@Override
	public PrivilegeContext openAgentSystemUserContext() throws PrivilegeException {
		return this.privilegeHandler.openSystemUserContext(StrolchConstants.SYSTEM_USER_AGENT);
	}

	@Override
	public li.strolch.privilege.handler.PrivilegeHandler getPrivilegeHandler() {
		return this.privilegeHandler;
	}
}
