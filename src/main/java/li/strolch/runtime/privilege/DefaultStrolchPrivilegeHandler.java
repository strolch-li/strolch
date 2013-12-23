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

import li.strolch.runtime.agent.ComponentContainerImpl;
import li.strolch.runtime.agent.StrolchComponent;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.runtime.configuration.RuntimeConfiguration;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.handler.SystemUserAction;
import ch.eitchnet.privilege.helper.PrivilegeInitializationHelper;
import ch.eitchnet.privilege.model.Certificate;
import ch.eitchnet.privilege.model.PrivilegeContext;

public class DefaultStrolchPrivilegeHandler extends StrolchComponent implements StrolchPrivilegeHandler {

	public static final String PROP_PRIVILEGE_CONFIG_FILE = "privilegeConfigFile"; //$NON-NLS-1$
	public static final String PRIVILEGE_CONFIG_XML = "PrivilegeConfig.xml"; //$NON-NLS-1$

	private PrivilegeHandler privilegeHandler;

	public DefaultStrolchPrivilegeHandler(ComponentContainerImpl container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {
		super.initialize(configuration);

		// initialize privilege
		RuntimeConfiguration runtimeConfiguration = configuration.getRuntimeConfiguration();
		File privilegeConfigFile = configuration.getConfigFile(PROP_PRIVILEGE_CONFIG_FILE, PRIVILEGE_CONFIG_XML,
				runtimeConfiguration);
		PrivilegeHandler privilegeHandler = PrivilegeInitializationHelper.initializeFromXml(privilegeConfigFile);
		this.privilegeHandler = privilegeHandler;
	}

	/**
	 * @param username
	 * @param password
	 * @return
	 * 
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#authenticate(String, byte[])
	 */
	@Override
	public Certificate authenticate(String username, byte[] password) {
		assertContainerStarted();
		return this.privilegeHandler.authenticate(username, password);
	}

	/**
	 * @param certificate
	 * @throws PrivilegeException
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#isCertificateValid(Certificate)
	 */
	@Override
	public void isCertificateValid(Certificate certificate) throws PrivilegeException {
		assertContainerStarted();
		this.privilegeHandler.isCertificateValid(certificate);
	}

	/**
	 * @param certificate
	 * @return
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#invalidateSession(ch.eitchnet.privilege.model.Certificate)
	 */
	@Override
	public boolean invalidateSession(Certificate certificate) {
		assertContainerStarted();
		return this.privilegeHandler.invalidateSession(certificate);
	}

	/**
	 * @param certificate
	 * @return
	 * @throws PrivilegeException
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#getPrivilegeContext(ch.eitchnet.privilege.model.Certificate)
	 */
	@Override
	public PrivilegeContext getPrivilegeContext(Certificate certificate) throws PrivilegeException {
		assertContainerStarted();
		return this.privilegeHandler.getPrivilegeContext(certificate);
	}

	/**
	 * @param systemUsername
	 * @param action
	 * @throws PrivilegeException
	 * @see ch.eitchnet.privilege.handler.PrivilegeHandler#runAsSystem(java.lang.String,
	 *      ch.eitchnet.privilege.handler.SystemUserAction)
	 */
	@Override
	public void runAsSystem(String systemUsername, SystemUserAction action) throws PrivilegeException {
		assertContainerStarted();
		this.privilegeHandler.runAsSystem(systemUsername, action);
	}

	@Override
	public PrivilegeHandler getPrivilegeHandler(Certificate certificate) throws PrivilegeException {
		assertContainerStarted();
		this.privilegeHandler.assertIsPrivilegeAdmin(certificate);
		return this.privilegeHandler;
	}
}
