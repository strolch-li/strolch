package li.strolch.runtime.privilege;

import java.io.File;

import li.strolch.runtime.component.ComponentContainer;
import li.strolch.runtime.component.StrolchComponent;
import li.strolch.runtime.configuration.ComponentConfiguration;
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

	public DefaultStrolchPrivilegeHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	public void initialize(ComponentConfiguration configuration) {
		super.initialize(configuration);

		// initialize privilege
		File privilegeConfigFile = configuration.getConfigFile(PROP_PRIVILEGE_CONFIG_FILE, PRIVILEGE_CONFIG_XML,
				configuration);
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
