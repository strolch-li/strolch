package li.strolch.runtime.privilege;

import java.io.File;

import li.strolch.runtime.component.ComponentState;
import li.strolch.runtime.component.StrolchComponent;
import li.strolch.runtime.configuration.ComponentConfiguration;
import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.handler.PrivilegeHandler;
import ch.eitchnet.privilege.helper.PrivilegeInitializationHelper;
import ch.eitchnet.privilege.model.Certificate;

public class StrolchPrivilegeHandler extends StrolchComponent {

	private PrivilegeHandler privilegeHandler;

	@Override
	public void initialize(ComponentConfiguration configuration) {
		super.initialize(configuration);

		// initialize privilege
		File privilegeConfigFile = new File(configuration.getRuntimeConfiguration().getRootPath()
				+ "/config/Privilege.xml");
		this.privilegeHandler = PrivilegeInitializationHelper.initializeFromXml(privilegeConfigFile);
	}

	public Certificate authenticate(String username, byte[] password) {
		assertStarted();
		return this.privilegeHandler.authenticate(username, password);
	}

	public void isCertificateValid(Certificate certificate) throws PrivilegeException {
		assertStarted();
		this.privilegeHandler.isCertificateValid(certificate);
	}

	private void assertStarted() {
		if (getState() != ComponentState.STARTED)
			throw new IllegalStateException("Component StrolchPrivilegeHandler is not yet started!"); //$NON-NLS-1$
	}

}
