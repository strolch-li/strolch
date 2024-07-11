package li.strolch.handler.mail;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.SimulatedSmtpMailer;
import li.strolch.utils.SmtpMailer;

public class SimulatedMailHandler extends SmtpMailHandler {

	public SimulatedMailHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	@Override
	protected SimulatedSmtpMailer initializeSmtpMailer(ComponentConfiguration configuration) {
		return SimulatedSmtpMailer.init(configuration.getAsProperties());
	}

	@Override
	protected SmtpMailer getSmtpMailer() {
		return SimulatedSmtpMailer.getInstance();
	}
}
