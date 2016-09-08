package li.strolch.handler.mail;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.runtime.configuration.ComponentConfiguration;
import li.strolch.utils.SmtpMailer;

/**
 * SMTP {@link MailHandler}. Uses {@link SmtpMailer}
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 * @see SmtpMailer
 */
public class SmtpMailHandler extends MailHandler {

	public SmtpMailHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	/**
	 * See {@link SmtpMailer} for which configuration properties are required
	 * 
	 * @see SmtpMailer
	 */
	@Override
	public void initialize(ComponentConfiguration configuration) throws Exception {

		SmtpMailer.init(configuration.getAsProperties());

		super.initialize(configuration);
	}

	@Override
	public void sendMail(String subject, String text, String recipient) {
		SmtpMailer.getInstance().sendMail(subject, text, recipient);
	}
}
