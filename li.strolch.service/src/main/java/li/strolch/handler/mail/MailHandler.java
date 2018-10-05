package li.strolch.handler.mail;

import li.strolch.agent.api.ComponentContainer;
import li.strolch.agent.api.StrolchComponent;

/**
 * {@link StrolchComponent} to send e-mails
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public abstract class MailHandler extends StrolchComponent {

	public MailHandler(ComponentContainer container, String componentName) {
		super(container, componentName);
	}

	/**
	 * Sends an e-mail to given recipient
	 *
	 * @param subject
	 * 		the subject of the e-mail
	 * @param text
	 * 		the test of the e-mail
	 * @param recipient
	 * 		the address to whom to send the e-mail
	 */
	public abstract void sendMail(String subject, String text, String recipient);
}
