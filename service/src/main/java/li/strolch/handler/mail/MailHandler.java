package li.strolch.handler.mail;

import jakarta.mail.internet.InternetAddress;

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
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 */
	public abstract void sendMail(String subject, String text, String recipients);

	/**
	 * Sends an e-mail with an attachment to the given recipients
	 *
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param attachment the attachment as a string
	 * @param fileName   the file name of the attachment
	 * @param type       the mime type of the attachment
	 */
	public abstract void sendMail(String subject, String text, String recipients, String attachment, String fileName,
								  String type);

	/**
	 * Sends an e-mail to the given recipients asynchronously
	 *
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 */
	public abstract void sendMailAsync(String subject, String text, String recipients);

	/**
	 * Sends an e-mail with an attachment to the given recipients asynchronously
	 *
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param attachment the attachment as a string
	 * @param fileName   the file name of the attachment
	 * @param type       the mime type of the attachment
	 */
	public abstract void sendMailAsync(String subject, String text, String recipients, String attachment,
									   String fileName, String type);
}
