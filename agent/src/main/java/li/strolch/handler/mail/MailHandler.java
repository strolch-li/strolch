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
	 * Returns true if GPG signing is configured and enabled
	 *
	 * @return true if GPG signing is configured and enabled
	 */
	public abstract boolean isSigningEnabled();

	/**
	 * Returns true if GPG encryption is configured and enabled, i.e. at least one GPG recipient key is available
	 *
	 * @return true if GPG encryption is configured and enabled
	 */
	public abstract boolean isEncryptionEnabled();

	/**
	 * Sends an e-mail to given recipient
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 */
	public abstract void sendMail(String recipients, String subject, String text);

	/**
	 * Sends an e-mail to given recipient
	 *
	 * @param recipients        the comma separated list of addresses to whom to send the e-mail see
	 *                          {@link InternetAddress#parse(String)}
	 * @param subject           the subject of the e-mail
	 * @param text              the test of the e-mail
	 * @param disableEncryption if encryption should be disabled for this email
	 */
	public abstract void sendMail(String recipients, String subject, String text, boolean disableEncryption);

	/**
	 * Sends an e-mail with an attachment to the given recipients
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 * @param attachment the attachment as a string
	 * @param fileName   the file name of the attachment
	 * @param type       the mime type of the attachment
	 */
	public abstract void sendMailWithAttachment(String recipients, String subject, String text, String attachment,
			String fileName, String type);

	/**
	 * Sends an e-mail with an attachment to the given recipients
	 *
	 * @param recipients        the comma separated list of addresses to whom to send the e-mail see
	 *                          {@link InternetAddress#parse(String)}
	 * @param subject           the subject of the e-mail
	 * @param text              the test of the e-mail
	 * @param attachment        the attachment as a string
	 * @param fileName          the file name of the attachment
	 * @param type              the mime type of the attachment
	 * @param disableEncryption if encryption should be disabled for this email
	 */
	public abstract void sendMailWithAttachment(String recipients, String subject, String text, String attachment,
			String fileName, String type, boolean disableEncryption);

	/**
	 * Sends an e-mail to the given recipients asynchronously
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 */
	public abstract void sendMailAsync(String recipients, String subject, String text);

	/**
	 * Sends an e-mail to the given recipients asynchronously
	 *
	 * @param recipients        the comma separated list of addresses to whom to send the e-mail see
	 *                          {@link InternetAddress#parse(String)}
	 * @param subject           the subject of the e-mail
	 * @param text              the test of the e-mail
	 * @param disableEncryption if encryption should be disabled for this email
	 */
	public abstract void sendMailAsync(String recipients, String subject, String text, boolean disableEncryption);

	/**
	 * Sends an e-mail with an attachment to the given recipients asynchronously
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 * @param attachment the attachment as a string
	 * @param fileName   the file name of the attachment
	 * @param type       the mime type of the attachment
	 */
	public abstract void sendMailWithAttachmentAsync(String recipients, String subject, String text, String attachment,
			String fileName, String type);

	/**
	 * Sends an e-mail with an attachment to the given recipients asynchronously
	 *
	 * @param recipients        the comma separated list of addresses to whom to send the e-mail see
	 *                          {@link InternetAddress#parse(String)}
	 * @param subject           the subject of the e-mail
	 * @param text              the test of the e-mail
	 * @param attachment        the attachment as a string
	 * @param fileName          the file name of the attachment
	 * @param type              the mime type of the attachment
	 * @param disableEncryption if encryption should be disabled for this email
	 */
	public abstract void sendMailWithAttachmentAsync(String recipients, String subject, String text, String attachment,
			String fileName, String type, boolean disableEncryption);
}
