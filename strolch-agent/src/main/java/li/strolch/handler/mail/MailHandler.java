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
	 * <p>Sends an e-mail to given recipient.</p>
	 *
	 * <p>If encryption is enabled, then the sent mail will be encrypted</p>
	 *
	 * <p>If signing is enabled, then the sent mail will be signed</p>
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 */
	public abstract void sendMail(String recipients, String subject, String text);

	/**
	 * <p>Sends an encrypted e-mail to given recipient.</p>
	 *
	 * <p>If encryption is not enabled, then an {@link IllegalStateException} is thrown</p>
	 *
	 * <p>If signing is not enabled, then an {@link IllegalStateException} is thrown</p>
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 */
	public abstract void sendEncryptedMail(String recipients, String subject, String text);

	/**
	 * <p>Sends an e-mail to given recipient.</p>
	 *
	 * <p>This method guarantees the email is not encrypted</p>
	 *
	 * <p>If signing is enabled, then the sent mail will be signed</p>
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 */
	public abstract void sendUnencryptedMail(String recipients, String subject, String text);

	/**
	 * <p>Sends an e-mail with an attachment to the given recipients.</p>
	 *
	 * <p>If encryption is enabled, then the sent mail will be encrypted</p>
	 *
	 * <p>If signing is enabled, then the sent mail will be signed</p>
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
	 * <p>Sends an encrypted e-mail with an encrypted attachment to the given recipients.</p>
	 *
	 * <p>If encryption is not enabled, then an {@link IllegalStateException} is thrown</p>
	 *
	 * <p>If signing is not enabled, then an {@link IllegalStateException} is thrown</p>
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 * @param attachment the attachment as a string
	 * @param fileName   the file name of the attachment
	 * @param type       the mime type of the attachment
	 */
	public abstract void sendEncryptedMailWithAttachment(String recipients, String subject, String text,
			String attachment, String fileName, String type);

	/**
	 * <p>Sends an e-mail with an attachment to the given recipients.</p>
	 *
	 * <p>This method guarantees the email is not encrypted</p>
	 *
	 * <p>If signing is enabled, then the sent mail will be signed</p>
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 * @param attachment the attachment as a string
	 * @param fileName   the file name of the attachment
	 * @param type       the mime type of the attachment
	 */
	public abstract void sendUnencryptedMailWithAttachment(String recipients, String subject, String text,
			String attachment, String fileName, String type);

	/**
	 * <p>Sends an e-mail to the given recipients asynchronously.</p>
	 *
	 * <p>If encryption is enabled, then the sent mail will be encrypted</p>
	 *
	 * <p>If signing is enabled, then the sent mail will be signed</p>
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 */
	public abstract void sendMailAsync(String recipients, String subject, String text);

	/**
	 * <p>Sends an encrypted e-mail to the given recipients asynchronously.</p>
	 *
	 * <p>If encryption is not enabled, then an {@link IllegalStateException} is thrown</p>
	 *
	 * <p>If signing is not enabled, then an {@link IllegalStateException} is thrown</p>
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 */
	public abstract void sendEncryptedMailAsync(String recipients, String subject, String text);

	/**
	 * <p>Sends an e-mail to the given recipients asynchronously</p>
	 *
	 * <p>This method guarantees the email is not encrypted</p>
	 *
	 * <p>If signing is enabled, then the sent mail will be signed</p>
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 */
	public abstract void sendUnencryptedMailAsync(String recipients, String subject, String text);

	/**
	 * <p>Sends an e-mail with an attachment to the given recipients asynchronously.</p>
	 *
	 * <p>If encryption is enabled, then the sent mail will be encrypted</p>
	 *
	 * <p>If signing is enabled, then the sent mail will be signed</p>
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
	 * <p>Sends an e-mail with an attachment to the given recipients asynchronously.</p>
	 *
	 * <p>If encryption is not enabled, then an {@link IllegalStateException} is thrown</p>
	 *
	 * <p>If signing is not enabled, then an {@link IllegalStateException} is thrown</p>
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 * @param attachment the attachment as a string
	 * @param fileName   the file name of the attachment
	 * @param type       the mime type of the attachment
	 */
	public abstract void sendEncryptedMailWithAttachmentAsync(String recipients, String subject, String text,
			String attachment, String fileName, String type);

	/**
	 * <p>Sends an e-mail with an attachment to the given recipients asynchronously.</p>
	 *
	 * <p>This method guarantees the email is not encrypted</p>
	 *
	 * <p>If signing is enabled, then the sent mail will be signed</p>
	 *
	 * @param recipients the comma separated list of addresses to whom to send the e-mail see
	 *                   {@link InternetAddress#parse(String)}
	 * @param subject    the subject of the e-mail
	 * @param text       the test of the e-mail
	 * @param attachment the attachment as a string
	 * @param fileName   the file name of the attachment
	 * @param type       the mime type of the attachment
	 */
	public abstract void sendUnencryptedMailWithAttachmentAsync(String recipients, String subject, String text,
			String attachment, String fileName, String type);

}
