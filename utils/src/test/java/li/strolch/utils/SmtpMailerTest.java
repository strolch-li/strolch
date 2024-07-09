package li.strolch.utils;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

@Ignore("Requires configured username and password")
public class SmtpMailerTest {

	public static final String SIGNING_KEY_FILE_NAME = "src/test/resources/strolch_example.key";
	public static final char[] SIGNING_KEY_PASSWORD = "example".toCharArray();
	public static final String RECIPIENT_PUBLIC_KEY_FILE_NAME = "src/test/resources/eitch@eitchnet.ch.asc";

	private SmtpMailer emailSender;

	@Before
	public void setUpBefore() {
		String fromAddress = "\"Strolch Email Test\" <network@atexxi.ch>";
		String host = "smtp.gmail.com";
		int port = 587;
		boolean auth = true;
		boolean startTls = true;
		String username = System.getenv("email.username");
		String password = System.getenv("email.password");
		this.emailSender = new SmtpMailer(fromAddress, host, port, auth, startTls, username, password);
	}

	@Test
	public void shouldSendUnsignedMail() {

		String recipient = "\"Robert von Burg\" <eitch@eitchnet.ch>";
		String subject = "Unsigned email test";
		String plainText = "This text is unsigned!";

		emailSender.sendMail(recipient, subject, plainText);
	}

	@Test
	public void shouldSendSignedMail() {

		emailSender.setSigningKeyFileName(SIGNING_KEY_FILE_NAME, SIGNING_KEY_PASSWORD);

		String recipient = "\"Robert von Burg\" <eitch@eitchnet.ch>";
		String subject = "Signed email test";
		String plainText = "This text should be signed!";

		emailSender.sendMail(recipient, subject, plainText);
	}

	@Test
	public void shouldSendEncryptedMail() {

		emailSender.addRecipientPublicKeyFileName(RECIPIENT_PUBLIC_KEY_FILE_NAME);
		emailSender.setSigningKeyFileName(SIGNING_KEY_FILE_NAME, SIGNING_KEY_PASSWORD);

		String recipient = "\"Robert von Burg\" <eitch@eitchnet.ch>";
		String subject = "Encrypted email test";
		String secretText = "This is the plain text!";

		String mailText = "This is an encrypted mail. Please decrypt the attached file for details.";
		String encryptedTextFileName = "encrypted-text_" + System.currentTimeMillis() + ".txt";
		emailSender.sendEncryptedEmail(recipient, subject, mailText, secretText, encryptedTextFileName);
	}

	@Test
	public void shouldSendEncryptedMailWithAttachment() {

		emailSender.addRecipientPublicKeyFileName(RECIPIENT_PUBLIC_KEY_FILE_NAME);
		emailSender.setSigningKeyFileName(SIGNING_KEY_FILE_NAME, SIGNING_KEY_PASSWORD);

		String recipient = "\"Robert von Burg\" <eitch@eitchnet.ch>";
		String subject = "Encrypted email test with attachment";
		String secretText = "This is the plain text!";

		String mailText = "This is an encrypted mail with attachments. Please decrypt the attached files for details.";
		long timestamp = System.currentTimeMillis();
		String encryptedTextFileName = "encrypted-text_" + timestamp + ".txt";
		String attachment = "This is the attached text";
		String fileName = "attachment_" + timestamp + ".txt";
		emailSender.sendEncryptedEmailWithAttachment(recipient, subject, mailText, secretText, encryptedTextFileName,
				attachment, fileName);
	}
}
