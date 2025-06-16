/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

package li.strolch.utils;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Properties;

@Ignore("Requires configured username and password")
public class SmtpMailerTest {

	public static final String SIGNING_KEY_FILE_NAME = "src/test/resources/strolch_example.key";
	public static final char[] SIGNING_KEY_PASSWORD = "example".toCharArray();
	public static final String RECIPIENT_PUBLIC_KEY_FILE_NAME = "src/test/resources/eitch@eitchnet.ch.asc";

	public static final String RECIPIENT = "\"Robert von Burg\" <eitch@eitchnet.ch>";
	public static final String SENDER = "\"Strolch Email Test\" <network@atexxi.ch>";
	public static final String SMTP_HOST = "smtp.gmail.com";

	private SmtpMailer emailSender;

	@Before
	public void setUpBefore() {
		int port = 587;
		boolean auth = true;
		boolean startTls = true;
		String username = System.getenv("email.username");
		String password = System.getenv("email.password");

		Properties props = new Properties();
		props.setProperty(SmtpMailer.PARAM_FROM_ADDR, SENDER);
		props.setProperty(SmtpMailer.PARAM_AUTH, String.valueOf(auth));
		props.setProperty(SmtpMailer.PARAM_USERNAME, username);
		props.setProperty(SmtpMailer.PARAM_PASSWORD, password);
		props.setProperty(SmtpMailer.PARAM_START_TLS, String.valueOf(startTls));
		props.setProperty(SmtpMailer.PARAM_HOST, SMTP_HOST);
		props.setProperty(SmtpMailer.PARAM_PORT, String.valueOf(port));

		this.emailSender = new SmtpMailer(props);
		this.emailSender.addRecipientPublicKeyFileName(RECIPIENT_PUBLIC_KEY_FILE_NAME);
		this.emailSender.setSigningKeyFileName(SIGNING_KEY_FILE_NAME, SIGNING_KEY_PASSWORD);
	}

	@Test
	public void shouldSendUnsignedMail() {

		String subject = "Unsigned email test";
		String plainText = "This text is unsigned!";

		this.emailSender.sendUnsignedMail(RECIPIENT, subject, plainText);
	}

	@Test
	public void shouldSendUnsignedMailWithUnsignedAttachment() {
		String subject = "Unsigned email test with unsigned attachment";
		String plainText
				= "Hello user\n\nThis text is unsigned!\n\nWe are making it quite long, adding indentations to see how it gets mangled by the mail reader\n\n  regards, Your server!";

		long timestamp = System.currentTimeMillis();
		MailAttachment mailAttachment = new MailAttachment("This is the attached text",
				"attachment_" + timestamp + ".txt");
		this.emailSender.sendUnsignedMailWithAttachment(RECIPIENT, subject, plainText, mailAttachment);
	}

	@Test
	public void shouldSendUnsignedMailWithSignedAttachment() {

		String subject = "Unsigned email test with signed attachment";
		String plainText
				= "Hello user\n\nThis text is unsigned!\n\nWe are making it quite long, adding indentations to see how it gets mangled by the mail reader\n\nFurthermore, this mail has an attachment, which should be signed\n\n  regards, Your server!";

		long timestamp = System.currentTimeMillis();
		MailAttachment mailAttachment = new MailAttachment(plainText, "attachment_" + timestamp + ".txt", true);
		this.emailSender.sendUnsignedMailWithAttachment(RECIPIENT, subject, plainText, mailAttachment);
	}

	@Test
	public void shouldSendSignedMail() {

		this.emailSender.setSigningKeyFileName(SIGNING_KEY_FILE_NAME, SIGNING_KEY_PASSWORD);

		String subject = "Signed email test";
		String plainText = "This text should be signed!";

		this.emailSender.sendMailSignedIfAvailable(RECIPIENT, subject, plainText);
	}

	@Test
	public void shouldSendEncryptedMail() {

		String subject = "Encrypted email test";
		String secretText = "This is the plain text!";

		String mailText = "This is an encrypted mail. Please decrypt the attached file for details.";
		String encryptedTextFileName = "encrypted-text_" + System.currentTimeMillis() + ".txt";
		this.emailSender.sendEncryptedEmail(RECIPIENT, subject, mailText, secretText, encryptedTextFileName);
	}

	@Test
	public void shouldSendEncryptedMailWithAttachment() {

		String subject = "Encrypted email test with encrypted attachment";
		String secretText = "This is the plain text!";

		String mailText = "This is an encrypted mail with attachments. Please decrypt the attached files for details.";
		long timestamp = System.currentTimeMillis();
		String encryptedTextFileName = "encrypted-text_" + timestamp + ".txt";
		MailAttachment mailAttachment = new MailAttachment("This is the attached text",
				"attachment_" + timestamp + ".txt");
		this.emailSender.sendEncryptedEmailWithAttachment(RECIPIENT, subject, mailText, secretText,
				encryptedTextFileName, mailAttachment);
	}
}
