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

import jakarta.mail.Multipart;
import jakarta.mail.internet.MimeBodyPart;
import jakarta.mail.internet.MimeMessage;
import org.bouncycastle.openpgp.api.OpenPGPCertificate;
import org.bouncycastle.openpgp.api.OpenPGPKey;
import org.junit.Test;
import org.pgpainless.PGPainless;
import org.pgpainless.decryption_verification.ConsumerOptions;
import org.pgpainless.decryption_verification.DecryptionStream;
import org.pgpainless.key.protection.SecretKeyRingProtector;
import org.pgpainless.util.Passphrase;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.util.Properties;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.*;

public class SimulatedSmtpMailerTest {

	private static final String SIGNING_KEY_FILE_NAME = "src/test/resources/strolch_example.key";
	private static final char[] SIGNING_KEY_PASSWORD = "example".toCharArray();
	private static final String RECIPIENT_PUBLIC_KEY_FILE_NAME = "src/test/resources/eitch@eitchnet.ch.asc";
	private static final String RECIPIENT = "recipient@example.com";
	private static final String SUBJECT = "Simulated email";

	@Test
	public void shouldSendEncryptedMessageWithoutKeysWhenSimulated() throws Exception {
		CapturingSimulatedSmtpMailer mailer = createSimulatedMailer(false, false);

		mailer.sendEncryptedEmail(RECIPIENT, SUBJECT, "Public note", "Secret text", "secret.txt");

		assertPlainAttachment(mailer.sentMessage);
	}

	@Test
	public void shouldSkipEncryptionWhenOnlySigningKeyIsAvailable() throws Exception {
		CapturingSimulatedSmtpMailer mailer = createSimulatedMailer(true, false);

		mailer.sendEncryptedEmail(RECIPIENT, SUBJECT, "Public note", "Secret text", "secret.txt");

		assertPlainAttachment(mailer.sentMessage);
	}

	@Test
	public void shouldSkipEncryptionWhenOnlyRecipientCertificateIsAvailable() throws Exception {
		CapturingSimulatedSmtpMailer mailer = createSimulatedMailer(false, true);

		mailer.sendEncryptedEmail(RECIPIENT, SUBJECT, "Public note", "Secret text", "secret.txt");

		assertPlainAttachment(mailer.sentMessage);
	}

	@Test
	public void shouldEncryptMessageAndAttachmentsWhenKeysAreAvailable() throws Exception {
		OpenPGPKey signingKey = loadSigningKey();
		CapturingSimulatedSmtpMailer mailer = new CapturingSimulatedSmtpMailer(signingKey.toCertificate());
		mailer.setSigningKeyFileName(SIGNING_KEY_FILE_NAME, SIGNING_KEY_PASSWORD);
		mailer.addRecipientPublicKeyFileName(RECIPIENT_PUBLIC_KEY_FILE_NAME);
		MailAttachment attachment = new MailAttachment("Attachément text", "attachment.txt", "text/plain", false,
				true);

		mailer.sendEncryptedEmailWithAttachment(RECIPIENT, SUBJECT, "Public note", "Sëcret text", "secret.txt",
				attachment);

		Multipart multipart = getMultipart(mailer.sentMessage);
		assertEquals("Public note", multipart.getBodyPart(0).getContent());
		assertEncryptedPart(multipart.getBodyPart(1));
		assertEncryptedPart(multipart.getBodyPart(2));

		assertEquals("Sëcret text", decryptAndVerify(multipart.getBodyPart(1), signingKey));
		assertEquals("Attachément text", decryptAndVerify(multipart.getBodyPart(2), signingKey));
	}

	@Test
	public void shouldSendEncryptedMessageAndAttachmentsAsPlaintextWhenSimulatedWithoutKeys() throws Exception {
		CapturingSimulatedSmtpMailer mailer = createSimulatedMailer(false, false);
		MailAttachment attachment = new MailAttachment("Attachment text", "attachment.txt", "text/plain", true,
				true);

		mailer.sendEncryptedEmailWithAttachment(RECIPIENT, SUBJECT, "Public note", "Secret text", "secret.txt",
				attachment);

		Multipart multipart = getMultipart(mailer.sentMessage);
		assertEquals("Public note", multipart.getBodyPart(0).getContent());
		assertPlainPart(multipart.getBodyPart(1), "secret.txt", "Secret text");
		assertPlainPart(multipart.getBodyPart(2), "attachment.txt", "Attachment text");
	}

	@Test
	public void shouldSendEncryptedAttachmentAsPlaintextWhenSimulatedWithoutKeys() throws Exception {
		CapturingSimulatedSmtpMailer mailer = createSimulatedMailer(false, false);
		MailAttachment attachment = new MailAttachment("Attachment text", "attachment.txt", "text/plain", false,
				true);

		mailer.sendMailWithAttachment(RECIPIENT, SUBJECT, "Public note", false, attachment);

		Multipart multipart = getMultipart(mailer.sentMessage);
		assertEquals("Public note", multipart.getBodyPart(0).getContent());
		assertPlainPart(multipart.getBodyPart(1), "attachment.txt", "Attachment text");
	}

	@Test
	public void shouldRejectEncryptedMailWithoutKeysWhenNotSimulated() {
		CapturingSmtpMailer mailer = new CapturingSmtpMailer();

		assertThrows(RuntimeException.class,
				() -> mailer.sendEncryptedEmail(RECIPIENT, SUBJECT, "Public note", "Secret text", "secret.txt"));
		assertNull(mailer.sentMessage);

		assertThrows(RuntimeException.class, () -> mailer.sendEncryptedEmailWithAttachment(RECIPIENT, SUBJECT,
				"Public note", "Secret text", "secret.txt", new MailAttachment("Attachment", "attachment.txt")));
		assertNull(mailer.sentMessage);
	}

	@Test
	public void shouldLogShortSimulatedMessages() {
		SimulatedSmtpMailer mailer = new SimulatedSmtpMailer(properties());

		mailer.sendUnsignedMail(RECIPIENT, SUBJECT, "Short message");
	}

	private static CapturingSimulatedSmtpMailer createSimulatedMailer(boolean signingKey, boolean recipientCertificate) {
		CapturingSimulatedSmtpMailer mailer = new CapturingSimulatedSmtpMailer();
		if (signingKey)
			mailer.setSigningKeyFileName(SIGNING_KEY_FILE_NAME, SIGNING_KEY_PASSWORD);
		if (recipientCertificate)
			mailer.addRecipientPublicKeyFileName(RECIPIENT_PUBLIC_KEY_FILE_NAME);
		return mailer;
	}

	private static Multipart getMultipart(MimeMessage message) throws Exception {
		assertNotNull(message);
		return (Multipart) message.getContent();
	}

	private static OpenPGPKey loadSigningKey() throws Exception {
		try (FileInputStream in = new FileInputStream(SIGNING_KEY_FILE_NAME)) {
			return PGPainless.getInstance().readKey().parseKey(in);
		}
	}

	private static String decryptAndVerify(jakarta.mail.BodyPart bodyPart, OpenPGPKey signingKey) throws Exception {
		var signingCertificate = signingKey.toCertificate();
		ConsumerOptions options = ConsumerOptions
				.get(PGPainless.getInstance())
				.addDecryptionKey(signingKey,
						SecretKeyRingProtector.unlockAnyKeyWith(new Passphrase(SIGNING_KEY_PASSWORD)))
				.addVerificationCert(signingCertificate);
		DecryptionStream decryptionStream = PGPainless
				.getInstance()
				.processMessage()
				.onInputStream(new ByteArrayInputStream(bodyPart.getContent().toString().getBytes(UTF_8)))
				.withOptions(options);
		String decrypted;
		try (decryptionStream) {
			decrypted = new String(decryptionStream.readAllBytes(), UTF_8);
		}
		assertTrue(decryptionStream.getMetadata().isVerifiedSignedBy(signingCertificate));
		return decrypted;
	}

	private static void assertPlainAttachment(MimeMessage message) throws Exception {
		Multipart multipart = getMultipart(message);
		assertEquals("Public note", multipart.getBodyPart(0).getContent());
		assertPlainPart(multipart.getBodyPart(1), "secret.txt", "Secret text");
	}

	private static void assertPlainPart(jakarta.mail.BodyPart bodyPart, String fileName, String content)
			throws Exception {
		assertEquals(fileName, bodyPart.getFileName());
		assertEquals(content, bodyPart.getContent());
		assertFalse(bodyPart.getContent().toString().contains("-----BEGIN PGP"));
	}

	private static void assertEncryptedPart(jakarta.mail.BodyPart bodyPart) throws Exception {
		assertTrue(bodyPart instanceof MimeBodyPart);
		assertTrue(bodyPart.getFileName().endsWith(".asc"));
		assertTrue(bodyPart.getContent().toString().contains("-----BEGIN PGP MESSAGE-----"));
	}

	private static Properties properties() {
		Properties properties = new Properties();
		properties.setProperty(SmtpMailer.PARAM_FROM_ADDR, "mailer@example.com");
		properties.setProperty(SmtpMailer.PARAM_HOST, "localhost");
		properties.setProperty(SmtpMailer.PARAM_PORT, "25");
		properties.setProperty(SmtpMailer.PARAM_AUTH, "false");
		properties.setProperty(SmtpMailer.PARAM_START_TLS, "false");
		return properties;
	}

	private static class CapturingSimulatedSmtpMailer extends SimulatedSmtpMailer {

		private final OpenPGPCertificate recipientCertificate;
		private MimeMessage sentMessage;

		private CapturingSimulatedSmtpMailer() {
			this(null);
		}

		private CapturingSimulatedSmtpMailer(OpenPGPCertificate recipientCertificate) {
			super(properties());
			this.recipientCertificate = recipientCertificate;
		}

		@Override
		protected OpenPGPCertificate getRecipientCertificate(String recipientPublicKeyFileName) {
			if (this.recipientCertificate != null)
				return this.recipientCertificate;
			return super.getRecipientCertificate(recipientPublicKeyFileName);
		}

		@Override
		protected void send(jakarta.mail.internet.InternetAddress[] recipients, MimeMessage message) {
			this.sentMessage = message;
		}
	}

	private static class CapturingSmtpMailer extends SmtpMailer {

		private MimeMessage sentMessage;

		private CapturingSmtpMailer() {
			super(properties());
		}

		@Override
		protected void send(jakarta.mail.internet.InternetAddress[] recipients, MimeMessage message) {
			this.sentMessage = message;
		}
	}
}
