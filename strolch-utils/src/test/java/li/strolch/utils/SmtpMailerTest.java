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

import org.bouncycastle.openpgp.api.OpenPGPKey;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.pgpainless.PGPainless;
import org.pgpainless.algorithm.DocumentSignatureType;
import org.pgpainless.algorithm.OpenPGPKeyVersion;
import org.pgpainless.key.protection.SecretKeyRingProtector;
import org.pgpainless.util.Passphrase;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.util.Date;
import java.util.Optional;
import java.util.Properties;

import static org.junit.Assert.*;

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
		this.emailSender = createMailer();
		this.emailSender.addRecipientPublicKeyFileName(RECIPIENT_PUBLIC_KEY_FILE_NAME);
		this.emailSender.setSigningKeyFileName(SIGNING_KEY_FILE_NAME, SIGNING_KEY_PASSWORD);
	}

	private static SmtpMailer createMailer() {
		int port = 587;
		String username = System.getenv("email.username");
		String password = System.getenv("email.password");
		boolean auth = username != null && password != null;
		boolean startTls = true;

		Properties props = new Properties();
		props.setProperty(SmtpMailer.PARAM_FROM_ADDR, SENDER);
		props.setProperty(SmtpMailer.PARAM_AUTH, String.valueOf(auth));
		if (auth) {
			props.setProperty(SmtpMailer.PARAM_USERNAME, username);
			props.setProperty(SmtpMailer.PARAM_PASSWORD, password);
		}
		props.setProperty(SmtpMailer.PARAM_START_TLS, String.valueOf(startTls));
		props.setProperty(SmtpMailer.PARAM_HOST, SMTP_HOST);
		props.setProperty(SmtpMailer.PARAM_PORT, String.valueOf(port));

		return new SmtpMailer(props);
	}

	@Test
	public void shouldSignCanonicalTextDocument() {
		String plainText = "Hello world!\nThis is a signed text document.";
		String signed = this.emailSender.sign(plainText, DocumentSignatureType.CANONICAL_TEXT_DOCUMENT);
		assertNotNull(signed);
		assertTrue(signed.contains("-----BEGIN PGP SIGNED MESSAGE-----"));
		assertTrue(signed.contains("-----BEGIN PGP SIGNATURE-----"));
		assertTrue(signed.contains("-----END PGP SIGNATURE-----"));
		assertTrue(signed.contains(plainText));
	}

	@Test
	public void shouldSignBinaryDocument() {
		String plainText = "Hello binary document!";
		String signed = this.emailSender.sign(plainText, DocumentSignatureType.BINARY_DOCUMENT);
		assertNotNull(signed);
		assertTrue(signed.contains("-----BEGIN PGP SIGNED MESSAGE-----"));
		assertTrue(signed.contains("-----BEGIN PGP SIGNATURE-----"));
		assertTrue(signed.contains("-----END PGP SIGNATURE-----"));
		assertTrue(signed.contains(plainText));
	}

	@Test
	public void shouldReturnExpirationOfExampleKey() {
		assertNotNull(this.emailSender.getSigningKeyFingerprint());
		Instant expiration = this.emailSender.getSigningKeyExpiration().orElseThrow();
		assertTrue(expiration.isAfter(Instant.now().plus(Duration.ofDays(90))));
	}

	@Test
	public void shouldReturnNoExpirationWithoutSigningKey() {
		assertEquals(Optional.empty(), createMailer().getSigningKeyExpiration());
	}

	@Test
	public void shouldReturnExpirationOfExpiringKey() throws Exception {
		Instant expiration = Instant.now().plus(Duration.ofDays(30));
		File keyFile = createKeyFile(Instant.now().minus(Duration.ofDays(365)), Instant.now(), expiration);

		this.emailSender.setSigningKeyFileName(keyFile.getAbsolutePath(), "test".toCharArray());

		Instant signingKeyExpiration = this.emailSender.getSigningKeyExpiration().orElseThrow();
		// expiration dates are stored in seconds
		assertTrue(Duration.between(expiration, signingKeyExpiration).abs().getSeconds() <= 1);
	}

	@Test
	public void shouldFailToSetExpiredSigningKey() throws Exception {
		Instant created = Instant.now().minus(Duration.ofDays(365));
		Instant evaluated = Instant.now().minus(Duration.ofDays(200));
		File keyFile = createKeyFile(created, evaluated, Instant.now().minus(Duration.ofDays(100)));

		SmtpMailer mailer = createMailer();
		IllegalStateException e = assertThrows(IllegalStateException.class,
				() -> mailer.setSigningKeyFileName(keyFile.getAbsolutePath(), "test".toCharArray()));
		assertTrue(e.getMessage(), e.getMessage().contains("has expired"));
		assertNull(mailer.getSigningKeyFingerprint());
	}

	private static File createKeyFile(Instant created, Instant evaluationTime, Instant expiration) throws Exception {
		OpenPGPKey key = PGPainless
				.getInstance()
				.generateKey(OpenPGPKeyVersion.v4, Date.from(created))
				.modernKeyRing("Test <test@example.com>", Passphrase.fromPassword("test"));
		key = PGPainless
				.getInstance()
				.modify(key, Date.from(evaluationTime))
				.setExpirationDate(Date.from(expiration),
						SecretKeyRingProtector.unlockAnyKeyWith(Passphrase.fromPassword("test")))
				.done();

		File file = writeKey(key);
		file.deleteOnExit();
		return file;
	}

	private static File writeKey(OpenPGPKey key) throws IOException {
		Path path = Files.createTempFile("signing-key", ".asc");
		Files.writeString(path, key.toAsciiArmoredString());
		return path.toFile();
	}

	@Ignore("Requires configured username and password")
	@Test
	public void shouldSendUnsignedMail() {

		String subject = "Unsigned email test";
		String plainText = "This text is unsigned!";

		this.emailSender.sendUnsignedMail(RECIPIENT, subject, plainText);
	}

	@Ignore("Requires configured username and password")
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

	@Ignore("Requires configured username and password")
	@Test
	public void shouldSendUnsignedMailWithSignedAttachment() {

		String subject = "Unsigned email test with signed attachment";
		String plainText
				= "Hello user\n\nThis text is unsigned!\n\nWe are making it quite long, adding indentations to see how it gets mangled by the mail reader\n\nFurthermore, this mail has an attachment, which should be signed\n\n  regards, Your server!";

		long timestamp = System.currentTimeMillis();
		MailAttachment mailAttachment = new MailAttachment(plainText, "attachment_" + timestamp + ".txt", true);
		this.emailSender.sendUnsignedMailWithAttachment(RECIPIENT, subject, plainText, mailAttachment);
	}

	@Ignore("Requires configured username and password")
	@Test
	public void shouldSendSignedMail() {

		this.emailSender.setSigningKeyFileName(SIGNING_KEY_FILE_NAME, SIGNING_KEY_PASSWORD);

		String subject = "Signed email test";
		String plainText = "This text should be signed!";

		this.emailSender.sendMailSignedIfAvailable(RECIPIENT, subject, plainText);
	}

	@Ignore("Requires configured username and password")
	@Test
	public void shouldSendEncryptedMail() {

		String subject = "Encrypted email test";
		String secretText = "This is the plain text!";

		String mailText = "This is an encrypted mail. Please decrypt the attached file for details.";
		String encryptedTextFileName = "encrypted-text_" + System.currentTimeMillis() + ".txt";
		this.emailSender.sendEncryptedEmail(RECIPIENT, subject, mailText, secretText, encryptedTextFileName);
	}

	@Ignore("Requires configured username and password")
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
