package li.strolch.utils;

import org.bouncycastle.openpgp.PGPPublicKeyRing;
import org.bouncycastle.openpgp.PGPSecretKeyRing;
import org.bouncycastle.util.io.Streams;
import org.junit.Ignore;
import org.junit.Test;
import org.pgpainless.PGPainless;
import org.pgpainless.algorithm.DocumentSignatureType;
import org.pgpainless.algorithm.HashAlgorithm;
import org.pgpainless.algorithm.SymmetricKeyAlgorithm;
import org.pgpainless.encryption_signing.*;
import org.pgpainless.key.protection.SecretKeyRingProtector;
import org.pgpainless.util.Passphrase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.nio.charset.StandardCharsets;

public class SmtpEncryptedEmailSenderTest {

	private static final Logger log = LoggerFactory.getLogger(SmtpEncryptedEmailSenderTest.class);

	@Ignore("Requires configured username and password")
	@Test
	public void shouldSendMail() {

		String recipientPublicKeyFileName = "src/test/resources/eitch@eitchnet.ch.asc";
		String signingKeyFileName = "src/test/resources/strolch_example.key";
		char[] signingKeyPassword = "example".toCharArray();

		String fromAddress = "\"Strolch Email Test\" <network@atexxi.ch>";
		String host = "smtp.gmail.com";
		int port = 587;
		boolean auth = true;
		boolean startTls = true;
		String username = System.getenv("email.username");
		String password = System.getenv("email.password");
		SmtpEncryptedEmailSender emailSender = new SmtpEncryptedEmailSender(fromAddress, host, port, auth, startTls, username, password);
		emailSender.setRecipientPublicKeyFileName(recipientPublicKeyFileName);
		emailSender.setSigningKeyFileName(signingKeyFileName);
		emailSender.setSigningKeyPassword(signingKeyPassword);

		String recipient = "\"Robert von Burg\" <eitch@eitchnet.ch>";
		String subject = "Encrypted email test";
		String plainText = "This is the plain text!";

		emailSender.sendSignedAndEncryptedEmail(recipient, subject,
				"This is an encrypted mail. Please decrypt the attached file for details.", plainText,
				"my-encrypted-file.asc");
	}

	@Test
	public void shouldEncryptAndSign() throws Exception {

		String recipientPublicKeyFileName = "src/test/resources/eitch@eitchnet.ch.asc";
		String signingKeyFileName = "src/test/resources/strolch_example.key";
		char[] signingKeyPassword = "example".toCharArray();

		// input
		String plainText = "hello world";
		byte[] plainBytes = plainText.getBytes(StandardCharsets.UTF_8);
		ByteArrayInputStream inputStream = new ByteArrayInputStream(plainBytes);

		// get public key (recipient)
		PGPPublicKeyRing recipientKeyRing = PGPainless
				.readKeyRing()
				.publicKeyRing(new FileInputStream(recipientPublicKeyFileName));
		if (recipientKeyRing == null)
			throw new IllegalStateException("No public key found for recipient key file " + recipientPublicKeyFileName);

		// get secret key (sender/signer)

		PGPSecretKeyRing signingKeyRing = PGPainless
				.readKeyRing()
				.secretKeyRing(new FileInputStream(signingKeyFileName));
		if (signingKeyRing == null)
			throw new IllegalStateException("No secret key ring found for signing key file " + signingKeyFileName);

		ByteArrayOutputStream signatureResult = new ByteArrayOutputStream();

		// If you use a single passphrase for all (sub-) keys, take this:
		SecretKeyRingProtector secretKeyDecryptor = SecretKeyRingProtector.unlockAnyKeyWith(
				new Passphrase(signingKeyPassword));
		EncryptionStream encryptionStream = PGPainless
				.encryptAndOrSign()
				.onOutputStream(signatureResult)
				.withOptions(ProducerOptions.signAndEncrypt(new EncryptionOptions().addRecipient(recipientKeyRing)
						// optionally override symmetric encryption algorithm
						.overrideEncryptionAlgorithm(SymmetricKeyAlgorithm.AES_256), new SigningOptions()
						// Sign in-line (using one-pass-signature packet)
						.addInlineSignature(secretKeyDecryptor, signingKeyRing,
								DocumentSignatureType.CANONICAL_TEXT_DOCUMENT)
						// optionally override hash algorithm
						.overrideHashAlgorithm(HashAlgorithm.SHA256)).setAsciiArmor(true) // Ascii armor or not
				);

		Streams.pipeAll(inputStream, encryptionStream);
		encryptionStream.close();

		// Information about the encryption (algorithms, detached signatures etc.)
		EncryptionResult result = encryptionStream.getResult();
		log.info("{}: encoding:{}:\n{}", result.getFileName(), result.getFileEncoding().name(), signatureResult);
	}
}
