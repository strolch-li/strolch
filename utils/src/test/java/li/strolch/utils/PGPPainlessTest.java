package li.strolch.utils;

import org.bouncycastle.openpgp.PGPPublicKeyRing;
import org.bouncycastle.openpgp.PGPSecretKeyRing;
import org.bouncycastle.util.io.Streams;
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

public class PGPPainlessTest {

	private static final Logger logger = LoggerFactory.getLogger(PGPPainlessTest.class);
	public static final String RECIPIENT_PUBLIC_KEY_FILE_NAME = "src/test/resources/eitch@eitchnet.ch.asc";
	public static final String SIGNING_KEY_FILE_NAME = "src/test/resources/strolch_example.key";
	public static final char[] CHAR_ARRAY = "example".toCharArray();

	@Test
	public void shouldSign() throws Exception {

		// input
		String plainText = "hello world";
		byte[] plainBytes = plainText.getBytes(StandardCharsets.UTF_8);
		ByteArrayInputStream inputStream = new ByteArrayInputStream(plainBytes);

		// get secret key (sender/signer)
		PGPSecretKeyRing signingKeyRing = PGPainless
				.readKeyRing()
				.secretKeyRing(new FileInputStream(SIGNING_KEY_FILE_NAME));
		if (signingKeyRing == null)
			throw new IllegalStateException("No secret key ring found for signing key file " + SIGNING_KEY_FILE_NAME);

		ByteArrayOutputStream signatureResult = new ByteArrayOutputStream();

		SecretKeyRingProtector secretKeyDecryptor = SecretKeyRingProtector.unlockAnyKeyWith(new Passphrase(CHAR_ARRAY));
		EncryptionStream encryptionStream = PGPainless
				.encryptAndOrSign()
				.onOutputStream(signatureResult)
				.withOptions(ProducerOptions
						.sign(new SigningOptions()
								.addDetachedSignature(secretKeyDecryptor, signingKeyRing)
								.overrideHashAlgorithm(HashAlgorithm.SHA256))
						.setCleartextSigned()
						.setVersion("")
						.setFileName("hello_world.txt"));

		Streams.pipeAll(inputStream, encryptionStream);
		encryptionStream.close();

		// Information about the encryption (algorithms, detached signatures etc.)
		EncryptionResult result = encryptionStream.getResult();
		logger.info("{}: encoding:{}:\n{}", result.getFileName(), result.getFileEncoding().name(), signatureResult);
	}

	@Test
	public void shouldEncryptAndSign() throws Exception {

		// input
		String plainText = "hello world";
		byte[] plainBytes = plainText.getBytes(StandardCharsets.UTF_8);
		ByteArrayInputStream inputStream = new ByteArrayInputStream(plainBytes);

		// get public key (recipient)
		PGPPublicKeyRing recipientKeyRing = PGPainless
				.readKeyRing()
				.publicKeyRing(new FileInputStream(RECIPIENT_PUBLIC_KEY_FILE_NAME));
		if (recipientKeyRing == null)
			throw new IllegalStateException(
					"No public key found for recipient key file " + RECIPIENT_PUBLIC_KEY_FILE_NAME);

		// get secret key (sender/signer)

		PGPSecretKeyRing signingKeyRing = PGPainless
				.readKeyRing()
				.secretKeyRing(new FileInputStream(SIGNING_KEY_FILE_NAME));
		if (signingKeyRing == null)
			throw new IllegalStateException("No secret key ring found for signing key file " + SIGNING_KEY_FILE_NAME);

		ByteArrayOutputStream signatureResult = new ByteArrayOutputStream();

		// If you use a single passphrase for all (sub-) keys, take this:
		SecretKeyRingProtector secretKeyDecryptor = SecretKeyRingProtector.unlockAnyKeyWith(new Passphrase(CHAR_ARRAY));
		EncryptionStream encryptionStream = PGPainless
				.encryptAndOrSign()
				.onOutputStream(signatureResult)
				.withOptions(ProducerOptions
						.signAndEncrypt(new EncryptionOptions().addRecipient(recipientKeyRing)
								// optionally override symmetric encryption algorithm
								.overrideEncryptionAlgorithm(SymmetricKeyAlgorithm.AES_256), new SigningOptions()
								// Sign in-line (using one-pass-signature packet)
								.addInlineSignature(secretKeyDecryptor, signingKeyRing,
										DocumentSignatureType.CANONICAL_TEXT_DOCUMENT)
								// optionally override hash algorithm
								.overrideHashAlgorithm(HashAlgorithm.SHA256))
						.setAsciiArmor(true)
						.setVersion("")
						.setFileName("hello_world.txt"));

		Streams.pipeAll(inputStream, encryptionStream);
		encryptionStream.close();

		// Information about the encryption (algorithms, detached signatures etc.)
		EncryptionResult result = encryptionStream.getResult();
		logger.info("{}: encoding:{}:\n{}", result.getFileName(), result.getFileEncoding().name(), signatureResult);
	}
}
