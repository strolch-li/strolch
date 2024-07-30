package li.strolch.utils.helper;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import java.io.*;
import java.nio.charset.StandardCharsets;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AesCryptoHelperTest {

	private static final Logger logger = LoggerFactory.getLogger(AesCryptoHelperTest.class);

	private static final char[] password = "A2589309-17AE-4819-B9E4-E577CFA7778F".toCharArray();
	private static final byte[] salt;

	static {
		salt = "E68761B3-4E8E-4122-9B12-8B89E0AEB233".getBytes(StandardCharsets.UTF_8);
	}

	@Test
	public void shouldWrapStreams() throws Exception {

		try {
			byte[] clearTextBytes = "Some text".getBytes();

			// encrypt data
			ByteArrayOutputStream encryptedOut = new ByteArrayOutputStream();
			try (OutputStream outputStream = AesCryptoHelper.wrapEncrypt(password, salt, encryptedOut)) {
				outputStream.write(clearTextBytes);
				outputStream.flush();
			}

			// decrypt data
			byte[] encryptedBytes = encryptedOut.toByteArray();
			ByteArrayInputStream encryptedIn = new ByteArrayInputStream(encryptedBytes);
			try (InputStream inputStream = AesCryptoHelper.wrapDecrypt(password, salt, encryptedIn)) {

				ByteArrayOutputStream decryptedOut = new ByteArrayOutputStream();
				byte[] readBuffer = new byte[64];
				int read = 0;
				while ((read = inputStream.read(readBuffer)) != -1) {
					decryptedOut.write(readBuffer, 0, read);
				}

				byte[] decryptedBytes = decryptedOut.toByteArray();
				assertArrayEquals(clearTextBytes, decryptedBytes);
			}

		} catch (RuntimeException e) {
			if (ExceptionHelper.getRootCause(e).getMessage().equals("Illegal key size or default parameters"))
				logger.warn("YOU ARE MISSING THE UNLIMITED JCE POLICIES and can not do AES encryption!");
			else
				throw e;
		}
	}

	@Test
	public void shouldEncryptBytes() {
		try {

			byte[] clearTextBytes = "Some text".getBytes();

			byte[] encryptedBytes = AesCryptoHelper.encrypt(password, salt, clearTextBytes);
			byte[] decryptedBytes = AesCryptoHelper.decrypt(password, salt, encryptedBytes);

			assertArrayEquals(clearTextBytes, decryptedBytes);

		} catch (RuntimeException e) {
			if (ExceptionHelper.getRootCause(e).getMessage().equals("Illegal key size or default parameters"))
				logger.warn("YOU ARE MISSING THE UNLIMITED JCE POLICIES and can not do AES encryption!");
			else
				throw e;
		}
	}

	@Test
	public void shouldEncryptShortFile() {
		// file to be encrypted
		String clearTextFileS = "src/test/resources/crypto_test_short.txt";
		// encrypted file
		String encryptedFileS = "target/encrypted_short.aes";
		// encrypted file
		String decryptedFileS = "target/decrypted_short.txt";

		testCrypto(clearTextFileS, encryptedFileS, decryptedFileS);
	}

	@Test
	public void shouldEncryptMiddleFile() {
		// file to be encrypted
		String clearTextFileS = "src/test/resources/crypto_test_middle.txt";
		// encrypted file
		String encryptedFileS = "target/encrypted_middle.aes";
		// encrypted file
		String decryptedFileS = "target/decrypted_middle.txt";

		testCrypto(clearTextFileS, encryptedFileS, decryptedFileS);
	}

	@Test
	public void shouldEncryptLongFile() {
		// file to be encrypted
		String clearTextFileS = "src/test/resources/crypto_test_long.txt";
		// encrypted file
		String encryptedFileS = "target/encrypted_long.aes";
		// encrypted file
		String decryptedFileS = "target/decrypted_long.txt";

		testCrypto(clearTextFileS, encryptedFileS, decryptedFileS);
	}

	@Test
	public void shouldEncryptBinaryFile() {

		// file to be encrypted
		String clearTextFileS = "src/test/resources/crypto_test_image.ico";
		// encrypted file
		String encryptedFileS = "target/encrypted_image.aes";
		// encrypted file
		String decryptedFileS = "target/decrypted_image.ico";

		testCrypto(clearTextFileS, encryptedFileS, decryptedFileS);

	}

	private static void testCrypto(String clearTextFileS, String encryptedFileS, String decryptedFileS) {
		try {

			AesCryptoHelper.encrypt(password, salt, clearTextFileS, encryptedFileS);
			AesCryptoHelper.decrypt(password, salt, encryptedFileS, decryptedFileS);

			String inputSha256 = StringHelper.toHexString(FileHelper.hashFileSha256(new File(clearTextFileS)));
			String doutputSha256 = StringHelper.toHexString(FileHelper.hashFileSha256(new File(decryptedFileS)));

			assertEquals(inputSha256, doutputSha256);

		} catch (RuntimeException e) {
			if (ExceptionHelper.getRootCause(e).getMessage().equals("Illegal key size or default parameters"))
				logger.warn("YOU ARE MISSING THE UNLIMITED JCE POLICIES and can not do AES encryption!");
			else
				throw e;
		}
	}
}
