package li.strolch.utils.helper;

import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.crypto.*;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.SecureRandom;
import java.security.spec.KeySpec;
import java.util.Base64;

/**
 * Inspired by <a
 * href="https://github.com/tozny/java-aes-crypto/blob/master/aes-crypto/src/main/java/com/tozny/crypto/android/AesCbcWithIntegrity.java">tozny</a>
 */
public class AesCryptoHelper {

	private static final String CIPHER_TRANSFORMATION = "AES/CBC/PKCS5Padding";
	private static final String CIPHER = "AES";
	private static final int AES_KEY_LENGTH_BITS = 128;
	private static final int IV_LENGTH_BYTES = 16;
	private static final int PBE_ITERATION_COUNT = 10000;
	private static final String PBE_ALGORITHM = "PBKDF2WithHmacSHA1";

	private static final String HMAC_ALGORITHM = "HmacSHA256";
	private static final int HMAC_KEY_LENGTH_BITS = 256;

	private static final Logger logger = LoggerFactory.getLogger(AesCryptoHelper.class);

	public static OutputStream wrapEncrypt(char[] password, byte[] salt, OutputStream outputStream) {
		try {
			SecretKeys secret = buildSecret(password, salt);
			return wrapEncrypt(secret, outputStream);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static OutputStream wrapEncrypt(SecretKeys secretKeys, OutputStream outputStream) {
		try {
			// set up cipher
			byte[] iv = generateIv();
			Cipher cipher = Cipher.getInstance(CIPHER_TRANSFORMATION);
			cipher.init(Cipher.ENCRYPT_MODE, secretKeys.getConfidentialityKey(), new IvParameterSpec(iv));

			/*
			 * Now we get back the IV that will actually be used. Some runtimes
			 * versions do funny stuff w/ the IV, so this is to work around bugs:
			 */
			iv = cipher.getIV();
			DBC.INTERIM.assertEquals("IV must be 16 bytes long!", 16, iv.length);

			// write the initialization vector, but not through the cipher output stream!
			outputStream.write(iv);
			outputStream.flush();

			return new CipherOutputStream(outputStream, cipher);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static InputStream wrapDecrypt(char[] password, byte[] salt, InputStream inputStream) {
		try {
			SecretKeys secret = buildSecret(password, salt);
			return wrapDecrypt(secret, inputStream);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static InputStream wrapDecrypt(SecretKeys secretKeys, InputStream inputStream) {
		try {
			// read the initialization vector from the normal input stream
			byte[] initVector = new byte[16];
			if (inputStream.read(initVector) != 16)
				throw new IllegalStateException("Failed to read init vector!");

			Cipher cipher = Cipher.getInstance(CIPHER_TRANSFORMATION);
			cipher.init(Cipher.DECRYPT_MODE, secretKeys.getConfidentialityKey(), new IvParameterSpec(initVector));

			return new CipherInputStream(inputStream, cipher);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static void encrypt(char[] password, byte[] salt, String clearTextFileS, String encryptedFileS) {
		try (InputStream inFile = Files.newInputStream(Paths.get(clearTextFileS));
			 OutputStream outFile = Files.newOutputStream(Paths.get(encryptedFileS))) {
			encrypt(password, salt, inFile, outFile);
		} catch (Exception e) {
			throw new RuntimeException("Failed to encrypt file " + clearTextFileS + " to " + encryptedFileS, e);
		}

		logger.info("Encrypted file " + clearTextFileS + " to " + encryptedFileS);
	}

	public static void encrypt(char[] password, byte[] salt, InputStream inFile, OutputStream outFile) {
		try {
			SecretKeys secret = buildSecret(password, salt);
			encrypt(secret, inFile, outFile);
		} catch (Exception e) {
			throw new RuntimeException("Failed to encrypt input stream to output stream!", e);
		}
	}

	public static void encrypt(SecretKeys secretKeys, InputStream inFile, OutputStream outFile) {
		try {
			// set up cipher
			byte[] iv = generateIv();
			Cipher cipher = Cipher.getInstance(CIPHER_TRANSFORMATION);
			cipher.init(Cipher.ENCRYPT_MODE, secretKeys.getConfidentialityKey(), new IvParameterSpec(iv));

			/*
			 * Now we get back the IV that will actually be used. Some runtimes
			 * versions do funny stuff w/ the IV, so this is to work around bugs:
			 */
			iv = cipher.getIV();
			DBC.INTERIM.assertEquals("IV must be 16 bytes long!", 16, iv.length);

			outFile.write(iv);

			byte[] input = new byte[64];
			int bytesRead;

			while ((bytesRead = inFile.read(input)) != -1) {
				byte[] output = cipher.update(input, 0, bytesRead);
				if (output != null)
					outFile.write(output);
			}

			byte[] byteCipherText = cipher.doFinal();
			if (byteCipherText != null)
				outFile.write(byteCipherText);

			outFile.flush();
		} catch (Exception e) {
			throw new RuntimeException("Failed to encrypt input stream to output stream!", e);
		}
	}

	public static void decrypt(char[] password, byte[] salt, String encryptedFileS, String decryptedFileS) {
		try (InputStream fis = Files.newInputStream(Paths.get(encryptedFileS));
			 OutputStream fos = Files.newOutputStream(Paths.get(decryptedFileS))) {
			decrypt(password, salt, fis, fos);
		} catch (Exception e) {
			throw new RuntimeException("Failed to decrypt file " + decryptedFileS + " to " + decryptedFileS, e);
		}

		logger.info("Decrypted file " + encryptedFileS + " to " + decryptedFileS);

	}

	public static void decrypt(char[] password, byte[] salt, InputStream fis, OutputStream fos) {
		try {
			SecretKeys secret = buildSecret(password, salt);
			decrypt(secret, fis, fos);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static void decrypt(SecretKeys secretKeys, InputStream fis, OutputStream fos) {
		try {

			// read the initialization vector
			byte[] iv = new byte[16];
			if (fis.read(iv) == -1)
				throw new IllegalStateException("Failed to read init vector!");

			// init cipher
			Cipher cipher = Cipher.getInstance(CIPHER_TRANSFORMATION);
			cipher.init(Cipher.DECRYPT_MODE, secretKeys.getConfidentialityKey(), new IvParameterSpec(iv));

			byte[] in = new byte[64];
			int read;
			while ((read = fis.read(in)) != -1) {
				byte[] output = cipher.update(in, 0, read);
				if (output != null)
					fos.write(output);
			}

			byte[] output = cipher.doFinal();
			if (output != null)
				fos.write(output);

			fos.flush();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static byte[] encrypt(char[] password, byte[] salt, String clearText) {
		return encrypt(password, salt, clearText.getBytes());
	}

	public static byte[] encrypt(char[] password, byte[] salt, byte[] clearTextBytes) {

		try {

			// set up key
			SecretKeys secret = buildSecret(password, salt);

			return encrypt(secret, clearTextBytes);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static byte[] encrypt(SecretKeys secretKeys, byte[] clearTextBytes) {
		try {

			byte[] iv = generateIv();
			Cipher aesCipherForEncryption = Cipher.getInstance(CIPHER_TRANSFORMATION);
			aesCipherForEncryption.init(Cipher.ENCRYPT_MODE, secretKeys.getConfidentialityKey(),
					new IvParameterSpec(iv));

			// encrypt
			byte[] encryptedBytes = aesCipherForEncryption.doFinal(clearTextBytes);

			// create result bytes
			ByteBuffer byteBuffer = ByteBuffer.allocate(iv.length + encryptedBytes.length);
			byteBuffer.put(iv);
			byteBuffer.put(encryptedBytes);

			return byteBuffer.array();

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static byte[] decrypt(char[] password, byte[] salt, byte[] encryptedBytes) {
		try {
			SecretKeys secret = buildSecret(password, salt);
			return decrypt(secret, encryptedBytes);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static byte[] decrypt(SecretKeys secretKeys, byte[] encryptedBytes) {
		try {

			// read initialization vector
			byte[] initVector = new byte[16];
			System.arraycopy(encryptedBytes, 0, initVector, 0, 16);

			Cipher cipher = Cipher.getInstance(CIPHER_TRANSFORMATION);
			cipher.init(Cipher.DECRYPT_MODE, secretKeys.getConfidentialityKey(), new IvParameterSpec(initVector));

			return cipher.doFinal(encryptedBytes, 16, encryptedBytes.length - 16);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static SecretKeys buildSecret(char[] password, byte[] salt) {
		try {
			// Get enough random bytes for both the AES key and the HMAC key:
			KeySpec keySpec = new PBEKeySpec(password, salt, PBE_ITERATION_COUNT,
					AES_KEY_LENGTH_BITS + HMAC_KEY_LENGTH_BITS);
			SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(PBE_ALGORITHM);
			byte[] keyBytes = keyFactory.generateSecret(keySpec).getEncoded();

			// Split the random bytes into two parts:
			byte[] confidentialityKeyBytes = copyOfRange(keyBytes, 0, AES_KEY_LENGTH_BITS / 8);
			byte[] integrityKeyBytes = copyOfRange(keyBytes, AES_KEY_LENGTH_BITS / 8,
					AES_KEY_LENGTH_BITS / 8 + HMAC_KEY_LENGTH_BITS / 8);

			// Generate the AES key
			SecretKey confidentialityKey = new SecretKeySpec(confidentialityKeyBytes, CIPHER);

			// Generate the HMAC key
			SecretKey integrityKey = new SecretKeySpec(integrityKeyBytes, HMAC_ALGORITHM);

			return new SecretKeys(confidentialityKey, integrityKey);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Copy the elements from the start to the end
	 *
	 * @param from  the source
	 * @param start the start index to copy
	 * @param end   the end index to finish
	 *
	 * @return the new buffer
	 */
	private static byte[] copyOfRange(byte[] from, int start, int end) {
		int length = end - start;
		byte[] result = new byte[length];
		System.arraycopy(from, start, result, 0, length);
		return result;
	}

	/**
	 * Creates a random Initialization Vector (IV) of IV_LENGTH_BYTES.
	 *
	 * @return The byte array of this IV
	 */
	public static byte[] generateIv() {
		SecureRandom random = new SecureRandom();
		byte[] b = new byte[IV_LENGTH_BYTES];
		random.nextBytes(b);
		return b;
	}

	/**
	 * Holder class that has both the secret AES key for encryption (confidentiality) and the secret HMAC key for
	 * integrity.
	 */

	public static class SecretKeys {
		private SecretKey confidentialityKey;
		private SecretKey integrityKey;

		/**
		 * Construct the secret keys container.
		 *
		 * @param confidentialityKeyIn The AES key
		 * @param integrityKeyIn       the HMAC key
		 */
		public SecretKeys(SecretKey confidentialityKeyIn, SecretKey integrityKeyIn) {
			setConfidentialityKey(confidentialityKeyIn);
			setIntegrityKey(integrityKeyIn);
		}

		public SecretKey getConfidentialityKey() {
			return confidentialityKey;
		}

		public void setConfidentialityKey(SecretKey confidentialityKey) {
			this.confidentialityKey = confidentialityKey;
		}

		public SecretKey getIntegrityKey() {
			return integrityKey;
		}

		public void setIntegrityKey(SecretKey integrityKey) {
			this.integrityKey = integrityKey;
		}

		/**
		 * Encodes the two keys as a string
		 *
		 * @return base64(confidentialityKey):base64(integrityKey)
		 */
		@Override
		public String toString() {
			return Base64.getEncoder().encodeToString(getConfidentialityKey().getEncoded()) + ":" + Base64
					.getEncoder()
					.encodeToString(getIntegrityKey().getEncoded());
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + confidentialityKey.hashCode();
			result = prime * result + integrityKey.hashCode();
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			SecretKeys other = (SecretKeys) obj;
			if (!integrityKey.equals(other.integrityKey))
				return false;
			if (!confidentialityKey.equals(other.confidentialityKey))
				return false;
			return true;
		}
	}
}