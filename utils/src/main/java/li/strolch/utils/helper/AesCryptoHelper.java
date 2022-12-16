package li.strolch.utils.helper;

import javax.crypto.*;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.AlgorithmParameters;
import java.security.spec.KeySpec;

import li.strolch.utils.dbc.DBC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AesCryptoHelper {

	private static final String CIPHER = "AES/CBC/PKCS5Padding";

	private static final Logger logger = LoggerFactory.getLogger(AesCryptoHelper.class);

	public static OutputStream wrapEncrypt(char[] password, byte[] salt, OutputStream outputStream) {

		try {

			// set up key
			SecretKey secret = buildSecret(password, salt);

			return wrapEncrypt(secret, outputStream);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static OutputStream wrapEncrypt(SecretKey secret, OutputStream outputStream) {

		try {

			// set up cipher
			Cipher cipher = Cipher.getInstance(CIPHER);
			cipher.init(Cipher.ENCRYPT_MODE, secret);

			// set up the initialization vector
			AlgorithmParameters params = cipher.getParameters();
			byte[] initVector = params.getParameterSpec(IvParameterSpec.class).getIV();
			DBC.INTERIM.assertEquals("IV must be 16 bytes long!", 16, initVector.length);

			// write the initialization vector, but not through the cipher output stream!
			outputStream.write(initVector);
			outputStream.flush();

			CipherOutputStream cipherOutputStream = new CipherOutputStream(outputStream, cipher);
			return cipherOutputStream;

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static InputStream wrapDecrypt(char[] password, byte[] salt, InputStream inputStream) {

		try {

			// set up key
			SecretKey secret = buildSecret(password, salt);

			return wrapDecrypt(secret, inputStream);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static InputStream wrapDecrypt(SecretKey secret, InputStream inputStream) {

		try {

			// read the initialization vector from the normal input stream
			byte[] initVector = new byte[16];
			inputStream.read(initVector);

			// init cipher
			Cipher cipher = Cipher.getInstance(CIPHER);
			cipher.init(Cipher.DECRYPT_MODE, secret, new IvParameterSpec(initVector));

			CipherInputStream cipherInputStream = new CipherInputStream(inputStream, cipher);
			return cipherInputStream;

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

	public static void encrypt(SecretKey secret, String clearTextFileS, String encryptedFileS) {

		try (InputStream inFile = Files.newInputStream(Paths.get(clearTextFileS));
				OutputStream outFile = Files.newOutputStream(Paths.get(encryptedFileS))) {

			encrypt(secret, inFile, outFile);

		} catch (Exception e) {
			throw new RuntimeException("Failed to encrypt file " + clearTextFileS + " to " + encryptedFileS, e);
		}

		logger.info("Encrypted file " + clearTextFileS + " to " + encryptedFileS);
	}

	public static void encrypt(char[] password, byte[] salt, InputStream inFile, OutputStream outFile) {

		try {

			// set up key
			SecretKey secret = buildSecret(password, salt);

			encrypt(secret, inFile, outFile);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static void encrypt(SecretKey secret, InputStream inFile, OutputStream outFile) {

		try {

			// set up cipher
			Cipher cipher = Cipher.getInstance(CIPHER);
			cipher.init(Cipher.ENCRYPT_MODE, secret);

			// set up the initialization vector
			AlgorithmParameters params = cipher.getParameters();
			byte[] initVector = params.getParameterSpec(IvParameterSpec.class).getIV();
			DBC.INTERIM.assertEquals("IV must be 16 bytes long!", 16, initVector.length);
			outFile.write(initVector);

			byte[] input = new byte[64];
			int bytesRead;

			while ((bytesRead = inFile.read(input)) != -1) {
				byte[] output = cipher.update(input, 0, bytesRead);
				if (output != null)
					outFile.write(output);
			}

			byte[] output = cipher.doFinal();
			if (output != null)
				outFile.write(output);

			outFile.flush();

		} catch (Exception e) {
			throw new RuntimeException(e);
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

	public static void decrypt(SecretKey secret, String encryptedFileS, String decryptedFileS) {

		try (InputStream fis = Files.newInputStream(Paths.get(encryptedFileS));
				OutputStream fos = Files.newOutputStream(Paths.get(decryptedFileS))) {

			decrypt(secret, fis, fos);

		} catch (Exception e) {
			throw new RuntimeException("Failed to decrypt file " + decryptedFileS + " to " + decryptedFileS, e);
		}

		logger.info("Decrypted file " + encryptedFileS + " to " + decryptedFileS);

	}

	public static void decrypt(char[] password, byte[] salt, InputStream fis, OutputStream fos) {

		try {

			// set up key
			SecretKey secret = buildSecret(password, salt);

			decrypt(secret, fis, fos);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static void decrypt(SecretKey secret, InputStream fis, OutputStream fos) {

		try {

			// read the initialization vector
			byte[] initVector = new byte[16];
			fis.read(initVector);

			// init cipher
			Cipher cipher = Cipher.getInstance(CIPHER);
			cipher.init(Cipher.DECRYPT_MODE, secret, new IvParameterSpec(initVector));

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

	public static byte[] encrypt(SecretKey secret, byte[] salt, String clearText) {
		return encrypt(secret, clearText.getBytes());
	}

	public static byte[] encrypt(char[] password, byte[] salt, byte[] clearTextBytes) {

		try {

			// set up key
			SecretKey secret = buildSecret(password, salt);

			return encrypt(secret, clearTextBytes);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static byte[] encrypt(SecretKey secret, byte[] clearTextBytes) {

		try {

			// set up cipher
			Cipher cipher = Cipher.getInstance(CIPHER);
			cipher.init(Cipher.ENCRYPT_MODE, secret);

			// set up the initialization vector
			AlgorithmParameters params = cipher.getParameters();
			byte[] initVector = params.getParameterSpec(IvParameterSpec.class).getIV();
			DBC.INTERIM.assertEquals("IV must be 16 bytes long!", 16, initVector.length);

			// encrypt
			byte[] encryptedBytes = cipher.doFinal(clearTextBytes);

			// create result bytes
			ByteBuffer byteBuffer = ByteBuffer.allocate(initVector.length + encryptedBytes.length);
			byteBuffer.put(initVector);
			byteBuffer.put(encryptedBytes);

			return byteBuffer.array();

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static byte[] decrypt(char[] password, byte[] salt, byte[] encryptedBytes) {

		try {

			// set up key
			SecretKey secret = buildSecret(password, salt);

			return decrypt(secret, encryptedBytes);

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static byte[] decrypt(SecretKey secret, byte[] encryptedBytes) {

		try {

			// read initialization vector
			byte[] initVector = new byte[16];
			System.arraycopy(encryptedBytes, 0, initVector, 0, 16);

			// init cipher
			Cipher cipher = Cipher.getInstance(CIPHER);
			cipher.init(Cipher.DECRYPT_MODE, secret, new IvParameterSpec(initVector));

			byte[] decryptedBytes = cipher.doFinal(encryptedBytes, 16, encryptedBytes.length - 16);
			return decryptedBytes;

		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static SecretKey buildSecret(char[] password, byte[] salt) {
		try {
			SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1");
			KeySpec keySpec = new PBEKeySpec(password, salt, 65536, 256);
			SecretKey secretKey = factory.generateSecret(keySpec);
			SecretKey secret = new SecretKeySpec(secretKey.getEncoded(), "AES");

			return secret;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}