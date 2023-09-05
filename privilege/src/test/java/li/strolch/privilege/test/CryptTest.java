package li.strolch.privilege.test;

import li.strolch.privilege.handler.DefaultEncryptionHandler;
import li.strolch.privilege.helper.XmlConstants;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static li.strolch.privilege.base.PrivilegeConstants.*;

public class CryptTest {

	private static DefaultEncryptionHandler encryptionHandler;

	@BeforeClass
	public static void beforeClass() {
		Map<String, String> parameterMap = new HashMap<>();
		parameterMap.put(XmlConstants.XML_PARAM_HASH_ALGORITHM, DEFAULT_ALGORITHM);
		parameterMap.put(XmlConstants.XML_PARAM_HASH_ITERATIONS, "" + DEFAULT_SMALL_ITERATIONS);
		parameterMap.put(XmlConstants.XML_PARAM_HASH_KEY_LENGTH, "" + DEFAULT_KEY_LENGTH);

		encryptionHandler = new DefaultEncryptionHandler();
		encryptionHandler.initialize(parameterMap);
	}

	@Test
	public void shouldAssertSamePassword20() {

		//		char[] password = "admin".toCharArray();
		//
		//		byte[] salt = "admin".getBytes();
		//		byte[] passwordHash = encryptionHandler.hashPassword(password, salt);
		//
		//		Crypt crypt = encryptionHandler.newCryptInstance();
		//		crypt.setSalt(salt);
		//		crypt.setPassword(passwordHash);
		//
		//
		//		encryptionHandler.
		//		String hash = "$PBKDF2WithHmacSHA512,100000,256$943f2d9208079322e50297f018c44d77d4e887e07bc9b37b2b80d121ad7dbd6e$8bcd819c99e79975e93e5f8bd87a376737afacd4427d7b33f0f69d0fc8030da5";
		//
		//		requestPasswordCrypt = encryptionHandler.hashPassword(password, userPasswordCrypt.getSalt(),
		//				userPasswordCrypt.getHashAlgorithm(), userPasswordCrypt.getHashIterations(),
		//				userPasswordCrypt.getHashKeyLength());
		//
		//		// validate password
		//		if (!Arrays.equals(requestPasswordCrypt.getPassword(), userPasswordCrypt.getPassword()))
		//			throw new InvalidCredentialsException(format("Password is incorrect for {0}", username));
	}
}
