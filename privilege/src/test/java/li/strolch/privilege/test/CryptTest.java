package li.strolch.privilege.test;

import static li.strolch.privilege.base.PrivilegeConstants.*;
import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import li.strolch.privilege.handler.DefaultEncryptionHandler;
import li.strolch.privilege.helper.Crypt;
import li.strolch.privilege.helper.XmlConstants;
import org.junit.BeforeClass;
import org.junit.Test;

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
	public void shouldAssertSamePassword01() {

		String hash = "$1$21232f297a57a5a743894a0e4a801fc3";
		char[] password = "admin".toCharArray();

		Crypt crypt = new Crypt().parseCrypt(hash);
		crypt.assertSame(password);
		assertEquals(hash, crypt.toCryptString());
	}

	@Test
	public void shouldAssertSamePassword05() {

		String hash = "$5$8c6976e5b5410415bde908bd4dee15dfb167a9c873fc4bb8a81f6f2ab448a918";
		char[] password = "admin".toCharArray();

		Crypt crypt = new Crypt().parseCrypt(hash);
		crypt.assertSame(password);
		assertEquals(hash, crypt.toCryptString());
	}

	@Test
	public void shouldAssertSamePassword06() {

		String hash = "$6$c7ad44cbad762a5da0a452f9e854fdc1e0e7a52a38015f23f3eab1d80b931dd472634dfac71cd34ebc35d16ab7fb8a90c81f975113d6c7538dc69dd8de9077ec";
		char[] password = "admin".toCharArray();

		Crypt crypt = new Crypt().parseCrypt(hash);
		crypt.assertSame(password);
		assertEquals(hash, crypt.toCryptString());
	}

	@Test
	public void shouldAssertSamePassword15() {

		String hash = "$5$61646d696e$f4aec2c20dd0c3ff0547f4bd56facd76097cce7c613da80c67842b6a357fdc04";
		char[] password = "admin".toCharArray();

		Crypt crypt = new Crypt().parseCrypt(hash);
		crypt.assertSame(password);
		assertEquals(hash, crypt.toCryptString());
	}

	@Test
	public void shouldAssertSamePassword16() {

		String hash = "$6$rounds=5000$61646d696e$5a39ca7443147f9bf549ee0c2d5ded0640690ed56ef8c903e1b0da2a3339010b";
		char[] password = "admin".toCharArray();

		Crypt crypt = new Crypt().parseCrypt(hash);
		crypt.assertSame(password);
		assertEquals(hash, crypt.toCryptString());
	}

	@Test
	public void shouldAssertSamePassword20() {

		char[] password = "admin".toCharArray();

		byte[] salt = "admin".getBytes();
		byte[] passwordHash = encryptionHandler.hashPassword(password, salt);

		Crypt crypt = encryptionHandler.newCryptInstance();
		crypt.setSalt(salt);
		crypt.setPassword(passwordHash);

		String hash = "$6$61646d696e$cb69962946617da006a2f95776d78b49e5ec7941d2bdb2d25cdb05f957f64344";
		assertEquals(hash, crypt.toCryptString());
	}
}
