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

package li.strolch.privilege.test;

import li.strolch.privilege.handler.DefaultEncryptionHandler;
import li.strolch.privilege.model.internal.PasswordCrypt;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static li.strolch.privilege.base.PrivilegeConstants.*;
import static li.strolch.privilege.helper.XmlConstants.*;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNotNull;

public class CryptTest {

	private static DefaultEncryptionHandler encryptionHandler;

	@BeforeClass
	public static void beforeClass() {
		Map<String, String> parameterMap = new HashMap<>();
		parameterMap.put(PARAM_HASH_ALGORITHM, DEFAULT_ALGORITHM);
		parameterMap.put(PARAM_HASH_ITERATIONS, "" + DEFAULT_SMALL_ITERATIONS);
		parameterMap.put(PARAM_HASH_KEY_LENGTH, "" + DEFAULT_KEY_LENGTH);

		encryptionHandler = new DefaultEncryptionHandler();
		encryptionHandler.initialize(parameterMap);
	}

	@Test
	public void shouldAssertSamePassword1() {

		String passwordCryptString
				= "$PBKDF2WithHmacSHA512,100000,256$61646d696e$074aa490729dd8008282479f36ce4620b17d90ffb57088e6de32c891bc3bab07";
		PasswordCrypt parsedCryptHash = PasswordCrypt.parse(passwordCryptString, null);
		assertNotNull(parsedCryptHash);

		char[] password = "admin".toCharArray();
		byte[] salt = "admin".getBytes();
		PasswordCrypt passwordCrypt = encryptionHandler.hashPassword(password, salt, "PBKDF2WithHmacSHA512", 100000,
				256);

		assertArrayEquals(passwordCrypt.password(), parsedCryptHash.password());
	}

	@Test
	public void shouldAssertSamePassword2() {

		String passwordCryptString
				= "$PBKDF2WithHmacSHA512,100000,256$b633eb666211f346ed2c512693dc5365dd02acf660ab433aa9d023282a1f7362$1fbeb451878838b58778da968ae94937f2bc650b0b8118a4db8db1ba7ea0b72f";
		PasswordCrypt parsedCryptHash = PasswordCrypt.parse(passwordCryptString, null);
		assertNotNull(parsedCryptHash);

		char[] password = "admin".toCharArray();
		PasswordCrypt passwordCrypt = encryptionHandler.hashPassword(password, parsedCryptHash.salt(),
				"PBKDF2WithHmacSHA512", 100000, 256);

		assertArrayEquals(passwordCrypt.password(), parsedCryptHash.password());
	}
}
