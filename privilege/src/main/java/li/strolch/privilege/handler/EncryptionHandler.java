/*
 * Copyright 2013 Robert von Burg <eitch@eitchnet.ch>
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
package li.strolch.privilege.handler;

import li.strolch.privilege.model.internal.PasswordCrypt;

import java.util.Map;

/**
 * The {@link EncryptionHandler} exposes API which is used to handle encrypting of strings, or returning secure tokens
 * for certificates and so forth
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public interface EncryptionHandler {

	/**
	 * Returns the configured algorithm
	 *
	 * @return the configured algorithm
	 */
	String getAlgorithm();

	/**
	 * Returns the configured iterations
	 *
	 * @return the configured iterations
	 */
	int getIterations();

	/**
	 * Returns the configured key length
	 *
	 * @return the configured key length
	 */
	int getKeyLength();

	/**
	 * Generates a token which can be used to identify certificates and so forth
	 *
	 * @return a new token
	 */
	String nextToken();

	/**
	 * Generates a token which can be used to identify certificates and so forth
	 *
	 * @return a new token
	 */
	byte[] nextSalt();

	/**
	 * Hashes the given password configured algorithm
	 *
	 * @param password the password
	 *
	 * @return the {@link PasswordCrypt}
	 */
	PasswordCrypt hashPasswordWithoutSalt(final char[] password);

	/**
	 * Hashes the given password with the given salt with the configured algorithm
	 *
	 * @param password the password
	 * @param salt     the salt
	 *
	 * @return the {@link PasswordCrypt}
	 */
	PasswordCrypt hashPassword(final char[] password, final byte[] salt);

	/**
	 * Hashes the given password with the given salt and algorithm properties
	 *
	 * @param password   the password
	 * @param salt       the salt
	 * @param algorithm  the algorithm
	 * @param iterations the iterations
	 * @param keyLength  the keyLength
	 *
	 * @return the {@link PasswordCrypt}
	 */
	PasswordCrypt hashPassword(final char[] password, final byte[] salt, String algorithm, int iterations,
			int keyLength);

	boolean isPasswordCryptOutdated(PasswordCrypt userPasswordCrypt);

	/**
	 * Initialize the concrete {@link EncryptionHandler}. The passed parameter map contains any configuration the
	 * concrete {@link EncryptionHandler} might need
	 *
	 * @param parameterMap a map containing configuration properties
	 */
	void initialize(Map<String, String> parameterMap);

	/**
	 * Returns the configuration for this {@link EncryptionHandler}
	 *
	 * @return the configuration as a Map
	 */
	Map<String, String> getParameterMap();
}
