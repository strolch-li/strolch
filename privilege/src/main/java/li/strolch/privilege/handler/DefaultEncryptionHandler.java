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

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.helper.XmlConstants;
import li.strolch.privilege.model.internal.PasswordCrypt;
import li.strolch.utils.helper.StringHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.text.MessageFormat;
import java.util.Map;

import static java.lang.String.valueOf;
import static li.strolch.privilege.base.PrivilegeConstants.*;
import static li.strolch.privilege.helper.XmlConstants.*;

/**
 * <p>
 * This default {@link EncryptionHandler} creates tokens using a {@link SecureRandom} object. Hashing is done by using
 * {@link MessageDigest} and the configured algorithm which is passed in the parameters
 * </p>
 * <p>
 * Required parameters:
 * <ul>
 * <li>{@link XmlConstants#XML_PARAM_HASH_ALGORITHM}</li>
 * </ul>
 *
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class DefaultEncryptionHandler implements EncryptionHandler {

	/**
	 * The log4j logger used in this instance
	 */
	private static final Logger logger = LoggerFactory.getLogger(DefaultEncryptionHandler.class);

	/**
	 * The {@link SecureRandom} which is used to create new tokens
	 */
	private SecureRandom secureRandom;

	/**
	 * The non-salt algorithm for this instance
	 */
	private String nonSaltAlgorithm;

	/**
	 * The configured algorithm for this instance
	 */
	private String algorithm;

	/**
	 * The number of iterations to perform the hashing with
	 */
	private int iterations;

	/**
	 * The length of the secure key for the hashing
	 */
	private int keyLength;
	private Map<String, String> parameterMap;

	@Override
	public Map<String, String> getParameterMap() {
		return this.parameterMap;
	}

	@Override
	public String getAlgorithm() {
		return this.algorithm;
	}

	@Override
	public int getIterations() {
		return this.iterations;
	}

	@Override
	public int getKeyLength() {
		return this.keyLength;
	}

	@Override
	public String nextToken() {
		byte[] bytes = new byte[32];
		this.secureRandom.nextBytes(bytes);
		return StringHelper.toHexString(bytes);
	}

	@Override
	public byte[] nextSalt() {
		byte[] bytes = new byte[32];
		this.secureRandom.nextBytes(bytes);
		return bytes;
	}

	@Override
	public PasswordCrypt hashPasswordWithoutSalt(char[] password) {
		try {

			MessageDigest digest = MessageDigest.getInstance(this.nonSaltAlgorithm);
			return new PasswordCrypt(digest.digest(new String(password).getBytes()), null);

		} catch (NoSuchAlgorithmException e) {
			throw new PrivilegeException(MessageFormat.format("Algorithm {0} was not found!", nonSaltAlgorithm),
					e.getCause());
		}
	}

	@Override
	public PasswordCrypt hashPassword(char[] password, byte[] salt) {
		return hashPassword(password, salt, this.algorithm, this.iterations, this.keyLength);
	}

	@Override
	public PasswordCrypt hashPassword(char[] password, byte[] salt, String algorithm, int iterations, int keyLength) {

		try {
			SecretKeyFactory skf = SecretKeyFactory.getInstance(algorithm);
			PBEKeySpec spec = new PBEKeySpec(password, salt, iterations, keyLength);
			SecretKey key = skf.generateSecret(spec);

			return new PasswordCrypt(key.getEncoded(), salt, algorithm, iterations, keyLength);

		} catch (NoSuchAlgorithmException | InvalidKeySpecException e) {
			throw new IllegalStateException(e);
		}
	}

	@Override
	public boolean isPasswordCryptOutdated(PasswordCrypt passwordCrypt) {
		return passwordCrypt.getSalt() == null || passwordCrypt.getHashAlgorithm() == null ||
				passwordCrypt.getHashIterations() != this.iterations ||
				passwordCrypt.getHashKeyLength() != this.keyLength;
	}

	@Override
	public void initialize(Map<String, String> parameterMap) {
		this.parameterMap = parameterMap;

		this.secureRandom = new SecureRandom();

		// get hash algorithm parameters
		this.algorithm = parameterMap.getOrDefault(XML_PARAM_HASH_ALGORITHM, DEFAULT_ALGORITHM);
		this.nonSaltAlgorithm = parameterMap.getOrDefault(XML_PARAM_HASH_ALGORITHM_NON_SALT,
				DEFAULT_ALGORITHM_NON_SALT);
		this.iterations = Integer.parseInt(
				parameterMap.getOrDefault(XML_PARAM_HASH_ITERATIONS, valueOf(DEFAULT_ITERATIONS)));
		this.keyLength = Integer.parseInt(
				parameterMap.getOrDefault(XML_PARAM_HASH_KEY_LENGTH, valueOf(DEFAULT_KEY_LENGTH)));

		// test non-salt hash algorithm
		try {
			hashPasswordWithoutSalt("test".toCharArray());
			DefaultEncryptionHandler.logger.info(
					MessageFormat.format("Using non-salt hashing algorithm {0}", this.nonSaltAlgorithm));
		} catch (Exception e) {
			String msg = "[{0}] Defined parameter {1} is invalid because of underlying exception: {2}";
			msg = MessageFormat.format(msg, EncryptionHandler.class.getName(), XML_PARAM_HASH_ALGORITHM_NON_SALT,
					e.getLocalizedMessage());
			throw new PrivilegeException(msg, e);
		}

		// test hash algorithm
		try {
			hashPassword("test".toCharArray(), "test".getBytes());
			DefaultEncryptionHandler.logger.info(MessageFormat.format("Using hashing algorithm {0}", this.algorithm));
		} catch (Exception e) {
			String msg = "[{0}] Defined parameter {1} is invalid because of underlying exception: {2}";
			msg = MessageFormat.format(msg, EncryptionHandler.class.getName(), XML_PARAM_HASH_ALGORITHM,
					e.getLocalizedMessage());
			throw new PrivilegeException(msg, e);
		}
	}
}
