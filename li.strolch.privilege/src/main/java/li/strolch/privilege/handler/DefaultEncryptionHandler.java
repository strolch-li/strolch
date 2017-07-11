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

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.text.MessageFormat;
import java.util.Map;

import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import li.strolch.privilege.base.PrivilegeException;
import li.strolch.privilege.helper.XmlConstants;
import li.strolch.utils.helper.StringHelper;

/**
 * <p>
 * This default {@link EncryptionHandler} creates tokens using a {@link SecureRandom} object. Hashing is done by using
 * {@link MessageDigest} and the configured algorithm which is passed in the parameters
 * </p>
 * 
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
	public byte[] hashPassword(char[] password, byte[] salt) {

		try {
			SecretKeyFactory skf = SecretKeyFactory.getInstance(this.algorithm);
			PBEKeySpec spec = new PBEKeySpec(password, salt, this.iterations, this.keyLength);
			SecretKey key = skf.generateSecret(spec);
			byte[] res = key.getEncoded();
			return res;

		} catch (NoSuchAlgorithmException | InvalidKeySpecException e) {
			throw new IllegalStateException(e);
		}
	}

	@Override
	public void initialize(Map<String, String> parameterMap) {

		this.secureRandom = new SecureRandom();

		// get hash algorithm parameters
		this.algorithm = parameterMap.getOrDefault(XmlConstants.XML_PARAM_HASH_ALGORITHM, "PBKDF2WithHmacSHA512");
		this.iterations = Integer.parseInt(parameterMap.getOrDefault(XmlConstants.XML_PARAM_HASH_ITERATIONS, "200000"));
		this.keyLength = Integer.parseInt(parameterMap.getOrDefault(XmlConstants.XML_PARAM_HASH_KEY_LENGTH, "256"));

		// test hash algorithm
		try {
			hashPassword("test".toCharArray(), "test".getBytes()); //$NON-NLS-1$
			DefaultEncryptionHandler.logger.info(MessageFormat.format("Using hashing algorithm {0}", this.algorithm)); //$NON-NLS-1$
		} catch (Exception e) {
			String msg = "[{0}] Defined parameter {1} is invalid because of underlying exception: {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, EncryptionHandler.class.getName(), XmlConstants.XML_PARAM_HASH_ALGORITHM,
					e.getLocalizedMessage());
			throw new PrivilegeException(msg, e);
		}
	}
}
