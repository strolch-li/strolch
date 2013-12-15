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
package ch.eitchnet.privilege.handler;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.helper.HashHelper;
import ch.eitchnet.privilege.helper.XmlConstants;

/**
 * <p>
 * This default {@link EncryptionHandler} creates tokens using a {@link SecureRandom} object. Hashing is done by using
 * {@link MessageDigest} and the configured algorithm which is passed in the parameters
 * </p>
 * 
 * Required parameters:
 * <ul>
 * <li> {@link XmlConstants#XML_PARAM_HASH_ALGORITHM}</li>
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
	 * The configured hash algorithm for this instance
	 */
	private String hashAlgorithm;

	@Override
	public String convertToHash(String string) {
		try {

			return HashHelper.stringToHash(this.hashAlgorithm, string);

		} catch (NoSuchAlgorithmException e) {
			throw new PrivilegeException("Algorithm " + this.hashAlgorithm + " was not found!", e);
		} catch (UnsupportedEncodingException e) {
			throw new PrivilegeException("Charset ASCII is not supported!", e);
		}
	}

	@Override
	public String convertToHash(byte[] bytes) {
		try {

			return HashHelper.stringToHash(this.hashAlgorithm, bytes);

		} catch (NoSuchAlgorithmException e) {
			throw new PrivilegeException("Algorithm " + this.hashAlgorithm + " was not found!", e);
		} catch (UnsupportedEncodingException e) {
			throw new PrivilegeException("Charset ASCII is not supported!", e);
		}
	}

	@Override
	public String nextToken() {
		byte[] bytes = new byte[16];
		this.secureRandom.nextBytes(bytes);
		String randomString = new String(bytes);
		return randomString;
	}

	@Override
	public void initialize(Map<String, String> parameterMap) {

		this.secureRandom = new SecureRandom();

		// get hash algorithm parameters
		this.hashAlgorithm = parameterMap.get(XmlConstants.XML_PARAM_HASH_ALGORITHM);
		if (this.hashAlgorithm == null || this.hashAlgorithm.isEmpty()) {
			throw new PrivilegeException("[" + EncryptionHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_HASH_ALGORITHM + " is invalid");
		}

		// test hash algorithm
		try {
			convertToHash("test");
			DefaultEncryptionHandler.logger.info("Using hashing algorithm " + this.hashAlgorithm);
		} catch (Exception e) {
			throw new PrivilegeException("[" + EncryptionHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_HASH_ALGORITHM + " is invalid because of underlying exception: "
					+ e.getLocalizedMessage(), e);
		}
	}
}
