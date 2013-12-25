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
import java.text.MessageFormat;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import ch.eitchnet.privilege.base.PrivilegeException;
import ch.eitchnet.privilege.helper.XmlConstants;
import ch.eitchnet.utils.helper.StringHelper;

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
		return convertToHash(string.getBytes());
	}

	@Override
	public String convertToHash(byte[] bytes) {
		try {

			return StringHelper.hashAsHex(this.hashAlgorithm, bytes);

		} catch (RuntimeException e) {
			if (e.getCause() == null)
				throw e;
			if (e.getCause().getClass().equals(NoSuchAlgorithmException.class))
				throw new PrivilegeException(
						MessageFormat.format("Algorithm {0} was not found!", this.hashAlgorithm), e.getCause()); //$NON-NLS-1$
			if (e.getCause().getClass().equals(UnsupportedEncodingException.class))
				throw new PrivilegeException("Charset ASCII is not supported!", e.getCause()); //$NON-NLS-1$

			throw e;
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
			String msg = "[{0}] Defined parameter {1} is invalid"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, EncryptionHandler.class.getName(), XmlConstants.XML_PARAM_HASH_ALGORITHM);
			throw new PrivilegeException(msg);
		}

		// test hash algorithm
		try {
			convertToHash("test"); //$NON-NLS-1$
			DefaultEncryptionHandler.logger.info(MessageFormat
					.format("Using hashing algorithm {0}", this.hashAlgorithm)); //$NON-NLS-1$
		} catch (Exception e) {
			String msg = "[{0}] Defined parameter {1} is invalid because of underlying exception: {2}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, EncryptionHandler.class.getName(), XmlConstants.XML_PARAM_HASH_ALGORITHM,
					e.getLocalizedMessage());
			throw new PrivilegeException(msg, e);
		}
	}
}
