/*
 * Copyright (c) 2010 - 2012
 * 
 * This file is part of Privilege.
 *
 * Privilege is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Privilege is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Privilege.  If not, see <http://www.gnu.org/licenses/>.
 * 
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
 * 
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

	/**
	 * @see ch.eitchnet.privilege.handler.EncryptionHandler#convertToHash(java.lang.String)
	 */
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

	/**
	 * @see ch.eitchnet.privilege.handler.EncryptionHandler#convertToHash(java.lang.String)
	 */
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

	/**
	 * @see ch.eitchnet.privilege.handler.EncryptionHandler#nextToken()
	 */
	@Override
	public String nextToken() {
		byte[] bytes = new byte[16];
		this.secureRandom.nextBytes(bytes);
		String randomString = new String(bytes);
		//String randomString = new BigInteger(80, secureRandom).toString(32); // 80 big integer bits = 16 chars
		return randomString;
	}

	/**
	 * @see ch.eitchnet.privilege.handler.EncryptionHandler#initialize(java.util.Map)
	 */
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
