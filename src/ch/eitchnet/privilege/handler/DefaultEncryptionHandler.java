/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.handler;

import java.io.UnsupportedEncodingException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Map;

import org.apache.log4j.Logger;
import org.dom4j.Element;

import ch.eitchnet.privilege.base.XmlConstants;
import ch.eitchnet.privilege.helper.ConfigurationHelper;
import ch.eitchnet.privilege.helper.EncryptionHelper;
import ch.eitchnet.privilege.i18n.PrivilegeException;

/**
 * @author rvonburg
 * 
 */
public class DefaultEncryptionHandler implements EncryptionHandler {
	private static final Logger logger = Logger.getLogger(DefaultEncryptionHandler.class);

	private SecureRandom secureRandom;
	private String hashAlgorithm;

	/**
	 * @see ch.eitchnet.privilege.handler.EncryptionHandler#convertToHash(java.lang.String)
	 */
	@Override
	public String convertToHash(String string) {
		try {

			return EncryptionHelper.encryptString(hashAlgorithm, string);

		} catch (NoSuchAlgorithmException e) {
			throw new PrivilegeException("Algorithm " + hashAlgorithm + " was not found!", e);
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
		secureRandom.nextBytes(bytes);
		String randomString = new String(bytes);
		//String randomString = new BigInteger(80, secureRandom).toString(32); // 80 big integer bits = 16 chars
		return randomString;
	}

	/**
	 * @see ch.eitchnet.privilege.base.PrivilegeContainerObject#initialize(org.dom4j.Element)
	 */
	public void initialize(Element element) {

		secureRandom = new SecureRandom();

		// get parameters
		Element parameterElement = element.element(XmlConstants.XML_PARAMETERS);
		Map<String, String> parameterMap = ConfigurationHelper.convertToParameterMap(parameterElement);

		// get hash algorithm parameters
		hashAlgorithm = parameterMap.get(XmlConstants.XML_PARAM_HASH_ALGORITHM);
		if (hashAlgorithm == null || hashAlgorithm.isEmpty()) {
			throw new PrivilegeException("[" + EncryptionHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_HASH_ALGORITHM + " is invalid");
		}

		// test hash algorithm
		try {
			convertToHash("test");
			logger.info("Using hashing algorithm " + hashAlgorithm);
		} catch (Exception e) {
			throw new PrivilegeException("[" + EncryptionHandler.class.getName() + "] Defined parameter "
					+ XmlConstants.XML_PARAM_HASH_ALGORITHM + " is invalid because of underlying exception: "
					+ e.getLocalizedMessage(), e);
		}
	}
}
