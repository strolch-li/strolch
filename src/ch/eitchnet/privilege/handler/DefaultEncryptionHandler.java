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
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Map;

import org.dom4j.Element;

import ch.eitchnet.privilege.helper.ConfigurationHelper;
import ch.eitchnet.privilege.i18n.PrivilegeException;

/**
 * @author rvonburg
 * 
 */
public class DefaultEncryptionHandler implements EncryptionHandler {

	public static final String XML_PARAM_HASH_ALGORITHM = "hashAlgorithm";

	public String hashAlgorithm;

	/**
	 * Hex char table for fast calculating of hex value
	 */
	private static final byte[] HEX_CHAR_TABLE = { (byte) '0', (byte) '1', (byte) '2', (byte) '3', (byte) '4',
			(byte) '5', (byte) '6', (byte) '7', (byte) '8', (byte) '9', (byte) 'a', (byte) 'b', (byte) 'c', (byte) 'd',
			(byte) 'e', (byte) 'f' };

	/**
	 * @see ch.eitchnet.privilege.handler.EncryptionHandler#convertToHash(java.lang.String)
	 */
	@Override
	public String convertToHash(String string) {
		try {

			MessageDigest digest = MessageDigest.getInstance(hashAlgorithm);
			byte[] hashArray = digest.digest(string.getBytes());

			byte[] hex = new byte[2 * hashArray.length];
			int index = 0;

			for (byte b : hashArray) {
				int v = b & 0xFF;
				hex[index++] = HEX_CHAR_TABLE[v >>> 4];
				hex[index++] = HEX_CHAR_TABLE[v & 0xF];
			}

			return new String(hex, "ASCII");

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
		SecureRandom secureRandom = new SecureRandom();
		String randomString = new BigInteger(130, secureRandom).toString(32);
		return randomString;
	}

	/**
	 * @see ch.eitchnet.privilege.base.PrivilegeContainerObject#initialize(org.dom4j.Element)
	 */
	public void initialize(Element element) {

		Element parameterElement = element.element("Parameters");
		Map<String, String> parameterMap = ConfigurationHelper.convertToParameterMap(parameterElement);

		// get and test configured algorithm
		hashAlgorithm = parameterMap.get(XML_PARAM_HASH_ALGORITHM);
		convertToHash("test");
	}
}
