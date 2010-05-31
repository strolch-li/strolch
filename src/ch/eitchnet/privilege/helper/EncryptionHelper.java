/*
 * Copyright (c) 2010
 * 
 * Robert von Burg
 * eitch@eitchnet.ch
 * 
 * All rights reserved.
 * 
 */

package ch.eitchnet.privilege.helper;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * @author rvonburg
 * 
 */
public class EncryptionHelper {

	/**
	 * Hex char table for fast calculating of hex value
	 */
	private static final byte[] HEX_CHAR_TABLE = { (byte) '0', (byte) '1', (byte) '2', (byte) '3', (byte) '4',
			(byte) '5', (byte) '6', (byte) '7', (byte) '8', (byte) '9', (byte) 'a', (byte) 'b', (byte) 'c', (byte) 'd',
			(byte) 'e', (byte) 'f' };

	public static String encryptString(String hashAlgorithm, String string) throws NoSuchAlgorithmException,
			UnsupportedEncodingException {

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
	}
}
