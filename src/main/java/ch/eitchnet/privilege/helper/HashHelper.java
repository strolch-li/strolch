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
package ch.eitchnet.privilege.helper;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Helper class to hash a String for a certain hash algorithm, using the Java {@link MessageDigest} classes
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class HashHelper {

	/**
	 * Hex char table for fast calculating of hex values
	 */
	private static final byte[] HEX_CHAR_TABLE = { (byte) '0', (byte) '1', (byte) '2', (byte) '3', (byte) '4',
			(byte) '5', (byte) '6', (byte) '7', (byte) '8', (byte) '9', (byte) 'a', (byte) 'b', (byte) 'c', (byte) 'd',
			(byte) 'e', (byte) 'f' };

	/**
	 * Creates the hash of the given string using {@link MessageDigest} and the defined hash algorithm
	 * 
	 * @param hashAlgorithm
	 *            the algorithm to use for hashing
	 * @param string
	 *            the string to hash
	 * 
	 * @return a new string encrypted by the defined algorithm
	 * 
	 * @throws NoSuchAlgorithmException
	 *             if the algorithm is not found
	 * @throws UnsupportedEncodingException
	 *             if something is wrong with the given string to hash
	 */
	public static String stringToHash(String hashAlgorithm, String string) throws NoSuchAlgorithmException,
			UnsupportedEncodingException {
		return HashHelper.stringToHash(hashAlgorithm, string.getBytes());
	}

	/**
	 * Creates the hash of the given string using {@link MessageDigest} and the defined hash algorithm
	 * 
	 * @param hashAlgorithm
	 *            the algorithm to use for hashing
	 * @param bytes
	 *            the bytes to hash
	 * 
	 * @return a new string encrypted by the defined algorithm
	 * 
	 * @throws NoSuchAlgorithmException
	 *             if the algorithm is not found
	 * @throws UnsupportedEncodingException
	 *             if something is wrong with the given string to hash
	 */
	public static String stringToHash(String hashAlgorithm, byte[] bytes) throws NoSuchAlgorithmException,
			UnsupportedEncodingException {

		MessageDigest digest = MessageDigest.getInstance(hashAlgorithm);
		byte[] hashArray = digest.digest(bytes);

		byte[] hex = new byte[2 * hashArray.length];
		int index = 0;

		for (byte b : hashArray) {
			int v = b & 0xFF;
			hex[index++] = HashHelper.HEX_CHAR_TABLE[v >>> 4];
			hex[index++] = HashHelper.HEX_CHAR_TABLE[v & 0xF];
		}

		return new String(hex, "ASCII");
	}
}
