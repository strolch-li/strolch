/*
 * Copyright (c) 2012
 * 
 * This file is part of ch.eitchnet.java.utils
 *
 * ch.eitchnet.java.utils is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ch.eitchnet.java.utils is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ch.eitchnet.java.utils.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package ch.eitchnet.utils.helper;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A helper class to perform different actions on {@link String}s
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class StringHelper {

	public static final String NEW_LINE = "\n"; //$NON-NLS-1$
	public static final String EMPTY = ""; //$NON-NLS-1$
	public static final String NULL = "null"; //$NON-NLS-1$

	private static final Logger logger = LoggerFactory.getLogger(StringHelper.class);

	/**
	 * Hex char table for fast calculating of hex value
	 */
	private static final byte[] HEX_CHAR_TABLE = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C',
			'D', 'E', 'F' };

	/**
	 * Converts each byte of the given byte array to a HEX value and returns the concatenation of these values
	 * 
	 * @param raw
	 *            the bytes to convert to String using numbers in hexadecimal
	 * 
	 * @return the encoded string
	 * 
	 * @throws RuntimeException
	 */
	public static String getHexString(byte[] raw) throws RuntimeException {
		try {
			byte[] hex = new byte[2 * raw.length];
			int index = 0;

			for (byte b : raw) {
				int v = b & 0xFF;
				hex[index++] = StringHelper.HEX_CHAR_TABLE[v >>> 4];
				hex[index++] = StringHelper.HEX_CHAR_TABLE[v & 0xF];
			}

			return new String(hex, "ASCII");

		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException("Something went wrong while converting to HEX: " + e.getLocalizedMessage(), e);
		}
	}

	/**
	 * Returns a byte array of a given string by converting each character of the string to a number base 16
	 * 
	 * @param encoded
	 *            the string to convert to a byt string
	 * 
	 * @return the encoded byte stream
	 */
	public static byte[] fromHexString(String encoded) {
		if ((encoded.length() % 2) != 0)
			throw new IllegalArgumentException("Input string must contain an even number of characters.");

		final byte result[] = new byte[encoded.length() / 2];
		final char enc[] = encoded.toCharArray();
		for (int i = 0; i < enc.length; i += 2) {
			StringBuilder curr = new StringBuilder(2);
			curr.append(enc[i]).append(enc[i + 1]);
			result[i / 2] = (byte) Integer.parseInt(curr.toString(), 16);
		}

		return result;
	}

	/**
	 * Generates the MD5 Hash of a string and converts it to a HEX string
	 * 
	 * @param string
	 *            the string to hash
	 * 
	 * @return the hash or null, if an exception was thrown
	 */
	public static String hashMd5AsHex(String string) {
		return getHexString(StringHelper.hashMd5(string.getBytes()));
	}

	/**
	 * Generates the MD5 Hash of a string. Use {@link StringHelper#getHexString(byte[])} to convert the byte array to a
	 * Hex String which is printable
	 * 
	 * @param string
	 *            the string to hash
	 * 
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hashMd5(String string) {
		return StringHelper.hashMd5(string.getBytes());
	}

	/**
	 * Generates the MD5 Hash of a byte array Use {@link StringHelper#getHexString(byte[])} to convert the byte array to
	 * a Hex String which is printable
	 * 
	 * @param bytes
	 *            the bytes to hash
	 * 
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hashMd5(byte[] bytes) {
		return StringHelper.hash("MD5", bytes);
	}

	/**
	 * Generates the SHA1 Hash of a string and converts it to a HEX String
	 * 
	 * @param string
	 *            the string to hash
	 * 
	 * @return the hash or null, if an exception was thrown
	 */
	public static String hashSha1AsHex(String string) {
		return getHexString(StringHelper.hashSha1(string.getBytes()));
	}

	/**
	 * Generates the SHA1 Hash of a string Use {@link StringHelper#getHexString(byte[])} to convert the byte array to a
	 * Hex String which is printable
	 * 
	 * @param string
	 *            the string to hash
	 * 
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hashSha1(String string) {
		return StringHelper.hashSha1(string.getBytes());
	}

	/**
	 * Generates the SHA1 Hash of a byte array Use {@link StringHelper#getHexString(byte[])} to convert the byte array
	 * to a Hex String which is printable
	 * 
	 * @param bytes
	 *            the bytes to hash
	 * 
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hashSha1(byte[] bytes) {
		return StringHelper.hash("SHA-1", bytes);
	}

	/**
	 * Generates the SHA-256 Hash of a string and converts it to a HEX String
	 * 
	 * @param string
	 *            the string to hash
	 * 
	 * @return the hash or null, if an exception was thrown
	 */
	public static String hashSha256AsHex(String string) {
		return getHexString(StringHelper.hashSha256(string.getBytes()));
	}

	/**
	 * Generates the SHA-256 Hash of a string Use {@link StringHelper#getHexString(byte[])} to convert the byte array to
	 * a Hex String which is printable
	 * 
	 * @param string
	 *            the string to hash
	 * 
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hashSha256(String string) {
		return StringHelper.hashSha256(string.getBytes());
	}

	/**
	 * Generates the SHA1 Hash of a byte array Use {@link StringHelper#getHexString(byte[])} to convert the byte array
	 * to a Hex String which is printable
	 * 
	 * @param bytes
	 *            the bytes to hash
	 * 
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hashSha256(byte[] bytes) {
		return StringHelper.hash("SHA-256", bytes);
	}

	/**
	 * Returns the hash of an algorithm
	 * 
	 * @param algorithm
	 *            the algorithm to use
	 * @param bytes
	 *            the bytes to hash
	 * 
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hash(String algorithm, byte[] bytes) {
		try {

			MessageDigest digest = MessageDigest.getInstance(algorithm);
			byte[] hashArray = digest.digest(bytes);

			return hashArray;

		} catch (NoSuchAlgorithmException e) {
			throw new RuntimeException("Algorithm " + algorithm + " does not exist!", e);
		}
	}

	/**
	 * Normalizes the length of a String. Does not shorten it when it is too long, but lengthens it, depending on the
	 * options set: adding the char at the beginning or appending it at the end
	 * 
	 * @param value
	 *            string to normalize
	 * @param length
	 *            length string must have
	 * @param beginning
	 *            add at beginning of value
	 * @param c
	 *            char to append when appending
	 * @return the new string
	 */
	public static String normalizeLength(String value, int length, boolean beginning, char c) {
		return StringHelper.normalizeLength(value, length, beginning, false, c);
	}

	/**
	 * Normalizes the length of a String. Shortens it when it is too long, giving out a logger warning, or lengthens it,
	 * depending on the options set: appending the char at the beginning or the end
	 * 
	 * @param value
	 *            string to normalize
	 * @param length
	 *            length string must have
	 * @param beginning
	 *            append at beginning of value
	 * @param shorten
	 *            allow shortening of value
	 * @param c
	 *            char to append when appending
	 * @return the new string
	 */
	public static String normalizeLength(String value, int length, boolean beginning, boolean shorten, char c) {

		if (value.length() == length)
			return value;

		if (value.length() < length) {

			String tmp = value;
			while (tmp.length() != length) {
				if (beginning) {
					tmp = c + tmp;
				} else {
					tmp = tmp + c;
				}
			}

			return tmp;

		} else if (shorten) {

			StringHelper.logger.warn("Shortening length of value: " + value);
			StringHelper.logger.warn("Length is: " + value.length() + " max: " + length);

			return value.substring(0, length);
		}

		return value;
	}

	/**
	 * Calls {@link #replacePropertiesIn(Properties, String)}, with {@link System#getProperties()} as input
	 * 
	 * @return a new string with all defined system properties replaced or if an error occurred the original value is
	 *         returned
	 */
	public static String replaceSystemPropertiesIn(String value) {
		return StringHelper.replacePropertiesIn(System.getProperties(), value);
	}

	/**
	 * Traverses the given string searching for occurrences of ${...} sequences. Theses sequences are replaced with a
	 * {@link Properties#getProperty(String)} value if such a value exists in the properties map. If the value of the
	 * sequence is not in the properties, then the sequence is not replaced
	 * 
	 * @param properties
	 *            the {@link Properties} in which to get the value
	 * @param value
	 *            the value in which to replace any system properties
	 * 
	 * @return a new string with all defined properties replaced or if an error occurred the original value is returned
	 */
	public static String replacePropertiesIn(Properties properties, String alue) {

		// get a copy of the value
		String tmpValue = alue;

		// get first occurrence of $ character
		int pos = -1;
		int stop = 0;

		// loop on $ character positions
		while ((pos = tmpValue.indexOf('$', pos + 1)) != -1) {

			// if pos+1 is not { character then continue
			if (tmpValue.charAt(pos + 1) != '{') {
				continue;
			}

			// find end of sequence with } character
			stop = tmpValue.indexOf('}', pos + 1);

			// if no stop found, then break as another sequence should be able to start
			if (stop == -1) {
				StringHelper.logger.error("Sequence starts at offset " + pos + " but does not end!");
				tmpValue = alue;
				break;
			}

			// get sequence enclosed by pos and stop
			String sequence = tmpValue.substring(pos + 2, stop);

			// make sure sequence doesn't contain $ { } characters
			if (sequence.contains("$") || sequence.contains("{") || sequence.contains("}")) {
				StringHelper.logger.error("Enclosed sequence in offsets " + pos + " - " + stop
						+ " contains one of the illegal chars: $ { }: " + sequence);
				tmpValue = alue;
				break;
			}

			// sequence is good, so see if we have a property for it
			String property = properties.getProperty(sequence, "");

			// if no property exists, then log and continue
			if (property.isEmpty()) {
				// logger.warn("No system property found for sequence " + sequence);
				continue;
			}

			// property exists, so replace in value
			tmpValue = tmpValue.replace("${" + sequence + "}", property);
		}

		return tmpValue;
	}

	/**
	 * Calls {@link #replaceProperties(Properties, Properties)} with null as the second argument. This allows for
	 * replacing all properties with itself
	 * 
	 * @param properties
	 *            the properties in which the values must have any ${...} replaced by values of the respective key
	 */
	public static void replaceProperties(Properties properties) {
		StringHelper.replaceProperties(properties, null);
	}

	/**
	 * Checks every value in the {@link Properties} and then then replaces any ${...} variables with keys in this
	 * {@link Properties} value using {@link StringHelper#replacePropertiesIn(Properties, String)}
	 * 
	 * @param properties
	 *            the properties in which the values must have any ${...} replaced by values of the respective key
	 * @param altProperties
	 *            if properties does not contain the ${...} key, then try these alternative properties
	 */
	public static void replaceProperties(Properties properties, Properties altProperties) {

		for (Object keyObj : properties.keySet()) {
			String key = (String) keyObj;
			String property = properties.getProperty(key);
			String newProperty = StringHelper.replacePropertiesIn(properties, property);

			// try first properties
			if (!property.equals(newProperty)) {
				// logger.info("Key " + key + " has replaced property " + property + " with new value " + newProperty);
				properties.put(key, newProperty);
			} else if (altProperties != null) {

				// try alternative properties
				newProperty = StringHelper.replacePropertiesIn(altProperties, property);
				if (!property.equals(newProperty)) {
					// logger.info("Key " + key + " has replaced property " + property + " from alternative properties with new value " + newProperty);
					properties.put(key, newProperty);
				}
			}
		}
	}

	/**
	 * This is a helper method with which it is possible to print the location in the two given strings where they start
	 * to differ. The length of string returned is currently 40 characters, or less if either of the given strings are
	 * shorter. The format of the string is 3 lines. The first line has information about where in the strings the
	 * difference occurs, and the second and third lines contain contexts
	 * 
	 * @param s1
	 *            the first string
	 * @param s2
	 *            the second string
	 * 
	 * @return the string from which the strings differ with a length of 40 characters within the original strings
	 */
	public static String printUnequalContext(String s1, String s2) {

		byte[] bytes1 = s1.getBytes();
		byte[] bytes2 = s2.getBytes();
		int i = 0;
		for (; i < bytes1.length; i++) {
			if (i > bytes2.length)
				break;

			if (bytes1[i] != bytes2[i])
				break;
		}

		int maxContext = 40;
		int start = Math.max(0, (i - maxContext));
		int end = Math.min(i + maxContext, (Math.min(bytes1.length, bytes2.length)));

		StringBuilder sb = new StringBuilder();
		sb.append("Strings are not equal! Start of inequality is at " + i + ". Showing " + maxContext
				+ " extra characters and start and end:\n");
		sb.append("context s1: " + s1.substring(start, end) + "\n");
		sb.append("context s2: " + s2.substring(start, end) + "\n");

		return sb.toString();
	}

	/**
	 * Formats the given number of milliseconds to a time like 0.000s/ms
	 * 
	 * @param millis
	 *            the number of milliseconds
	 * 
	 * @return format the given number of milliseconds to a time like 0.000s/ms
	 */
	public static String formatMillisecondsDuration(final long millis) {
		if (millis > 1000) {
			return String.format("%.3fs", (((double) millis) / 1000)); //$NON-NLS-1$
		}

		return millis + "ms"; //$NON-NLS-1$
	}

	/**
	 * Formats the given number of nanoseconds to a time like 0.000s/ms/us/ns
	 * 
	 * @param nanos
	 *            the number of nanoseconds
	 * 
	 * @return format the given number of nanoseconds to a time like 0.000s/ms/us/ns
	 */
	public static String formatNanoDuration(final long nanos) {
		if (nanos > 1000000000) {
			return String.format("%.3fs", (((double) nanos) / 1000000000)); //$NON-NLS-1$
		} else if (nanos > 1000000) {
			return String.format("%.3fms", (((double) nanos) / 1000000)); //$NON-NLS-1$
		} else if (nanos > 1000) {
			return String.format("%.3fus", (((double) nanos) / 1000)); //$NON-NLS-1$
		} else {
			return nanos + "ns"; //$NON-NLS-1$
		}
	}

	/**
	 * Simply returns true if the value is null, or empty
	 * 
	 * @param value
	 *            the value to check
	 * 
	 * @return true if the value is null, or empty
	 */
	public static boolean isEmpty(String value) {
		return value == null || value.isEmpty();
	}

	/**
	 * <p>
	 * Parses the given string value to a boolean. This extends the default {@link Boolean#parseBoolean(String)} as it
	 * throws an exception if the string value is not equal to "true" or "false" being case insensitive.
	 * </p>
	 * 
	 * <p>
	 * This additional restriction is important where false should really be caught, not any random vaue for false
	 * </p>
	 * 
	 * @param value
	 *            the value to check
	 * 
	 * @return true or false, depending on the string value
	 * 
	 * @throws RuntimeException
	 *             if the value is empty, or not equal to the case insensitive value "true" or "false"
	 */
	public static boolean parseBoolean(String value) throws RuntimeException {
		if (isEmpty(value))
			throw new RuntimeException("Value to parse to boolean is empty! Expected case insensitive true or false");
		String tmp = value.toLowerCase();
		if (tmp.equals(Boolean.TRUE.toString())) {
			return true;
		} else if (tmp.equals(Boolean.FALSE.toString())) {
			return false;
		} else {
			throw new RuntimeException("Value " + value
					+ " can not be parsed to boolean! Expected case insensitive true or false");
		}
	}
}
