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
package li.strolch.utils.helper;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.MessageFormat;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A helper class to perform different actions on {@link String}s
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class StringHelper {

	public static final String NEW_LINE = "\n"; //$NON-NLS-1$
	public static final String EMPTY = ""; //$NON-NLS-1$
	public static final String SPACE = " "; //$NON-NLS-1$
	public static final String NULL = "null"; //$NON-NLS-1$
	public static final String DASH = "-"; //$NON-NLS-1$
	public static final String UNDERLINE = "_"; //$NON-NLS-1$
	public static final String COMMA = ","; //$NON-NLS-1$
	public static final String DOT = "."; //$NON-NLS-1$
	public static final String SEMICOLON = ";"; //$NON-NLS-1$
	public static final String COLON = ":"; //$NON-NLS-1$

	private static final Logger logger = LoggerFactory.getLogger(StringHelper.class);

	/**
	 * the semi-unique id which is incremented on every {@link #getUniqueId()}-method call
	 */
	private static long uniqueId = System.currentTimeMillis() - 1119953500000l;

	/**
	 * Hex char table for fast calculating of hex values
	 */
	private static final byte[] HEX_CHAR_TABLE = { (byte) '0',
			(byte) '1',
			(byte) '2',
			(byte) '3',
			(byte) '4',
			(byte) '5',
			(byte) '6',
			(byte) '7',
			(byte) '8',
			(byte) '9',
			(byte) 'a',
			(byte) 'b',
			(byte) 'c',
			(byte) 'd',
			(byte) 'e',
			(byte) 'f' };

	public static String toHexString(byte data) {
		return String.format("%02x", data);
	}

	public static String toPrettyHexString(byte[] raw) {
		try {
			byte[] hex = new byte[3 * raw.length + (raw.length / 8)];
			int index = 0;

			for (int i = 0; i < raw.length; i++) {
				byte b = raw[i];
				int v = b & 0xFF;
				hex[index++] = HEX_CHAR_TABLE[v >>> 4];
				hex[index++] = HEX_CHAR_TABLE[v & 0xF];
				hex[index++] = ' ';

				if ((i + 1) % 8 == 0) {
					hex[index++] = ' ';
				}
			}

			return new String(hex, "ASCII"); //$NON-NLS-1$

		} catch (UnsupportedEncodingException e) {
			String msg = MessageFormat
					.format("Something went wrong while converting to HEX: {0}", e.getMessage()); //$NON-NLS-1$
			throw new RuntimeException(msg, e);
		}
	}

	public static byte[] fromPrettyHexString(String prettyHex) {
		String s = prettyHex.replace(" ", "");
		int len = s.length();
		byte[] data = new byte[len / 2];
		for (int i = 0; i < len; i += 2) {
			data[i / 2] = (byte) ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i + 1), 16));
		}
		return data;
	}

	/**
	 * Converts each byte of the given byte array to a HEX value and returns the concatenation of these values
	 *
	 * @param raw
	 * 		the bytes to convert to String using numbers in hexadecimal
	 *
	 * @return the encoded string
	 *
	 * @throws RuntimeException
	 * 		if {@link UnsupportedEncodingException} is thrown
	 */
	public static String toHexString(byte[] raw) throws RuntimeException {
		return toHexString(raw, 0, raw.length);
	}

	/**
	 * Converts each byte of the given byte array to a HEX value and returns the concatenation of these values
	 *
	 * @param raw
	 * 		the bytes to convert to String using numbers in hexadecimal
	 *
	 * @return the encoded string
	 *
	 * @throws RuntimeException
	 * 		if {@link UnsupportedEncodingException} is thrown
	 */
	public static String toHexString(byte[] raw, int offset, int length) throws RuntimeException {
		try {
			byte[] hex = new byte[2 * length];
			int index = 0;

			int pos = offset;
			for (int i = 0; i < length; i++) {
				byte b = raw[pos];
				int v = b & 0xFF;
				hex[index++] = HEX_CHAR_TABLE[v >>> 4];
				hex[index++] = HEX_CHAR_TABLE[v & 0xF];
				pos++;
			}

			return new String(hex, "ASCII"); //$NON-NLS-1$

		} catch (UnsupportedEncodingException e) {
			String msg = MessageFormat
					.format("Something went wrong while converting to HEX: {0}", e.getMessage()); //$NON-NLS-1$
			throw new RuntimeException(msg, e);
		}
	}

	public static byte fromHexStringByte(String encoded) {
		if (encoded.length() != 2)
			throw new IllegalArgumentException("Input string must be exactly two characters long."); //$NON-NLS-1$
		byte result = (byte) Integer.parseInt(encoded, 16);
		return result;
	}

	/**
	 * Returns a byte array of a given string by converting each character of the string to a number base 16
	 *
	 * @param encoded
	 * 		the string to convert to a byt string
	 *
	 * @return the encoded byte stream
	 */
	public static byte[] fromHexString(String encoded) {
		if ((encoded.length() % 2) != 0)
			throw new IllegalArgumentException("Input string must contain an even number of characters."); //$NON-NLS-1$

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
	 * 		the string to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static String hashMd5AsHex(String string) {
		return toHexString(hashMd5(string.getBytes()));
	}

	/**
	 * Generates the MD5 Hash of a string. Use {@link StringHelper#toHexString(byte[])} to convert the byte array to a
	 * Hex String which is printable
	 *
	 * @param string
	 * 		the string to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hashMd5(String string) {
		return hashMd5(string.getBytes());
	}

	/**
	 * Generates the MD5 Hash of a byte array Use {@link StringHelper#toHexString(byte[])} to convert the byte array to
	 * a Hex String which is printable
	 *
	 * @param bytes
	 * 		the bytes to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hashMd5(byte[] bytes) {
		return hash("MD5", bytes); //$NON-NLS-1$
	}

	/**
	 * Generates the SHA1 Hash of a string and converts it to a HEX String
	 *
	 * @param string
	 * 		the string to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static String hashSha1AsHex(String string) {
		return toHexString(hashSha1(string.getBytes()));
	}

	/**
	 * Generates the SHA1 Hash of a string Use {@link StringHelper#toHexString(byte[])} to convert the byte array to a
	 * Hex String which is printable
	 *
	 * @param string
	 * 		the string to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hashSha1(String string) {
		return hashSha1(string.getBytes());
	}

	/**
	 * Generates the SHA1 Hash of a byte array Use {@link StringHelper#toHexString(byte[])} to convert the byte array to
	 * a Hex String which is printable
	 *
	 * @param bytes
	 * 		the bytes to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hashSha1(byte[] bytes) {
		return hash("SHA-1", bytes); //$NON-NLS-1$
	}

	/**
	 * Generates the SHA-256 Hash of a string and converts it to a HEX String
	 *
	 * @param string
	 * 		the string to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static String hashSha256AsHex(String string) {
		return toHexString(hashSha256(string.getBytes()));
	}

	/**
	 * Generates the SHA-256 Hash of a string Use {@link StringHelper#toHexString(byte[])} to convert the byte array to
	 * a Hex String which is printable
	 *
	 * @param string
	 * 		the string to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hashSha256(String string) {
		return hashSha256(string.getBytes());
	}

	/**
	 * Generates the SHA1 Hash of a byte array Use {@link StringHelper#toHexString(byte[])} to convert the byte array to
	 * a Hex String which is printable
	 *
	 * @param bytes
	 * 		the bytes to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hashSha256(byte[] bytes) {
		return hash("SHA-256", bytes); //$NON-NLS-1$
	}

	/**
	 * Returns the hash of an algorithm
	 *
	 * @param algorithm
	 * 		the algorithm to use
	 * @param string
	 * 		the string to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static String hashAsHex(String algorithm, String string) {
		return toHexString(hash(algorithm, string));
	}

	/**
	 * Returns the hash of an algorithm
	 *
	 * @param algorithm
	 * 		the algorithm to use
	 * @param string
	 * 		the string to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hash(String algorithm, String string) {
		try {

			MessageDigest digest = MessageDigest.getInstance(algorithm);
			byte[] hashArray = digest.digest(string.getBytes());

			return hashArray;

		} catch (NoSuchAlgorithmException e) {
			String msg = MessageFormat.format("Algorithm {0} does not exist!", algorithm); //$NON-NLS-1$
			throw new RuntimeException(msg, e);
		}
	}

	/**
	 * Returns the hash of an algorithm
	 *
	 * @param algorithm
	 * 		the algorithm to use
	 * @param bytes
	 * 		the bytes to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static String hashAsHex(String algorithm, byte[] bytes) {
		return toHexString(hash(algorithm, bytes));
	}

	/**
	 * Returns the hash of an algorithm
	 *
	 * @param algorithm
	 * 		the algorithm to use
	 * @param bytes
	 * 		the bytes to hash
	 *
	 * @return the hash or null, if an exception was thrown
	 */
	public static byte[] hash(String algorithm, byte[] bytes) {
		try {

			MessageDigest digest = MessageDigest.getInstance(algorithm);
			byte[] hashArray = digest.digest(bytes);

			return hashArray;

		} catch (NoSuchAlgorithmException e) {
			String msg = MessageFormat.format("Algorithm {0} does not exist!", algorithm); //$NON-NLS-1$
			throw new RuntimeException(msg, e);
		}
	}

	/**
	 * Normalizes the length of a String. Does not shorten it when it is too long, but lengthens it, depending on the
	 * options set: adding the char at the beginning or appending it at the end
	 *
	 * @param value
	 * 		string to normalize
	 * @param length
	 * 		length string must have
	 * @param beginning
	 * 		add at beginning of value
	 * @param c
	 * 		char to append when appending
	 *
	 * @return the new string
	 */
	public static String normalizeLength(String value, int length, boolean beginning, char c) {
		return normalizeLength(value, length, beginning, false, c);
	}

	/**
	 * Normalizes the length of a String. Shortens it when it is too long, giving out a logger warning, or lengthens it,
	 * depending on the options set: appending the char at the beginning or the end
	 *
	 * @param value
	 * 		string to normalize
	 * @param length
	 * 		length string must have
	 * @param beginning
	 * 		append at beginning of value
	 * @param shorten
	 * 		allow shortening of value
	 * @param c
	 * 		char to append when appending
	 *
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

			logger.warn(MessageFormat.format("Shortening length of value: {0}", value)); //$NON-NLS-1$
			logger.warn(MessageFormat.format("Length is: {0} max: {1}", value.length(), length)); //$NON-NLS-1$

			return value.substring(0, length);
		}

		return value;
	}

	/**
	 * Calls {@link #replacePropertiesIn(Properties, String)}, with {@link System#getProperties()} as input
	 *
	 * @return a new string with all defined system properties replaced or if an error occurred the original value is
	 * returned
	 */
	public static String replaceSystemPropertiesIn(String value) {
		return replacePropertiesIn(System.getProperties(), value);
	}

	/**
	 * Traverses the given string searching for occurrences of ${...} sequences. Theses sequences are replaced with a
	 * {@link Properties#getProperty(String)} value if such a value exists in the properties map. If the value of the
	 * sequence is not in the properties, then the sequence is not replaced
	 *
	 * @param properties
	 * 		the {@link Properties} in which to get the value
	 * @param value
	 * 		the value in which to replace any system properties
	 *
	 * @return a new string with all defined properties replaced or if an error occurred the original value is returned
	 */
	public static String replacePropertiesIn(Properties properties, String value) {
		return replacePropertiesIn(properties, "$", value);
	}

	/**
	 * Traverses the given string searching for occurrences of <code>prefix</code>{...} sequences. Theses sequences are
	 * replaced with a {@link Properties#getProperty(String)} value if such a value exists in the properties map. If the
	 * value of the sequence is not in the properties, then the sequence is not replaced
	 *
	 * @param properties
	 * 		the {@link Properties} in which to get the value
	 * @param prefix
	 * 		the prefix to use, for instance use <code>$</code> to replace occurrences of <code>$</code>{...}
	 * @param value
	 * 		the value in which to replace any system properties
	 *
	 * @return a new string with all defined properties replaced or if an error occurred the original value is returned
	 */
	public static String replacePropertiesIn(Properties properties, String prefix, String value) {

		String startTag = prefix + "{";
		int tagLength = startTag.length();

		// get a copy of the value
		String tmpValue = value;

		// get first occurrence of $ character
		int pos = -1;
		int stop = 0;

		// loop on prefix positions
		while ((pos = tmpValue.indexOf(startTag, pos)) != -1) {

			// find end of sequence with } character
			stop = tmpValue.indexOf('}', pos + 1);

			// if no stop found, then break as another sequence should be able to start
			if (stop == -1) {
				logger.error(
						MessageFormat.format("Sequence starts at offset {0} but does not end!", pos)); //$NON-NLS-1$
				tmpValue = value;
				break;
			}

			// get sequence enclosed by pos and stop

			String sequence = tmpValue.substring(pos + tagLength, stop);

			// make sure sequence doesn't contain $ { } characters
			if (sequence.contains(startTag) || sequence.contains("}")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				String msg = "Enclosed sequence in offsets {0} - {1} contains one of the illegal chars: {2} { }: {3}";
				msg = MessageFormat.format(msg, pos, stop, prefix, sequence);
				logger.error(msg);
				tmpValue = value;
				break;
			}

			// sequence is good, so see if we have a property for it
			String property = properties.getProperty(sequence);

			// if no property exists, then log and continue
			if (property == null) {
				pos = stop;
				// logger.warn("No system property found for sequence " + sequence);
				continue;
			}

			// property exists, so replace in value
			tmpValue = tmpValue.replace(startTag + sequence + "}", property); //$NON-NLS-1$ //$NON-NLS-2$
		}

		return tmpValue;
	}

	/**
	 * Calls {@link #replaceProperties(Properties, Properties)} with null as the second argument. This allows for
	 * replacing all properties with itself
	 *
	 * @param properties
	 * 		the properties in which the values must have any ${...} replaced by values of the respective key
	 */
	public static void replaceProperties(Properties properties) {
		replaceProperties(properties, null);
	}

	/**
	 * Checks every value in the {@link Properties} and then then replaces any ${...} variables with keys in this
	 * {@link Properties} value using {@link StringHelper#replacePropertiesIn(Properties, String)}
	 *
	 * @param properties
	 * 		the properties in which the values must have any ${...} replaced by values of the respective key
	 * @param altProperties
	 * 		if properties does not contain the ${...} key, then try these alternative properties
	 */
	public static void replaceProperties(Properties properties, Properties altProperties) {

		for (Object keyObj : properties.keySet()) {
			String key = (String) keyObj;
			String property = properties.getProperty(key);
			String newProperty = replacePropertiesIn(properties, property);

			// try first properties
			if (!property.equals(newProperty)) {
				// logger.info("Key " + key + " has replaced property " + property + " with new value " + newProperty);
				properties.put(key, newProperty);
			} else if (altProperties != null) {

				// try alternative properties
				newProperty = replacePropertiesIn(altProperties, property);
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
	 * 		the first string
	 * @param s2
	 * 		the second string
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
		sb.append("Strings are not equal! Start of inequality is at " + i); //$NON-NLS-1$
		sb.append(". Showing " + maxContext); //$NON-NLS-1$
		sb.append(" extra characters and start and end:\n"); //$NON-NLS-1$
		sb.append("context s1: "); //$NON-NLS-1$
		sb.append(s1.substring(start, end));
		sb.append("\n"); //$NON-NLS-1$
		sb.append("context s2: "); //$NON-NLS-1$
		sb.append(s2.substring(start, end));
		sb.append("\n"); //$NON-NLS-1$

		return sb.toString();
	}

	/**
	 * Formats the given number of milliseconds to a time like #h/m/s/ms/us/ns
	 *
	 * @param millis
	 * 		the number of milliseconds
	 *
	 * @return format the given number of milliseconds to a time like #h/m/s/ms/us/ns
	 */
	public static String formatMillisecondsDuration(final long millis) {
		return formatNanoDuration(millis * 1000000L);
	}

	/**
	 * Formats the given number of nanoseconds to a time like #h/m/s/ms/us/ns
	 *
	 * @param nanos
	 * 		the number of nanoseconds
	 *
	 * @return format the given number of nanoseconds to a time like #h/m/s/ms/us/ns
	 */
	public static String formatNanoDuration(final long nanos) {
		if (nanos >= 3600000000000L) {
			return String.format("%.0fh", (nanos / 3600000000000.0D)); //$NON-NLS-1$
		} else if (nanos >= 60000000000L) {
			return String.format("%.0fm", (nanos / 60000000000.0D)); //$NON-NLS-1$
		} else if (nanos >= 1000000000L) {
			return String.format("%.0fs", (nanos / 1000000000.0D)); //$NON-NLS-1$
		} else if (nanos >= 1000000L) {
			return String.format("%.0fms", (nanos / 1000000.0D)); //$NON-NLS-1$
		} else if (nanos >= 1000L) {
			return String.format("%.0fus", (nanos / 1000.0D)); //$NON-NLS-1$
		} else {
			return nanos + "ns"; //$NON-NLS-1$
		}
	}

	/**
	 * @see ExceptionHelper#formatException(Throwable)
	 */
	public static String getExceptionMessage(Throwable t) {
		return ExceptionHelper.getExceptionMessage(t);
	}

	/**
	 * @see ExceptionHelper#formatException(Throwable)
	 */
	public static String formatException(Throwable t) {
		return ExceptionHelper.formatException(t);
	}

	/**
	 * @see ExceptionHelper#formatExceptionMessage(Throwable)
	 */
	public static String formatExceptionMessage(Throwable t) {
		return ExceptionHelper.formatExceptionMessage(t);
	}

	/**
	 * Simply returns true if the value is null, or empty
	 *
	 * @param value
	 * 		the value to check
	 *
	 * @return true if the value is null, or empty
	 */
	public static boolean isEmpty(String value) {
		return value == null || value.isEmpty();
	}

	/**
	 * Simply returns true if the value is neither null nor empty
	 *
	 * @param value
	 * 		the value to check
	 *
	 * @return true if the value is neither null nor empty
	 */
	public static boolean isNotEmpty(String value) {
		return value != null && !value.isEmpty();
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
	 * 		the value to check
	 *
	 * @return true or false, depending on the string value
	 *
	 * @throws RuntimeException
	 * 		if the value is empty, or not equal to the case insensitive value "true" or "false"
	 */
	public static boolean parseBoolean(String value) throws RuntimeException {
		if (isEmpty(value))
			throw new RuntimeException(
					"Value to parse to boolean is empty! Expected case insensitive true or false"); //$NON-NLS-1$
		String tmp = value.toLowerCase();
		if (tmp.equals(Boolean.TRUE.toString())) {
			return true;
		} else if (tmp.equals(Boolean.FALSE.toString())) {
			return false;
		} else {
			String msg = "Value {0} can not be parsed to boolean! Expected case insensitive true or false"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, value);
			throw new RuntimeException(msg);
		}
	}

	public static String commaSeparated(String... values) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < values.length; i++) {
			sb.append(values[i]);
			if (i < values.length - 1)
				sb.append(", "); //$NON-NLS-1$
		}
		return sb.toString();
	}

	public static String[] splitCommaSeparated(String values) {
		String[] split = values.split(","); //$NON-NLS-1$
		for (int i = 0; i < split.length; i++) {
			split[i] = split[i].trim();
		}
		return split;
	}

	/**
	 * If the value parameter is empty, then a {@link #DASH} is returned, otherwise the value is returned
	 *
	 * @param value
	 * 		the value
	 *
	 * @return the non-empty value, or a {@link #DASH}
	 */
	public static String valueOrDash(String value) {
		if (isNotEmpty(value))
			return value;
		return DASH;
	}

	/**
	 * Return a pseudo unique id which is incremented on each call. The id is initialized from the current time
	 *
	 * @return a pseudo unique id
	 */
	public static synchronized String getUniqueId() {
		return Long.toString(getUniqueIdLong());
	}

	/**
	 * Return a pseudo unique id which is incremented on each call. The id is initialized from the current time
	 *
	 * @return a pseudo unique id
	 */
	public static synchronized long getUniqueIdLong() {

		if (uniqueId == Long.MAX_VALUE - 1) {
			uniqueId = 0;
		}

		uniqueId += 1;
		return uniqueId;
	}
}
