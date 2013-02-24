/*
 * Copyright (c) 2012, Robert von Burg
 *
 * All rights reserved.
 *
 * This file is part of the ch.eitchnet.java.utils.
 *
 *  ch.eitchnet.java.utils is free software: you can redistribute 
 *  it and/or modify it under the terms of the GNU General Public License as 
 *  published by the Free Software Foundation, either version 3 of the License, 
 *  or (at your option) any later version.
 *
 *  ch.eitchnet.java.utils is distributed in the hope that it will 
 *  be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with ch.eitchnet.java.utils.  If not, see 
 *  <http://www.gnu.org/licenses/>.
 */
package ch.eitchnet.utils.helper;

/**
 * @author Robert von Burg <eitch@eitchnet.ch>
 * 
 */
public class ByteHelper {

	/**
	 * Creates a long of the given byte array. They byte array must be 8 bytes long. The byte at index 0 is the highest
	 * byte
	 * 
	 * @param bytes
	 *            the bytes to convert to a long
	 * 
	 * @return the long created from the bytes
	 */
	public static long toLong(byte[] bytes) {

		if (bytes.length != 8)
			throw new IllegalArgumentException("The input byte array for a long must have 8 values");

		return ((long) (bytes[0] & 0xff) << 56) //
				| ((long) (bytes[1] & 0xff) << 48) //
				| ((long) (bytes[2] & 0xff) << 40) //
				| ((long) (bytes[3] & 0xff) << 32) //
				| ((long) (bytes[4] & 0xff) << 24) //
				| ((long) (bytes[5] & 0xff) << 16) //
				| ((long) (bytes[6] & 0xff) << 8) // 
				| ((bytes[7] & 0xff));
	}

	/**
	 * Creates an integer of the given byte array. They byte array must be 4 bytes long. The byte at index 0 is the
	 * highest byte
	 * 
	 * @param bytes
	 *            the bytes to convert to an integer
	 * 
	 * @return the integer created from the bytes
	 */
	public static int toInt(byte[] bytes) {

		if (bytes.length != 4)
			throw new IllegalArgumentException("The input byte array for a long must have 4 values");

		return ((bytes[0] & 0xff) << 24) //
				| ((bytes[1] & 0xff) << 16) //
				| ((bytes[2] & 0xff) << 8) //
				| ((bytes[3] & 0xff));
	}

	/**
	 * Formats the given byte to a binary string
	 * 
	 * @param b
	 *            the byte to format to a binary string
	 * 
	 * @return the binary string
	 */
	public static String asBinary(byte b) {

		StringBuilder sb = new StringBuilder();

		sb.append(((b >>> 7) & 1));
		sb.append(((b >>> 6) & 1));
		sb.append(((b >>> 5) & 1));
		sb.append(((b >>> 4) & 1));
		sb.append(((b >>> 3) & 1));
		sb.append(((b >>> 2) & 1));
		sb.append(((b >>> 1) & 1));
		sb.append(((b >>> 0) & 1));

		return sb.toString();
	}

	/**
	 * Formats the given byte array to a binary string, separating each byte by a space
	 * 
	 * @param b
	 *            the byte to format to a binary string
	 * 
	 * @return the binary string
	 */
	public static String asBinary(byte[] bytes) {
		StringBuilder sb = new StringBuilder();

		for (byte b : bytes) {
			sb.append(asBinary(b));
			sb.append(" ");
		}

		return sb.toString();
	}

	/**
	 * Formats the given integer to a binary string, each byte is separated by a space
	 * 
	 * @param i
	 *            the integer to format to a string
	 * 
	 * @return the binary string
	 */
	public static String asBinary(int i) {

		StringBuilder sb = new StringBuilder();

		sb.append(((i >>> 31) & 1));
		sb.append(((i >>> 30) & 1));
		sb.append(((i >>> 29) & 1));
		sb.append(((i >>> 28) & 1));
		sb.append(((i >>> 27) & 1));
		sb.append(((i >>> 26) & 1));
		sb.append(((i >>> 25) & 1));
		sb.append(((i >>> 24) & 1));

		sb.append(" ");

		sb.append(((i >>> 23) & 1));
		sb.append(((i >>> 22) & 1));
		sb.append(((i >>> 21) & 1));
		sb.append(((i >>> 20) & 1));
		sb.append(((i >>> 19) & 1));
		sb.append(((i >>> 18) & 1));
		sb.append(((i >>> 17) & 1));
		sb.append(((i >>> 16) & 1));

		sb.append(" ");

		sb.append(((i >>> 15) & 1));
		sb.append(((i >>> 14) & 1));
		sb.append(((i >>> 13) & 1));
		sb.append(((i >>> 12) & 1));
		sb.append(((i >>> 11) & 1));
		sb.append(((i >>> 10) & 1));
		sb.append(((i >>> 9) & 1));
		sb.append(((i >>> 8) & 1));

		sb.append(" ");

		sb.append(((i >>> 7) & 1));
		sb.append(((i >>> 6) & 1));
		sb.append(((i >>> 5) & 1));
		sb.append(((i >>> 4) & 1));
		sb.append(((i >>> 3) & 1));
		sb.append(((i >>> 2) & 1));
		sb.append(((i >>> 1) & 1));
		sb.append(((i >>> 0) & 1));

		return sb.toString();
	}

	/**
	 * Formats the given long to a binary string, each byte is separated by a space
	 * 
	 * @param i
	 *            the long to format
	 * 
	 * @return the binary string
	 */
	public static String asBinary(long i) {

		StringBuilder sb = new StringBuilder();

		sb.append(((i >>> 63) & 1));
		sb.append(((i >>> 62) & 1));
		sb.append(((i >>> 61) & 1));
		sb.append(((i >>> 60) & 1));
		sb.append(((i >>> 59) & 1));
		sb.append(((i >>> 58) & 1));
		sb.append(((i >>> 57) & 1));
		sb.append(((i >>> 56) & 1));

		sb.append(" ");

		sb.append(((i >>> 55) & 1));
		sb.append(((i >>> 54) & 1));
		sb.append(((i >>> 53) & 1));
		sb.append(((i >>> 52) & 1));
		sb.append(((i >>> 51) & 1));
		sb.append(((i >>> 50) & 1));
		sb.append(((i >>> 49) & 1));
		sb.append(((i >>> 48) & 1));

		sb.append(" ");

		sb.append(((i >>> 47) & 1));
		sb.append(((i >>> 46) & 1));
		sb.append(((i >>> 45) & 1));
		sb.append(((i >>> 44) & 1));
		sb.append(((i >>> 43) & 1));
		sb.append(((i >>> 42) & 1));
		sb.append(((i >>> 41) & 1));
		sb.append(((i >>> 40) & 1));

		sb.append(" ");

		sb.append(((i >>> 39) & 1));
		sb.append(((i >>> 38) & 1));
		sb.append(((i >>> 37) & 1));
		sb.append(((i >>> 36) & 1));
		sb.append(((i >>> 35) & 1));
		sb.append(((i >>> 34) & 1));
		sb.append(((i >>> 33) & 1));
		sb.append(((i >>> 32) & 1));

		sb.append(" ");

		sb.append(((i >>> 31) & 1));
		sb.append(((i >>> 30) & 1));
		sb.append(((i >>> 29) & 1));
		sb.append(((i >>> 28) & 1));
		sb.append(((i >>> 27) & 1));
		sb.append(((i >>> 26) & 1));
		sb.append(((i >>> 25) & 1));
		sb.append(((i >>> 24) & 1));

		sb.append(" ");

		sb.append(((i >>> 23) & 1));
		sb.append(((i >>> 22) & 1));
		sb.append(((i >>> 21) & 1));
		sb.append(((i >>> 20) & 1));
		sb.append(((i >>> 19) & 1));
		sb.append(((i >>> 18) & 1));
		sb.append(((i >>> 17) & 1));
		sb.append(((i >>> 16) & 1));

		sb.append(" ");

		sb.append(((i >>> 15) & 1));
		sb.append(((i >>> 14) & 1));
		sb.append(((i >>> 13) & 1));
		sb.append(((i >>> 12) & 1));
		sb.append(((i >>> 11) & 1));
		sb.append(((i >>> 10) & 1));
		sb.append(((i >>> 9) & 1));
		sb.append(((i >>> 8) & 1));

		sb.append(" ");

		sb.append(((i >>> 7) & 1));
		sb.append(((i >>> 6) & 1));
		sb.append(((i >>> 5) & 1));
		sb.append(((i >>> 4) & 1));
		sb.append(((i >>> 3) & 1));
		sb.append(((i >>> 2) & 1));
		sb.append(((i >>> 1) & 1));
		sb.append(((i >>> 0) & 1));

		return sb.toString();
	}
}
