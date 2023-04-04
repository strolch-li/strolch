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

/**
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class ByteHelper {

	public static boolean isBitSet(byte data, int position) {
		if (position > 7)
			throw new IllegalStateException("Position " + position + " is not available in a byte!");
		return ((data >> position) & 1) == 1;
	}

	public static boolean isBitSet(short data, int position) {
		if (position > 15)
			throw new IllegalStateException("Position " + position + " is not available in a short!");
		return ((data >> position) & 1) == 1;
	}

	public static boolean isBitSet(int data, int position) {
		if (position > 31)
			throw new IllegalStateException("Position " + position + " is not available in an int!");
		return ((data >> position) & 1) == 1;
	}

	public static boolean isBitSet(long data, int position) {
		if (position > 63)
			throw new IllegalStateException("Position " + position + " is not available in a long!");
		return ((data >> position) & 1) == 1;
	}

	public static boolean isBitNotSet(byte data, int position) {
		if (position > 7)
			throw new IllegalStateException("Position " + position + " is not available in a byte!");
		return ((data >> position) & 1) == 0;
	}

	public static boolean isBitNotSet(short data, int position) {
		if (position > 15)
			throw new IllegalStateException("Position " + position + " is not available in a short!");
		return ((data >> position) & 1) == 0;
	}

	public static boolean isBitNotSet(int data, int position) {
		if (position > 31)
			throw new IllegalStateException("Position " + position + " is not available in an int!");
		return ((data >> position) & 1) == 0;
	}

	public static boolean isBitNotSet(long data, int position) {
		if (position > 63)
			throw new IllegalStateException("Position " + position + " is not available in a long!");
		return ((data >> position) & 1) == 0;
	}

	public static byte setBit(byte data, int position) {
		if (position > 7)
			throw new IllegalStateException("Position " + position + " is not available in a byte!");
		return (byte) (data | (1 << position));
	}

	public static short setBit(short data, int position) {
		if (position > 15)
			throw new IllegalStateException("Position " + position + " is not available in a short!");
		return (short) (data | (1 << position));
	}

	public static int setBit(int data, int position) {
		if (position > 31)
			throw new IllegalStateException("Position " + position + " is not available in an int!");
		return (data | (1 << position));
	}

	public static long setBit(long data, int position) {
		if (position > 63)
			throw new IllegalStateException("Position " + position + " is not available in a long!");
		return (data | (1 << position));
	}

	public static byte clearBit(byte data, int position) {
		if (position > 7)
			throw new IllegalStateException("Position " + position + " is not available in a byte!");
		return (byte) (data & ~(1 << position));
	}

	public static short clearBit(short data, int position) {
		if (position > 15)
			throw new IllegalStateException("Position " + position + " is not available in a short!");
		return (short) (data & ~(1 << position));
	}

	public static int clearBit(int data, int position) {
		if (position > 31)
			throw new IllegalStateException("Position " + position + " is not available in a int!");
		return (data & ~(1 << position));
	}

	public static long clearBit(long data, int position) {
		if (position > 63)
			throw new IllegalStateException("Position " + position + " is not available in a long!");
		return (data & ~(1 << position));
	}

	public static int countSetBits(byte data) {
		int setBits = 0;
		for (int i = 0; i < 8; i++) {
			if (isBitSet(data, i))
				setBits++;
		}
		return setBits;
	}

	public static int countSetBits(short data) {
		int setBits = 0;
		for (int i = 0; i < 16; i++) {
			if (isBitSet(data, i))
				setBits++;
		}
		return setBits;
	}

	public static int countSetBits(int data) {
		int setBits = 0;
		for (int i = 0; i < 32; i++) {
			if (isBitSet(data, i))
				setBits++;
		}
		return setBits;
	}

	public static int countSetBits(long data) {
		int setBits = 0;
		for (int i = 0; i < 64; i++) {
			if (isBitSet(data, i))
				setBits++;
		}
		return setBits;
	}

	public static byte getHighestBit(byte data) {
		byte pos = 0;
		for (byte i = 0; i < Byte.SIZE; i++) {
			if (isBitSet(data, i))
				pos = i;
		}
		return pos;
	}

	public static byte getHighestBit(short data) {
		byte pos = 0;
		for (byte i = 0; i < Short.SIZE; i++) {
			if (isBitSet(data, i))
				pos = i;
		}
		return pos;
	}

	public static byte getHighestBit(int data) {
		byte pos = 0;
		for (byte i = 0; i < Integer.SIZE; i++) {
			if (isBitSet(data, i))
				pos = i;
		}
		return pos;
	}

	/**
	 * Creates a long of the given byte array. They byte array must be 8 bytes long. The byte at index 0 is the highest
	 * byte
	 *
	 * @param bytes
	 * 		the bytes to convert to a long
	 *
	 * @return the long created from the bytes
	 */
	public static long toLong(byte[] bytes) {

		if (bytes.length != 8)
			throw new IllegalArgumentException("The input byte array for a long must have 8 values"); //$NON-NLS-1$

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
	 * 		the bytes to convert to an integer
	 *
	 * @return the integer created from the bytes
	 */
	public static int toInt(byte[] bytes) {

		if (bytes.length != 4)
			throw new IllegalArgumentException("The input byte array for an int must have 4 values"); //$NON-NLS-1$

		return ((bytes[0] & 0xff) << 24) //
				| ((bytes[1] & 0xff) << 16) //
				| ((bytes[2] & 0xff) << 8) //
				| ((bytes[3] & 0xff));
	}

	/**
	 * Creates a short of the given byte array. They byte array must be 2 bytes long. The byte at index 0 is the highest
	 * byte
	 *
	 * @param bytes
	 * 		the bytes to convert to an integer
	 *
	 * @return the integer created from the bytes
	 */
	public static short toShort(byte[] bytes) {

		if (bytes.length != 2)
			throw new IllegalArgumentException("The input byte array for a short must have 2 values"); //$NON-NLS-1$

		return (short) (((bytes[0] & 0xff) << 8) //
				| ((bytes[1] & 0xff)));
	}

	public static short toShort(byte high, byte low) {
		return (short) (((high & 0xff) << 8) | (low & 0xff));
	}

	public static byte[] toByteArrayLittleEndian(short value) {
		byte low = (byte) (value & 0xff);
		byte high = (byte) ((value >> 8) & 0xff);
		return new byte[] { low, high };
	}

	public static byte[] toByteArrayBigEndian(short value) {
		byte low = (byte) (value & 0xff);
		byte high = (byte) ((value >> 8) & 0xff);
		return new byte[] { high, low };
	}

	public static byte reverse(byte x) {
		byte b = 0;
		for (int i = 0; i < 8; ++i) {
			b <<= 1;
			b |= (x & 1);
			x >>= 1;
		}
		return b;
	}

	public static byte invert(byte x) {
		return (byte) (x ^ 0xff);
	}

	public static byte setLowerNibble(byte b, byte nibble) {
		return (byte) ((b & ~(0xf)) | nibble);
	}

	public static byte setUpperNibble(byte b, byte nibble) {
		return (byte) ((b & ~(0xf << 4)) | (nibble << 4));
	}

	public static byte getUpperNibble(byte b) {
		return (byte) (b >>> 4 & 0x0f);
	}

	public static byte getLowerNibble(byte b) {
		return (byte) (0x0f & b);
	}

	public static byte parseBinaryToByte(String value) {
		if (value.length() != Byte.SIZE)
			throw new IllegalStateException("parsing binary to byte requires exactly " + Byte.SIZE + " digits!");
		return (byte) Integer.parseInt(value, 2);
	}

	public static short parseBinaryToShort(String value) {
		if (value.length() != Short.SIZE)
			throw new IllegalStateException("parsing binary to short requires exactly " + Short.SIZE + " digits!");
		return (short) Integer.parseInt(value, 2);
	}

	public static int parseBinaryToInt(String value) {
		if (value.length() != Integer.SIZE)
			throw new IllegalStateException("parsing binary to int requires exactly " + Integer.SIZE + " digits!");
		return Integer.parseInt(value, 2);
	}

	/**
	 * Formats the given byte array to a binary string, separating each byte by a space
	 *
	 * @param bytes
	 * 		the byte to format to a binary string
	 *
	 * @return the binary string
	 */
	public static String asBinary(byte[] bytes) {
		StringBuilder sb = new StringBuilder();

		for (byte b : bytes) {
			sb.append(asBinary(b));
			sb.append(StringHelper.SPACE);
		}

		return sb.toString();
	}

	/**
	 * Formats the given byte to a binary string
	 *
	 * @param b
	 * 		the byte to format to a binary string
	 *
	 * @return the binary string
	 */
	public static String asBinary(byte b) {

		String sb =
				String.valueOf((b >>> 7) & 1) + ((b >>> 6) & 1) + ((b >>> 5) & 1) + ((b >>> 4) & 1) + ((b >>> 3) & 1)
						+ ((b >>> 2) & 1) + ((b >>> 1) & 1) + ((b) & 1);

		return sb;
	}

	/**
	 * Formats the given integer to a binary string, each byte is separated by a space
	 *
	 * @param i
	 * 		the integer to format to a string
	 *
	 * @return the binary string
	 */
	public static String asBinary(short i) {

		String sb =
				String.valueOf((i >>> 15) & 1) + ((i >>> 14) & 1) + ((i >>> 13) & 1) + ((i >>> 12) & 1) + ((i >>> 11)
						& 1) + ((i >>> 10) & 1) + ((i >>> 9) & 1) + ((i >>> 8) & 1) + StringHelper.SPACE + ((i >>> 7)
						& 1) + ((i >>> 6) & 1) + ((i >>> 5) & 1) + ((i >>> 4) & 1) + ((i >>> 3) & 1) + ((i >>> 2) & 1)
						+ ((i >>> 1) & 1) + ((i) & 1);

		return sb;
	}

	/**
	 * Formats the given integer to a binary string, each byte is separated by a space
	 *
	 * @param i
	 * 		the integer to format to a string
	 *
	 * @return the binary string
	 */
	public static String asBinary(int i) {

		String sb =
				String.valueOf((i >>> 31) & 1) + ((i >>> 30) & 1) + ((i >>> 29) & 1) + ((i >>> 28) & 1) + ((i >>> 27)
						& 1) + ((i >>> 26) & 1) + ((i >>> 25) & 1) + ((i >>> 24) & 1) + StringHelper.SPACE + ((i >>> 23)
						& 1) + ((i >>> 22) & 1) + ((i >>> 21) & 1) + ((i >>> 20) & 1) + ((i >>> 19) & 1) + ((i >>> 18)
						& 1) + ((i >>> 17) & 1) + ((i >>> 16) & 1) + StringHelper.SPACE + ((i >>> 15) & 1) + ((i >>> 14)
						& 1) + ((i >>> 13) & 1) + ((i >>> 12) & 1) + ((i >>> 11) & 1) + ((i >>> 10) & 1) + ((i >>> 9)
						& 1) + ((i >>> 8) & 1) + StringHelper.SPACE + ((i >>> 7) & 1) + ((i >>> 6) & 1) + ((i >>> 5)
						& 1) + ((i >>> 4) & 1) + ((i >>> 3) & 1) + ((i >>> 2) & 1) + ((i >>> 1) & 1) + ((i) & 1);

		return sb;
	}

	/**
	 * Formats the given long to a binary string, each byte is separated by a space
	 *
	 * @param i
	 * 		the long to format
	 *
	 * @return the binary string
	 */
	public static String asBinary(long i) {

		String sb =
				String.valueOf((i >>> 63) & 1) + ((i >>> 62) & 1) + ((i >>> 61) & 1) + ((i >>> 60) & 1) + ((i >>> 59)
						& 1) + ((i >>> 58) & 1) + ((i >>> 57) & 1) + ((i >>> 56) & 1) + StringHelper.SPACE + ((i >>> 55)
						& 1) + ((i >>> 54) & 1) + ((i >>> 53) & 1) + ((i >>> 52) & 1) + ((i >>> 51) & 1) + ((i >>> 50)
						& 1) + ((i >>> 49) & 1) + ((i >>> 48) & 1) + StringHelper.SPACE + ((i >>> 47) & 1) + ((i >>> 46)
						& 1) + ((i >>> 45) & 1) + ((i >>> 44) & 1) + ((i >>> 43) & 1) + ((i >>> 42) & 1) + ((i >>> 41)
						& 1) + ((i >>> 40) & 1) + StringHelper.SPACE + ((i >>> 39) & 1) + ((i >>> 38) & 1) + ((i >>> 37)
						& 1) + ((i >>> 36) & 1) + ((i >>> 35) & 1) + ((i >>> 34) & 1) + ((i >>> 33) & 1) + ((i >>> 32)
						& 1) + StringHelper.SPACE + ((i >>> 31) & 1) + ((i >>> 30) & 1) + ((i >>> 29) & 1) + ((i >>> 28)
						& 1) + ((i >>> 27) & 1) + ((i >>> 26) & 1) + ((i >>> 25) & 1) + ((i >>> 24) & 1)
						+ StringHelper.SPACE + ((i >>> 23) & 1) + ((i >>> 22) & 1) + ((i >>> 21) & 1) + ((i >>> 20) & 1)
						+ ((i >>> 19) & 1) + ((i >>> 18) & 1) + ((i >>> 17) & 1) + ((i >>> 16) & 1) + StringHelper.SPACE
						+ ((i >>> 15) & 1) + ((i >>> 14) & 1) + ((i >>> 13) & 1) + ((i >>> 12) & 1) + ((i >>> 11) & 1)
						+ ((i >>> 10) & 1) + ((i >>> 9) & 1) + ((i >>> 8) & 1) + StringHelper.SPACE + ((i >>> 7) & 1)
						+ ((i >>> 6) & 1) + ((i >>> 5) & 1) + ((i >>> 4) & 1) + ((i >>> 3) & 1) + ((i >>> 2) & 1) + (
						(i >>> 1) & 1) + ((i) & 1);

		return sb;
	}
}
