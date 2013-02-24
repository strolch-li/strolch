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
 * <p>
 * This class implements the decoding part of RFC 4648 <a>https://tools.ietf.org/html/rfc4648</a>. For the encoding see
 * {@link BaseEncoding}
 * </p>
 * 
 * <p>
 * All versions are implemented: Base64 with URL and file name safe encoding, Base32 with HEX and Base16
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class BaseDecoding {

	// private static final Logger logger = LoggerFactory.getLogger(BaseDecoding.class);

	private static final byte PAD = '=';

	// these reverse base encoding alphabets were generated from the actual alphabet

	private static final byte[] REV_BASE_16 = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -1, -1, -1, -1, -1, -1, 10, 11, 12, 13, 14, 15, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
	private static final byte[] REV_BASE_32 = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, 26, 27, 28, 29, 30, 31, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8,
			9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
	private static final byte[] REV_BASE_32_CROCKFORD = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -1, -1, -1, -1, -1, -1, 10, 11, 12, 13, 14, 15,
			16, 17, -1, 18, 19, -1, 20, 21, -1, 22, 23, 24, 25, 26, -1, 27, 28, 29, 30, 31, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1 };
	private static final byte[] REV_BASE_32_DMEDIA = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, -1, -1, -1, -1, -1, -1, -1, 7, 8, 9, 10, 11, 12, 13,
			14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1 };
	private static final byte[] REV_BASE_32_HEX = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -1, -1, -1, -1, -1, -1, 10, 11, 12, 13, 14, 15, 16, 17,
			18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1 };
	private static final byte[] REV_BASE_64 = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1,
			-1, -1, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8,
			9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1, -1, 26, 27, 28, 29,
			30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1 };
	private static final byte[] REV_BASE_64_SAFE = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
			-1, -1, 62, -1, -1, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5,
			6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, 63, -1, 26, 27,
			28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1,
			-1, -1 };

	public static byte[] fromBase64(byte[] bytes) {
		return fromBase64(REV_BASE_64, bytes);
	}

	public static byte[] fromBase64Safe(byte[] bytes) {
		return fromBase64(REV_BASE_64_SAFE, bytes);
	}

	public static byte[] fromBase32(byte[] bytes) {
		return fromBase32(REV_BASE_32, bytes);
	}

	public static byte[] fromBase32Hex(byte[] bytes) {
		return fromBase32(REV_BASE_32_HEX, bytes);
	}

	public static byte[] fromBase32Dmedia(byte[] bytes) {
		return fromBase32(REV_BASE_32_DMEDIA, bytes);
	}

	public static byte[] fromBase32Crockford(byte[] bytes) {
		return fromBase32(REV_BASE_32_CROCKFORD, bytes);
	}

	public static byte[] fromBase16(byte[] bytes) {
		return fromBase16(REV_BASE_16, bytes);
	}

	/**
	 * Decodes the given Base64 encoded data to the original data set
	 * 
	 * @param alphabet
	 *            the 64-bit alphabet to use
	 * @param bytes
	 *            the bytes to decode
	 * 
	 * @return the decoded data
	 */
	public static byte[] fromBase64(byte[] alphabet, byte[] bytes) {
		int inputLength = bytes.length;
		if (inputLength == 0)
			return new byte[0];
		if ((inputLength % 4) != 0) {
			throw new RuntimeException("The input bytes to be decoded must be multiples of 4, but is multiple of "
					+ (inputLength % 4));
		}

		if (alphabet.length != 128)
			throw new RuntimeException("Alphabet does not have expected size 128 but is " + alphabet.length);

		// find how much padding we have
		int nrOfBytesPadding = 0;
		if (bytes[inputLength - 1] == PAD) {
			int end = inputLength - 1;
			while (bytes[end] == PAD)
				end--;
			if (end != inputLength - 1)
				nrOfBytesPadding = inputLength - 1 - end;
		}

		int inputDataLength = inputLength - nrOfBytesPadding;
		int dataLengthBits = inputDataLength * 6; // 6 bits data for every 8 bits inputs
		// multiples of 6 required
		// truncating is no problem due to the input having padding to have multiples of 32 bits
		dataLengthBits = dataLengthBits - (dataLengthBits % 8);
		int dataLengthBytes = dataLengthBits / 8;

		// f   => Zg==
		// fo  => Zm8=
		// foo => Zm9v

		// we want to write as much as 24 bits in multiples of 6.
		// these multiples of 6 are read from multiples of 8
		// i.e. we discard 2 bits from every 8 bits input
		// thus we need to read 24 / 6 = 4 bytes

		byte[] data = new byte[dataLengthBytes];
		int dataPos = 0;

		// but we simply ignore the padding
		int bytesPos = 0;
		while (bytesPos < inputDataLength) {
			int remaining = inputDataLength - bytesPos;

			long bits;
			if (remaining >= 4) {

				// XXX check each byte value so that it is legal

				bits = ((long) (alphabet[bytes[bytesPos++]] & 63) << 18) //
						| ((long) (alphabet[bytes[bytesPos++]] & 63) << 12) //
						| ((long) (alphabet[bytes[bytesPos++]] & 63) << 6) //
						| (alphabet[bytes[bytesPos++]] & 63);

			} else if (remaining == 3) {

				bits = ((long) (alphabet[bytes[bytesPos++]] & 63) << 18) //
						| ((long) (alphabet[bytes[bytesPos++]] & 63) << 12) //
						| ((long) (alphabet[bytes[bytesPos++]] & 63) << 6);

			} else if (remaining == 2) {

				bits = ((long) (alphabet[bytes[bytesPos++]] & 63) << 18) //
						| ((long) (alphabet[bytes[bytesPos++]] & 63) << 12);
//
//				long b;
//				byte a;
//				a = bytes[0];
//				logger.info("1 a: " + a + " " + ((char) a) + " - " + ByteHelper.asBinary(a) + " " + indexOf(a));
//				b = (byte) (alphabet[a] & 63);
//				logger.info("1 b: " + b + " - " + ((char) b) + " - " + ByteHelper.asBinary(b));
//				a = bytes[1];
//				logger.info("2 a: " + a + " " + ((char) a) + " - " + ByteHelper.asBinary(a) + " " + indexOf(a));
//				b = (byte) (alphabet[a] & 63);
//				logger.info("2 b: " + b + " - " + ((char) b) + " - " + ByteHelper.asBinary(b));

			} else if (remaining == 1) {

				bits = ((alphabet[bytes[bytesPos++]] & 63) << 18);

			} else {

				bits = 0L;
			}

			// we can truncate to 8 bits
			int toWrite = remaining >= 4 ? 3 : remaining * 6 / 8;
			// max is always 3 bytes data from 4 bytes input

//			logger.info("toWrite: " + toWrite + ", remaining: " + remaining);
//			logger.info("bits: " + ByteHelper.asBinary(bits));

			// always start at 24. bit
			int bitPos = 23;
			// always write 24 bits (8 bits * n bytes)
			for (int i = 0; i < toWrite; i++) {

				byte value = 0;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 128;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 64;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 32;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 16;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 8;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 4;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 2;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 1;
				bitPos--;
				data[dataPos] = value;
				dataPos++;
			}
		}

		return data;
	}

//
//	/**
//	 * @param a
//	 * @return
//	 */
//	private static int indexOf(byte a) {
//		for (int i = 0; i < BaseEncoding.BASE_64.length; i++) {
//			if (BaseEncoding.BASE_64[i] == a)
//				return i;
//		}
//		return -1;
//	}
//
//	private static int indexOf1(byte a) {
//		for (int i = 0; i < REV_BASE_64.length; i++) {
//			if (REV_BASE_64[i] == a)
//				return i;
//		}
//		return -1;
//	}

	/**
	 * Decodes the given Base32 encoded data to the original data set
	 * 
	 * @param alphabet
	 *            the 32-bit alphabet to use
	 * @param bytes
	 *            the bytes to decode
	 * 
	 * @return the decoded data
	 */
	public static byte[] fromBase32(byte[] alphabet, byte[] bytes) {
		int inputLength = bytes.length;
		if (inputLength == 0)
			return new byte[0];
		if ((inputLength % 8) != 0) {
			throw new RuntimeException("The input bytes to be decoded must be multiples of 8, but is multiple of "
					+ (inputLength % 8));
		}

		if (alphabet.length != 128)
			throw new RuntimeException("Alphabet does not have expected size 128 but is " + alphabet.length);

		// find how much padding we have
		int nrOfBytesPadding = 0;
		if (bytes[inputLength - 1] == PAD) {
			int end = inputLength - 1;
			while (bytes[end] == PAD)
				end--;
			if (end != inputLength - 1)
				nrOfBytesPadding = inputLength - 1 - end;
		}

		int inputDataLength = inputLength - nrOfBytesPadding;
		int dataLengthBits = inputDataLength * 5; // 5 bits data for every 8 bits inputs
		// multiples of 8 required
		// truncating is no problem due to the input having padding to have multiples of 40 bits
		dataLengthBits = dataLengthBits - (dataLengthBits % 8);
		int dataLengthBytes = dataLengthBits / 8;

//		logger.info("Input " + inputLength + " bytes, InputData " + inputDataLength + " bytes, Padding: "
//				+ nrOfBytesPadding + " bytes, dataLength: " + dataLengthBits + " bits, dataLengthBytes: "
//				+ dataLengthBytes + " bytes");
//		logger.info(ByteHelper.asBinary(bytes));

		// we want to write as much as 40 bits in multiples of 5.
		// these multiples of 5 are read from multiples of 8
		// i.e. we discard 3 bits from every 8 bits input
		// thus we need to read 40 / 5 = 8 bytes

		byte[] data = new byte[dataLengthBytes];
		int dataPos = 0;

		// but we simply ignore the padding
		int bytesPos = 0;
		while (bytesPos < inputDataLength) {
			int remaining = inputDataLength - bytesPos;

			long bits;
			if (remaining >= 8) {

				// XXX check each byte value so that it is legal

				bits = ((long) (alphabet[bytes[bytesPos++]] & 31) << 35) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 30) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 25) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 20) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 15) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 10) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 5) //
						| (alphabet[bytes[bytesPos++]] & 31);

			} else if (remaining >= 7) {

				bits = ((long) (alphabet[bytes[bytesPos++]] & 31) << 35) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 30) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 25) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 20) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 15) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 10) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 5);

			} else if (remaining == 6) {

				bits = ((long) (alphabet[bytes[bytesPos++]] & 31) << 35) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 30) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 25) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 20) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 15) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 10);

			} else if (remaining == 5) {

				bits = ((long) (alphabet[bytes[bytesPos++]] & 31) << 35) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 30) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 25) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 20) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 15);

			} else if (remaining == 4) {

				bits = ((long) (alphabet[bytes[bytesPos++]] & 31) << 35) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 30) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 25) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 20);

			} else if (remaining == 3) {

				bits = ((long) (alphabet[bytes[bytesPos++]] & 31) << 35) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 30) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 25);

			} else if (remaining == 2) {

				bits = ((long) (alphabet[bytes[bytesPos++]] & 31) << 35) //
						| ((long) (alphabet[bytes[bytesPos++]] & 31) << 30);

			} else if (remaining == 1) {

				bits = ((alphabet[bytes[bytesPos++]] & 31) << 35);

			} else {

				bits = 0L;
			}

			// we can truncate to 8 bits
			int toRead = remaining >= 8 ? 5 : remaining * 5 / 8;
			// max is always 5 bytes data from 8 bytes input

			// always start at 40. bit
			int bitPos = 39;
			// always write 40 bits (5 bytes * 8 bits)
			for (int i = 0; i < toRead; i++) {

				byte value = 0;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 128;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 64;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 32;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 16;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 8;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 4;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 2;
				bitPos--;
				value |= ((bits >>> bitPos) & 1) == 0 ? 0 : 1;
				bitPos--;
				data[dataPos] = value;
				dataPos++;
			}
		}

		return data;
	}

	/**
	 * Decodes the given Base16 encoded data to the original data set
	 * 
	 * @param alphabet
	 *            the 16-bit alphabet to use
	 * @param bytes
	 *            the bytes to decode
	 * 
	 * @return the decoded data
	 */
	public static byte[] fromBase16(byte[] alphabet, byte[] bytes) {
		if (bytes.length == 0)
			return new byte[0];
		if ((bytes.length % 2) != 0) {
			throw new RuntimeException("The input bytes to be decoded must be multiples of 4, but is multiple of "
					+ (bytes.length % 4));
		}

		if (alphabet.length != 128)
			throw new RuntimeException("Alphabet does not have expected size 128 but is " + alphabet.length);

		int dataLength = bytes.length / 2;

		byte[] data = new byte[dataLength];
		for (int i = 0; i < bytes.length;) {

			byte b1 = bytes[i++];
			byte b2 = bytes[i++];

			if (b1 < 0) {
				throw new IllegalArgumentException("Value at index " + (i - 2) + " is not in range of alphabet (0-127)"
						+ b1);
			}
			if (b2 < 0) {
				throw new IllegalArgumentException("Value at index " + (i - 1) + " is not in range of alphabet (0-127)"
						+ b2);
			}

			byte c1 = alphabet[b1];
			byte c2 = alphabet[b2];

			if (c1 == -1) {
				throw new IllegalArgumentException("Value at index " + (i - 2)
						+ " is referencing illegal value in alphabet: " + b1);
			}
			if (c2 == -1) {
				throw new IllegalArgumentException("Value at index " + (i - 2)
						+ " is referencing illegal value in alphabet: " + b2);
			}

			int dataIndex = (i / 2) - 1;
			int value = ((c1 << 4) & 0xff) | c2;
			data[dataIndex] = (byte) value;
		}

		return data;
	}
}
