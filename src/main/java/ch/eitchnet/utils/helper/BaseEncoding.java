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
 * This class implements the encoding part of RFC 4648 <a>https://tools.ietf.org/html/rfc4648</a>. For the decoding see
 * {@link BaseDecoding}.
 * </p>
 * 
 * <p>
 * The following implementations are supported:
 * <ul>
 * <li>Base64</li>
 * <li>Base64 URL safe</li>
 * <li>Base32</li>
 * <li>Base32 HEX</li>
 * <li>Base16 / HEX</li>
 * </ul>
 * </p>
 * 
 * @author Robert von Burg <eitch@eitchnet.ch>
 */
public class BaseEncoding {

	// private static final Logger logger = LoggerFactory.getLogger(BaseEncoding.class);

	private static final byte PAD = '=';

	private static final byte[] BASE_16 = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E',
			'F' };

	private static final byte[] BASE_32 = { 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
			'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '2', '3', '4', '5', '6', '7' };

	private static final byte[] BASE_32_HEX = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D',
			'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V' };

	private static final byte[] BASE_32_DMEDIA = { '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
			'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y' };

	private static final byte[] BASE_32_CROCKFORD = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C',
			'D', 'E', 'F', 'G', 'H', 'J', 'K', 'M', 'N', 'P', 'Q', 'R', 'S', 'T', 'V', 'W', 'X', 'Y', 'Z' };

	private static final byte[] BASE_64 = { 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
			'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
			'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4',
			'5', '6', '7', '8', '9', '+', '/' };

	private static final byte[] BASE_64_SAFE = { 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
			'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
			'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3',
			'4', '5', '6', '7', '8', '9', '-', '_' };

	public static byte[] toBase64(byte[] bytes) {
		return toBase64(BASE_64, bytes);
	}

	public static byte[] toBase64Safe(byte[] bytes) {
		return toBase64(BASE_64_SAFE, bytes);
	}

	public static byte[] toBase32(byte[] bytes) {
		return toBase32(BASE_32, bytes);
	}

	public static byte[] toBase32Hex(byte[] bytes) {
		return toBase32(BASE_32_HEX, bytes);
	}

	public static byte[] toBase32Dmedia(byte[] bytes) {
		return toBase32(BASE_32_DMEDIA, bytes);
	}

	public static byte[] toBase32Crockford(byte[] bytes) {
		return toBase32(BASE_32_CROCKFORD, bytes);
	}

	public static byte[] toBase16(byte[] bytes) {
		return toBase16(bytes, BASE_16);
	}

	/**
	 * Encodes the given data to a 64-bit alphabet encoding. Thus the passed alphabet can be any arbitrary alphabet
	 * 
	 * @param bytes
	 *            the bytes to encode
	 * @param alphabet
	 *            the 64-bit alphabet to use
	 * 
	 * @return the encoded data
	 */
	private static byte[] toBase64(byte[] alphabet, byte[] bytes) {
		if (bytes.length == 0) {
			return new byte[0];
		}
		if (alphabet.length != 64) {
			throw new RuntimeException("Alphabet does not have expected size 64 but is " + alphabet.length);
		}

		// 6 bits input for every 8 bits (1 byte) output
		// least common multiple of 6 bits input and 8 bits output = 24
		// and output multiple is then lcm(6, 8) / 6 = 4
		// thus we need to write multiples of 4 bytes of data
		int bitsIn = 6;
		int outputMultiple = 4;

		// first convert to bits
		int nrOfInputBytes = bytes.length;
		int nrOfInputBits = nrOfInputBytes * Byte.SIZE;

		// calculate number of bits missing for multiples of bitsIn
		int inputPadding = nrOfInputBits % bitsIn;
		int nrOfOutputBytes;
		if (inputPadding == 0)
			nrOfOutputBytes = nrOfInputBits / bitsIn;
		else
			nrOfOutputBytes = (nrOfInputBits + (bitsIn - (inputPadding))) / bitsIn;

		// calculate number of bits missing for multiple of bitsOut
		int nrOfBytesPadding = outputMultiple - (nrOfOutputBytes % outputMultiple);
		if (nrOfBytesPadding == outputMultiple)
			nrOfBytesPadding = 0;

		// actual result array is multiples of bitsOut/8 thus sum of:
		int txtLength = nrOfOutputBytes + nrOfBytesPadding;

//		logger.info(String.format("Input: %d bytes, Output: %d bytes, Padding: %d bytes, TextLength: %d",
//				nrOfInputBytes, nrOfOutputBytes, nrOfBytesPadding, txtLength));

		byte[] txt = new byte[txtLength];
		long bits;
		int bytesPos = 0;
		int txtPos = 0;
		while (bytesPos < bytes.length) {

			int remaining = bytes.length - bytesPos;
			// get up to 24 bits of data in 3 bytes
			bits = 0;
			if (remaining >= 3) {

				bits = ((long) (bytes[bytesPos++] & 0xff) << 16) //
						| ((long) (bytes[bytesPos++] & 0xff) << 8) // 
						| ((bytes[bytesPos++] & 0xff));

			} else if (remaining == 2) {

				bits = ((long) (bytes[bytesPos++] & 0xff) << 16) //
						| ((long) (bytes[bytesPos++] & 0xff) << 8);

			} else if (remaining == 1) {

				bits = ((long) (bytes[bytesPos++] & 0xff) << 16);
			}

			// always start at 24. bit
			int bitPos = 23;

			// always write 24 bits (6 bytes * 4 multiples), but this will also write into the padding
			// we will fix this by writing the padding as has been calculated previously
			while (bitPos >= 0) {

				int index = 0;
				index |= ((bits >>> bitPos) & 1) == 0 ? 0 : 32;
				bitPos--;
				index |= ((bits >>> bitPos) & 1) == 0 ? 0 : 16;
				bitPos--;
				index |= ((bits >>> bitPos) & 1) == 0 ? 0 : 8;
				bitPos--;
				index |= ((bits >>> bitPos) & 1) == 0 ? 0 : 4;
				bitPos--;
				index |= ((bits >>> bitPos) & 1) == 0 ? 0 : 2;
				bitPos--;
				index |= ((bits >>> bitPos) & 1) == 0 ? 0 : 1;
				bitPos--;
				byte character = alphabet[index];
				txt[txtPos] = character;
				txtPos++;
			}
		}

		// write any padding that was calculated
		if (nrOfBytesPadding != 0) {
			int paddingPos = txtPos - nrOfBytesPadding;
			for (; paddingPos < txtLength; paddingPos++) {
				txt[paddingPos] = PAD;
			}
		}

		return txt;
	}

	/**
	 * Encodes the given data to a 32-bit alphabet encoding. Thus the passed alphabet can be any arbitrary alphabet
	 * 
	 * @param bytes
	 *            the bytes to encode
	 * @param alphabet
	 *            the 32-bit alphabet to use
	 * 
	 * @return the encoded data
	 */
	public static byte[] toBase32(byte[] alphabet, byte[] bytes) {
		if (bytes.length == 0) {
			return new byte[0];
		}
		if (alphabet.length != 32) {
			throw new RuntimeException("Alphabet does not have expected size 32 but is " + alphabet.length);
		}

		// 5 bits input for every 8 bits (1 byte) output
		// least common multiple of 5 bits input and 8 bits output = 40
		// and output multiple is then lcm(5, 8) / 5 = 8
		// thus we need to write multiples of 8 bytes of data
		int bitsIn = 5;
		int outputMultiple = 8;

		// first convert to bits
		int nrOfInputBytes = bytes.length;
		int nrOfInputBits = nrOfInputBytes * Byte.SIZE;

		// calculate number of bits missing for multiples of bitsIn
		int inputPadding = nrOfInputBits % bitsIn;
		int nrOfOutputBytes;
		if (inputPadding == 0)
			nrOfOutputBytes = nrOfInputBits / bitsIn;
		else
			nrOfOutputBytes = (nrOfInputBits + (bitsIn - (inputPadding))) / bitsIn;

		// calculate number of bits missing for multiple of bitsOut
		int nrOfBytesPadding = outputMultiple - (nrOfOutputBytes % outputMultiple);
		if (nrOfBytesPadding == outputMultiple)
			nrOfBytesPadding = 0;

		// actual result array is multiples of bitsOut/8 thus sum of:
		int txtLength = nrOfOutputBytes + nrOfBytesPadding;

//		logger.info(String.format("Input: %d bytes, Output: %d bytes, Padding: %d bytes, TextLength: %d",
//				nrOfInputBytes, nrOfOutputBytes, nrOfBytesPadding, txtLength));

		byte[] txt = new byte[txtLength];
		long bits;
		int bytesPos = 0;
		int txtPos = 0;
		while (bytesPos < bytes.length) {

			int remaining = bytes.length - bytesPos;
			// get up to 40 bits of data in 5 bytes
			bits = 0;
			if (remaining >= 5) {

				bits = ((long) (bytes[bytesPos++] & 0xff) << 32) //
						| ((long) (bytes[bytesPos++] & 0xff) << 24) //
						| ((long) (bytes[bytesPos++] & 0xff) << 16) //
						| ((long) (bytes[bytesPos++] & 0xff) << 8) // 
						| ((bytes[bytesPos++] & 0xff));

			} else if (remaining == 4) {

				bits = ((long) (bytes[bytesPos++] & 0xff) << 32) //
						| ((long) (bytes[bytesPos++] & 0xff) << 24) //
						| ((long) (bytes[bytesPos++] & 0xff) << 16) //
						| ((long) (bytes[bytesPos++] & 0xff) << 8);

			} else if (remaining == 3) {

				bits = ((long) (bytes[bytesPos++] & 0xff) << 32) //
						| ((long) (bytes[bytesPos++] & 0xff) << 24) //
						| ((long) (bytes[bytesPos++] & 0xff) << 16);

			} else if (remaining == 2) {

				bits = ((long) (bytes[bytesPos++] & 0xff) << 32) //
						| ((long) (bytes[bytesPos++] & 0xff) << 24);

			} else if (remaining == 1) {

				bits = ((long) (bytes[bytesPos++] & 0xff) << 32);

			}

			// always start at 40. bit
			int bitPos = 39;

			// always write 40 bits (5 bytes * 8 multiples), but this will also write into the padding
			// we will fix this by writing the padding as has been calculated previously
			while (bitPos >= 0) {

				int index = 0;
				index |= ((bits >>> bitPos) & 1) == 0 ? 0 : 16;
				bitPos--;
				index |= ((bits >>> bitPos) & 1) == 0 ? 0 : 8;
				bitPos--;
				index |= ((bits >>> bitPos) & 1) == 0 ? 0 : 4;
				bitPos--;
				index |= ((bits >>> bitPos) & 1) == 0 ? 0 : 2;
				bitPos--;
				index |= ((bits >>> bitPos) & 1) == 0 ? 0 : 1;
				bitPos--;
				byte character = alphabet[index];
				txt[txtPos] = character;
				txtPos++;
			}
		}

		// write any padding that was calculated
		if (nrOfBytesPadding != 0) {
			int paddingPos = txtPos - nrOfBytesPadding;
			for (; paddingPos < txtLength; paddingPos++) {
				txt[paddingPos] = PAD;
			}
		}

		return txt;
	}

	/**
	 * Encodes the given data to a 16-bit alphabet encoding. Thus the passed alphabet can be any arbitrary alphabet
	 * 
	 * @param bytes
	 *            the bytes to encode
	 * @param alphabet
	 *            the 16-bit alphabet to use
	 * 
	 * @return the encoded data
	 */
	public static byte[] toBase16(byte[] bytes, byte[] alphabet) {
		if (bytes.length == 0) {
			return new byte[0];
		}
		if (alphabet.length != 32) {
			throw new RuntimeException("Alphabet does not have expected size 32 but is " + alphabet.length);
		}

		// calculate output text length
		int nrOfInputBytes = bytes.length;
		int nrOfOutputBytes = nrOfInputBytes * 2;
		int txtLength = nrOfOutputBytes;

//		logger.info(String.format("Input: %d bytes, Output: %d bytes, TextLength: %d", nrOfInputBytes, nrOfOutputBytes,
//				txtLength));

		byte[] txt = new byte[txtLength];
		byte bits;
		int bytesPos = 0;
		int txtPos = 0;
		while (bytesPos < bytes.length) {

			// get 8 bits of data (1 byte)
			bits = bytes[bytesPos++];

			// now write the 8 bits as 2 * 4 bits

			// output byte 1
			int index = (bits >>> 4) & 0xf;
			byte character = alphabet[index];
			txt[txtPos] = character;
			txtPos++;

			// output byte 2
			index = bits & 0xf;
			character = alphabet[index];
			txt[txtPos] = character;
			txtPos++;
		}

		return txt;
	}
}