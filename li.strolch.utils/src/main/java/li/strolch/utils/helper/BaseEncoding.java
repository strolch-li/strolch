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

import java.text.MessageFormat;

/**
 * <p>
 * This class implements the encoding and decoding of RFC 4648 <a>https://tools.ietf.org/html/rfc4648</a>.
 * </p>
 *
 * <p>
 * The following implementations are supported:
 * </p>
 *
 * <ul>
 * <li>Base64</li>
 * <li>Base64 URL safe</li>
 * <li>Base32</li>
 * <li>Base32 HEX</li>
 * <li>Base16 / HEX</li>
 * </ul>
 *
 * <p>
 * As a further bonus, it is possible to use the algorithm with a client specified alphabet. In this case the client is
 * responsible for generating the alphabet for use in the decoding
 * </p>
 *
 * <p>
 * This class also implements a number of utility methods to check if given data is in a valid encoding
 * </p>
 *
 * @author Robert von Burg &lt;eitch@eitchnet.ch&gt;
 */
public class BaseEncoding {

	// private static final Logger logger = LoggerFactory.getLogger(BaseEncoding.class);

	private static final int PADDING_64 = 2;
	private static final int PADDING_32 = 6;

	public static final byte PAD = '=';

	static final byte[] BASE_16 = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

	static final byte[] BASE_32 = { 'A',
			'B',
			'C',
			'D',
			'E',
			'F',
			'G',
			'H',
			'I',
			'J',
			'K',
			'L',
			'M',
			'N',
			'O',
			'P',
			'Q',
			'R',
			'S',
			'T',
			'U',
			'V',
			'W',
			'X',
			'Y',
			'Z',
			'2',
			'3',
			'4',
			'5',
			'6',
			'7' };

	static final byte[] BASE_32_HEX = { '0',
			'1',
			'2',
			'3',
			'4',
			'5',
			'6',
			'7',
			'8',
			'9',
			'A',
			'B',
			'C',
			'D',
			'E',
			'F',
			'G',
			'H',
			'I',
			'J',
			'K',
			'L',
			'M',
			'N',
			'O',
			'P',
			'Q',
			'R',
			'S',
			'T',
			'U',
			'V' };

	static final byte[] BASE_32_DMEDIA = { '3',
			'4',
			'5',
			'6',
			'7',
			'8',
			'9',
			'A',
			'B',
			'C',
			'D',
			'E',
			'F',
			'G',
			'H',
			'I',
			'J',
			'K',
			'L',
			'M',
			'N',
			'O',
			'P',
			'Q',
			'R',
			'S',
			'T',
			'U',
			'V',
			'W',
			'X',
			'Y' };

	static final byte[] BASE_32_CROCKFORD = { '0',
			'1',
			'2',
			'3',
			'4',
			'5',
			'6',
			'7',
			'8',
			'9',
			'A',
			'B',
			'C',
			'D',
			'E',
			'F',
			'G',
			'H',
			'J',
			'K',
			'M',
			'N',
			'P',
			'Q',
			'R',
			'S',
			'T',
			'V',
			'W',
			'X',
			'Y',
			'Z' };

	static final byte[] BASE_64 = { 'A',
			'B',
			'C',
			'D',
			'E',
			'F',
			'G',
			'H',
			'I',
			'J',
			'K',
			'L',
			'M',
			'N',
			'O',
			'P',
			'Q',
			'R',
			'S',
			'T',
			'U',
			'V',
			'W',
			'X',
			'Y',
			'Z',
			'a',
			'b',
			'c',
			'd',
			'e',
			'f',
			'g',
			'h',
			'i',
			'j',
			'k',
			'l',
			'm',
			'n',
			'o',
			'p',
			'q',
			'r',
			's',
			't',
			'u',
			'v',
			'w',
			'x',
			'y',
			'z',
			'0',
			'1',
			'2',
			'3',
			'4',
			'5',
			'6',
			'7',
			'8',
			'9',
			'+',
			'/' };

	static final byte[] BASE_64_SAFE = { 'A',
			'B',
			'C',
			'D',
			'E',
			'F',
			'G',
			'H',
			'I',
			'J',
			'K',
			'L',
			'M',
			'N',
			'O',
			'P',
			'Q',
			'R',
			'S',
			'T',
			'U',
			'V',
			'W',
			'X',
			'Y',
			'Z',
			'a',
			'b',
			'c',
			'd',
			'e',
			'f',
			'g',
			'h',
			'i',
			'j',
			'k',
			'l',
			'm',
			'n',
			'o',
			'p',
			'q',
			'r',
			's',
			't',
			'u',
			'v',
			'w',
			'x',
			'y',
			'z',
			'0',
			'1',
			'2',
			'3',
			'4',
			'5',
			'6',
			'7',
			'8',
			'9',
			'-',
			'_' };

	// these reverse base encoding alphabets were generated from the actual alphabet

	private static final byte[] REV_BASE_16 = { -1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			0,
			1,
			2,
			3,
			4,
			5,
			6,
			7,
			8,
			9,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			10,
			11,
			12,
			13,
			14,
			15,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1 };

	private static final byte[] REV_BASE_32 = { -1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			26,
			27,
			28,
			29,
			30,
			31,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			0,
			1,
			2,
			3,
			4,
			5,
			6,
			7,
			8,
			9,
			10,
			11,
			12,
			13,
			14,
			15,
			16,
			17,
			18,
			19,
			20,
			21,
			22,
			23,
			24,
			25,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1 };

	private static final byte[] REV_BASE_32_CROCKFORD = { -1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			0,
			1,
			2,
			3,
			4,
			5,
			6,
			7,
			8,
			9,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			10,
			11,
			12,
			13,
			14,
			15,
			16,
			17,
			-1,
			18,
			19,
			-1,
			20,
			21,
			-1,
			22,
			23,
			24,
			25,
			26,
			-1,
			27,
			28,
			29,
			30,
			31,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1 };

	private static final byte[] REV_BASE_32_DMEDIA = { -1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			0,
			1,
			2,
			3,
			4,
			5,
			6,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			7,
			8,
			9,
			10,
			11,
			12,
			13,
			14,
			15,
			16,
			17,
			18,
			19,
			20,
			21,
			22,
			23,
			24,
			25,
			26,
			27,
			28,
			29,
			30,
			31,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1 };

	private static final byte[] REV_BASE_32_HEX = { -1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			0,
			1,
			2,
			3,
			4,
			5,
			6,
			7,
			8,
			9,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			10,
			11,
			12,
			13,
			14,
			15,
			16,
			17,
			18,
			19,
			20,
			21,
			22,
			23,
			24,
			25,
			26,
			27,
			28,
			29,
			30,
			31,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1 };

	private static final byte[] REV_BASE_64 = { -1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			62,
			-1,
			-1,
			-1,
			63,
			52,
			53,
			54,
			55,
			56,
			57,
			58,
			59,
			60,
			61,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			0,
			1,
			2,
			3,
			4,
			5,
			6,
			7,
			8,
			9,
			10,
			11,
			12,
			13,
			14,
			15,
			16,
			17,
			18,
			19,
			20,
			21,
			22,
			23,
			24,
			25,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			26,
			27,
			28,
			29,
			30,
			31,
			32,
			33,
			34,
			35,
			36,
			37,
			38,
			39,
			40,
			41,
			42,
			43,
			44,
			45,
			46,
			47,
			48,
			49,
			50,
			51,
			-1,
			-1,
			-1,
			-1,
			-1 };

	private static final byte[] REV_BASE_64_SAFE = { -1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			62,
			-1,
			-1,
			52,
			53,
			54,
			55,
			56,
			57,
			58,
			59,
			60,
			61,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			-1,
			0,
			1,
			2,
			3,
			4,
			5,
			6,
			7,
			8,
			9,
			10,
			11,
			12,
			13,
			14,
			15,
			16,
			17,
			18,
			19,
			20,
			21,
			22,
			23,
			24,
			25,
			-1,
			-1,
			-1,
			-1,
			63,
			-1,
			26,
			27,
			28,
			29,
			30,
			31,
			32,
			33,
			34,
			35,
			36,
			37,
			38,
			39,
			40,
			41,
			42,
			43,
			44,
			45,
			46,
			47,
			48,
			49,
			50,
			51,
			-1,
			-1,
			-1,
			-1,
			-1 };

	public static byte[] toBase64(byte[] bytes) {
		return toBase64(BASE_64, bytes);
	}

	public static String toBase64(String data) {
		return toBase64(BASE_64, data);
	}

	public static byte[] toBase64Safe(byte[] bytes) {
		return toBase64(BASE_64_SAFE, bytes);
	}

	public static String toBase64Safe(String data) {
		return toBase64(BASE_64_SAFE, data);
	}

	public static String toBase64(byte[] alphabet, String data) {
		return new String(toBase64(alphabet, data.getBytes()));
	}

	public static byte[] toBase32(byte[] bytes) {
		return toBase32(BASE_32, bytes);
	}

	public static String toBase32(String data) {
		return toBase32(BASE_32, data);
	}

	public static byte[] toBase32Hex(byte[] bytes) {
		return toBase32(BASE_32_HEX, bytes);
	}

	public static String toBase32Hex(String data) {
		return toBase32(BASE_32_HEX, data);
	}

	public static byte[] toBase32Dmedia(byte[] bytes) {
		return toBase32(BASE_32_DMEDIA, bytes);
	}

	public static String toBase32Dmedia(String data) {
		return toBase32(BASE_32_DMEDIA, data);
	}

	public static byte[] toBase32Crockford(byte[] bytes) {
		return toBase32(BASE_32_CROCKFORD, bytes);
	}

	public static String toBase32Crockford(String data) {
		return toBase32(BASE_32_CROCKFORD, data);
	}

	public static String toBase32(byte[] alphabet, String data) {
		return new String(toBase32(alphabet, data.getBytes()));
	}

	public static byte[] toBase16(byte[] bytes) {
		return toBase16(BASE_16, bytes);
	}

	public static String toBase16(String data) {
		return toBase16(BASE_16, data);
	}

	public static String toBase16(byte[] alphabet, String data) {
		return new String(toBase16(alphabet, data.getBytes()));
	}

	public static byte[] fromBase64(byte[] bytes) {
		return fromBase64(REV_BASE_64, bytes);
	}

	public static String fromBase64(String data) {
		return fromBase64(REV_BASE_64, data);
	}

	public static String fromBase64Safe(String data) {
		return fromBase64(REV_BASE_64_SAFE, data);
	}

	public static String fromBase64(byte[] alphabet, String data) {
		return new String(fromBase64(alphabet, data.getBytes()));
	}

	public static byte[] fromBase32(byte[] bytes) {
		return fromBase32(REV_BASE_32, bytes);
	}

	public static String fromBase32(String data) {
		return fromBase32(REV_BASE_32, data);
	}

	public static byte[] fromBase32Hex(byte[] bytes) {
		return fromBase32(REV_BASE_32_HEX, bytes);
	}

	public static String fromBase32Hex(String data) {
		return fromBase32(REV_BASE_32_HEX, data);
	}

	public static byte[] fromBase32Dmedia(byte[] bytes) {
		return fromBase32(REV_BASE_32_DMEDIA, bytes);
	}

	public static String fromBase32Dmedia(String data) {
		return fromBase32(REV_BASE_32_DMEDIA, data);
	}

	public static byte[] fromBase32Crockford(byte[] bytes) {
		return fromBase32(REV_BASE_32_CROCKFORD, bytes);
	}

	public static String fromBase32Crockford(String data) {
		return fromBase32(REV_BASE_32_CROCKFORD, data);
	}

	public static String fromBase32(byte[] alphabet, String data) {
		return new String(fromBase32(alphabet, data.getBytes()));
	}

	public static byte[] fromBase16(byte[] bytes) {
		return fromBase16(REV_BASE_16, bytes);
	}

	public static String fromBase16(String data) {
		return fromBase16(REV_BASE_16, data);
	}

	public static String fromBase16(byte[] alphabet, String data) {
		return new String(fromBase16(alphabet, data.getBytes()));
	}

	public static boolean isBase64(byte[] bytes) {
		return isEncodedByAlphabet(REV_BASE_64, bytes, PADDING_64);
	}

	public static boolean isBase64(String data) {
		return isEncodedByAlphabet(REV_BASE_64, data, PADDING_64);
	}

	public static boolean isBase64Safe(byte[] bytes) {
		return isEncodedByAlphabet(REV_BASE_64_SAFE, bytes, PADDING_64);
	}

	public static boolean isBase64Safe(String data) {
		return isEncodedByAlphabet(REV_BASE_64_SAFE, data, PADDING_64);
	}

	public static boolean isBase32(byte[] bytes) {
		return isEncodedByAlphabet(REV_BASE_32, bytes, PADDING_32);
	}

	public static boolean isBase32(String data) {
		return isEncodedByAlphabet(REV_BASE_32, data, PADDING_32);
	}

	public static boolean isBase32Hex(byte[] bytes) {
		return isEncodedByAlphabet(REV_BASE_32_HEX, bytes, PADDING_32);
	}

	public static boolean isBase32Hex(String data) {
		return isEncodedByAlphabet(REV_BASE_32_HEX, data, PADDING_32);
	}

	public static boolean isBase32Crockford(byte[] bytes) {
		return isEncodedByAlphabet(REV_BASE_32_CROCKFORD, bytes, PADDING_32);
	}

	public static boolean isBase32Crockford(String data) {
		return isEncodedByAlphabet(REV_BASE_32_CROCKFORD, data, PADDING_32);
	}

	public static boolean isBase32Dmedia(byte[] bytes) {
		return isEncodedByAlphabet(REV_BASE_32_DMEDIA, bytes, PADDING_32);
	}

	public static boolean isBase32Dmedia(String data) {
		return isEncodedByAlphabet(REV_BASE_32_DMEDIA, data, PADDING_32);
	}

	public static boolean isBase16(byte[] bytes) {
		return isEncodedByAlphabet(REV_BASE_16, bytes, 0);
	}

	public static boolean isBase16(String data) {
		return isEncodedByAlphabet(REV_BASE_16, data, 0);
	}

	public static boolean isEncodedByAlphabet(byte[] alphabet, String data, int padding) {
		return isEncodedByAlphabet(alphabet, data.getBytes(), padding);
	}

	/**
	 * @param alphabet
	 * 		the alphabet to use
	 * @param bytes
	 * 		the check if encoded
	 * @param maxPadding
	 * 		max padding allowed
	 *
	 * @return true if encoded by alphabet
	 */
	public static boolean isEncodedByAlphabet(byte[] alphabet, byte[] bytes, int maxPadding) {
		if (bytes.length == 0)
			return true;

		int paddingStart = 0;
		for (int i = 0; i < bytes.length; i++) {
			byte b = bytes[i];
			if (b < 0 || b > alphabet.length)
				return false;

			byte c = alphabet[b];
			if (c == -1) {

				if (b == PAD && maxPadding != 0) {
					if (paddingStart == 0)
						paddingStart = i;

					continue;
				}

				return false;
			}
		}

		if (paddingStart != 0 && paddingStart < (bytes.length - maxPadding))
			return false;

		return true;
	}

	/**
	 * Encodes the given data to a 64-bit alphabet encoding. Thus the passed alphabet can be any arbitrary alphabet
	 *
	 * @param alphabet
	 * 		the 64-bit alphabet to use
	 * @param bytes
	 * 		the bytes to encode
	 *
	 * @return the encoded data
	 */
	public static byte[] toBase64(byte[] alphabet, byte[] bytes) {
		if (bytes.length == 0)
			return new byte[0];
		if (alphabet.length != 64) {
			String msg = MessageFormat
					.format("Alphabet does not have expected size 64 but is {0}", alphabet.length); //$NON-NLS-1$
			throw new RuntimeException(msg);
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

			// always write 24 bits (6 bits * 4), but this will also write into the padding
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
	 * @param alphabet
	 * 		the 32-bit alphabet to use
	 * @param bytes
	 * 		the bytes to encode
	 *
	 * @return the encoded data
	 */
	public static byte[] toBase32(byte[] alphabet, byte[] bytes) {
		if (bytes.length == 0)
			return new byte[0];
		if (alphabet.length != 32) {
			String msg = MessageFormat
					.format("Alphabet does not have expected size 32 but is {0}", alphabet.length); //$NON-NLS-1$
			throw new RuntimeException(msg);
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
	 * @param alphabet
	 * 		the 16-bit alphabet to use
	 * @param bytes
	 * 		the bytes to encode
	 *
	 * @return the encoded data
	 */
	public static byte[] toBase16(byte[] alphabet, byte[] bytes) {
		if (bytes.length == 0)
			return new byte[0];
		if (alphabet.length != 16) {
			String msg = MessageFormat
					.format("Alphabet does not have expected size 16 but is {0}", alphabet.length); //$NON-NLS-1$
			throw new RuntimeException(msg);
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

	/**
	 * Decodes the given Base64 encoded data to the original data set
	 *
	 * @param alphabet
	 * 		the 64-bit alphabet to use
	 * @param bytes
	 * 		the bytes to decode
	 *
	 * @return the decoded data
	 */
	public static byte[] fromBase64(byte[] alphabet, byte[] bytes) {
		int inputLength = bytes.length;
		if (inputLength == 0)
			return new byte[0];
		if ((inputLength % 4) != 0) {
			String msg = MessageFormat
					.format("The input bytes to be decoded must be multiples of 4, but is multiple of {0}",//$NON-NLS-1$
							(inputLength % 4));
			throw new RuntimeException(msg);
		}

		if (alphabet.length != 128) {
			String msg = MessageFormat
					.format("Alphabet does not have expected size 128 but is {0}", alphabet.length); //$NON-NLS-1$
			throw new RuntimeException(msg);
		}

		if (!isEncodedByAlphabet(alphabet, bytes, PADDING_64))
			throw new RuntimeException(
					"The data contains illegal values which are not mapped by the given alphabet!"); //$NON-NLS-1$

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

	/**
	 * Decodes the given Base32 encoded data to the original data set
	 *
	 * @param alphabet
	 * 		the 32-bit alphabet to use
	 * @param bytes
	 * 		the bytes to decode
	 *
	 * @return the decoded data
	 */
	public static byte[] fromBase32(byte[] alphabet, byte[] bytes) {
		int inputLength = bytes.length;
		if (inputLength == 0)
			return new byte[0];
		if ((inputLength % 8) != 0) {
			String msg = "The input bytes to be decoded must be multiples of 8, but is multiple of {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, (inputLength % 8));
			throw new RuntimeException(msg);
		}

		if (alphabet.length != 128) {
			String msg = MessageFormat
					.format("Alphabet does not have expected size 128 but is {0}", alphabet.length); //$NON-NLS-1$
			throw new RuntimeException(msg);
		}

		if (!isEncodedByAlphabet(alphabet, bytes, PADDING_32))
			throw new RuntimeException(
					"The data contains illegal values which are not mapped by the given alphabet!"); //$NON-NLS-1$

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
	 * 		the 16-bit alphabet to use
	 * @param bytes
	 * 		the bytes to decode
	 *
	 * @return the decoded data
	 */
	public static byte[] fromBase16(byte[] alphabet, byte[] bytes) {
		if (bytes.length == 0)
			return new byte[0];
		if ((bytes.length % 2) != 0) {
			String msg = "The input bytes to be decoded must be multiples of 4, but is multiple of {0}"; //$NON-NLS-1$
			msg = MessageFormat.format(msg, (bytes.length % 4));
			throw new RuntimeException(msg);
		}

		if (alphabet.length != 128) {
			String msg = MessageFormat
					.format("Alphabet does not have expected size 128 but is {0}", alphabet.length); //$NON-NLS-1$
			throw new RuntimeException(msg);
		}

		if (!isEncodedByAlphabet(alphabet, bytes, 0))
			throw new RuntimeException(
					"The data contains illegal values which are not mapped by the given alphabet!"); //$NON-NLS-1$

		int dataLength = bytes.length / 2;

		byte[] data = new byte[dataLength];
		for (int i = 0; i < bytes.length; ) {

			byte b1 = bytes[i++];
			byte b2 = bytes[i++];

			String msgOutOfRange = "Value at index {0} is not in range of alphabet (0-127){1}"; //$NON-NLS-1$
			if (b1 < 0) {
				msgOutOfRange = MessageFormat.format(msgOutOfRange, (i - 2), b1);
				throw new IllegalArgumentException(msgOutOfRange);
			}
			if (b2 < 0) {
				msgOutOfRange = MessageFormat.format(msgOutOfRange, (i - 1), b2);
				throw new IllegalArgumentException(msgOutOfRange);
			}

			byte c1 = alphabet[b1];
			byte c2 = alphabet[b2];

			String msgIllegalValue = "Value at index {0} is referencing illegal value in alphabet: {1}"; //$NON-NLS-1$
			if (c1 == -1) {
				msgIllegalValue = MessageFormat.format(msgIllegalValue, (i - 2), b1);
				throw new IllegalArgumentException(msgIllegalValue);
			}
			if (c2 == -1) {
				msgIllegalValue = MessageFormat.format(msgIllegalValue, (i - 2), b2);
				throw new IllegalArgumentException(msgIllegalValue);
			}

			int dataIndex = (i / 2) - 1;
			int value = ((c1 << 4) & 0xff) | c2;
			data[dataIndex] = (byte) value;
		}

		return data;
	}
}