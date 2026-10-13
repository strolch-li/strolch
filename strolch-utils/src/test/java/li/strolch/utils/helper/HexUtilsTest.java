/*
 * Copyright (c) 2013-2025 Robert von Burg <eitch@eitchnet.ch>
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

import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

public class HexUtilsTest {

	@Test
	public void testBytesToHex_Empty() {
		assertEquals("", HexUtils.bytesToHex(new byte[0]));
	}

	@Test
	public void testBytesToHex_SingleByte() {
		assertEquals("00", HexUtils.bytesToHex(new byte[]{0x00}));
		assertEquals("01", HexUtils.bytesToHex(new byte[]{0x01}));
		assertEquals("0a", HexUtils.bytesToHex(new byte[]{0x0A}));
		assertEquals("0f", HexUtils.bytesToHex(new byte[]{0x0F}));
		assertEquals("10", HexUtils.bytesToHex(new byte[]{0x10}));
		assertEquals("7f", HexUtils.bytesToHex(new byte[]{0x7F}));
		assertEquals("80", HexUtils.bytesToHex(new byte[]{(byte) 0x80}));
		assertEquals("ff", HexUtils.bytesToHex(new byte[]{(byte) 0xFF}));
	}

	@Test
	public void testBytesToHex_MultipleBytes() {
		byte[] data = {0x01, 0x02, 0x03, 0x04, 0x05};
		assertEquals("01 02 03 04 05", HexUtils.bytesToHex(data));
	}

	@Test
	public void testBytesToHex_EightBytes() {
		byte[] data = {1, 2, 3, 4, 5, 6, 7, 8};
		assertEquals("01 02 03 04 05 06 07 08", HexUtils.bytesToHex(data));
	}

	@Test
	public void testBytesToHex_NineBytes_IncludesDoubleSpaceAfterEighthByte() {
		byte[] data = {1, 2, 3, 4, 5, 6, 7, 8, 9};
		assertEquals("01 02 03 04 05 06 07 08  09", HexUtils.bytesToHex(data));
	}

	@Test
	public void testBytesToHex_SixteenBytes() {
		byte[] data = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};
		assertEquals("01 02 03 04 05 06 07 08  09 0a 0b 0c 0d 0e 0f 10", HexUtils.bytesToHex(data));
	}

	@Test
	public void testBytesToHex_SeventeenBytes() {
		byte[] data = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17};
		assertEquals("01 02 03 04 05 06 07 08  09 0a 0b 0c 0d 0e 0f 10  11", HexUtils.bytesToHex(data));
	}

	@Test
	public void testHexToBytes_EmptyAndWhitespace() {
		assertArrayEquals(new byte[0], HexUtils.hexToBytes(""));
		assertArrayEquals(new byte[0], HexUtils.hexToBytes("   "));
	}

	@Test
	public void testHexToBytes_ValidNoSpaces() {
		byte[] expected = {0x01, 0x02, 0x03, 0x0A, 0x0B, 0x0C, (byte) 0xFF};
		assertArrayEquals(expected, HexUtils.hexToBytes("0102030a0b0cff"));
		assertArrayEquals(expected, HexUtils.hexToBytes("0102030A0B0CFF"));
	}

	@Test
	public void testHexToBytes_ValidWithSpaces() {
		byte[] expected = {0x01, 0x02, 0x03, 0x0A, 0x0B, 0x0C, (byte) 0xFF};
		assertArrayEquals(expected, HexUtils.hexToBytes("01 02 03 0a 0b 0c ff"));
		assertArrayEquals(expected, HexUtils.hexToBytes("  01  02   03  0A  0B 0C  FF  "));
	}

	@Test
	public void testHexToBytes_OddLength_ThrowsIllegalArgumentException() {
		assertThrows(IllegalArgumentException.class, () -> HexUtils.hexToBytes("1"));
		assertThrows(IllegalArgumentException.class, () -> HexUtils.hexToBytes("123"));
		assertThrows(IllegalArgumentException.class, () -> HexUtils.hexToBytes("01 02 3"));
		assertThrows(IllegalArgumentException.class, () -> HexUtils.hexToBytes("  1 2 3  "));
	}

	@Test
	public void testHexToBytes_InvalidCharacters_ThrowsIllegalArgumentException() {
		assertThrows(IllegalArgumentException.class, () -> HexUtils.hexToBytes("0g"));
		assertThrows(IllegalArgumentException.class, () -> HexUtils.hexToBytes("zz"));
		assertThrows(IllegalArgumentException.class, () -> HexUtils.hexToBytes("01 02 0x"));
		assertThrows(IllegalArgumentException.class, () -> HexUtils.hexToBytes("01 02 -3"));
		assertThrows(IllegalArgumentException.class, () -> HexUtils.hexToBytes("01 02 ?4"));
	}

	@Test
	public void testRoundtrip_All256ByteValues() {
		for (int i = 0; i < 256; i++) {
			byte[] original = {(byte) i};
			String hex = HexUtils.bytesToHex(original);
			byte[] decoded = HexUtils.hexToBytes(hex);
			assertArrayEquals(original, decoded);
		}
	}

	@Test
	public void testRoundtrip_VariousLengths() {
		for (int len = 0; len <= 64; len++) {
			byte[] original = new byte[len];
			for (int i = 0; i < len; i++) {
				original[i] = (byte) ((i * 31 + 17) & 0xFF);
			}
			String hex = HexUtils.bytesToHex(original);
			byte[] decoded = HexUtils.hexToBytes(hex);
			assertArrayEquals(original, decoded);
		}
	}
}
